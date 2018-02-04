#' @export
echelon <- function(A, 
                    B = diag(nrow(A)), 
                    tol = sqrt(.Machine$double.eps), 
                    fractions = FALSE,
                    gauss = FALSE,
                    digits = 2){
  
  # A: Matrice des coefficients
  # B: Matrice des constantes
  # tol: tolérance pour vérifier pivot à zéro
  # fractions: si TRUE, on écrit avec des \frac{}{}
  # gauss: si TRUE, fait la vraie méthode de Gauss
  # Si B est présent, on obtient la forme triangulaire supérieure
  
  if ((!is.matrix(A)) || (!is.numeric(A)))
    stop("Argument doit être une matrice numérique")
  row <- nrow(A)
  if (!missing(B)){
    if (!(nrow(B) == nrow(A)) || !is.numeric(B) || !(is.matrix(B)))
      stop("Argument doit être une matrice numérique et doit avoir le même nombre de colonnes que A")
  }
  A <- cbind(A, B)
  col <- ncol(A)
  cat("\\begin{align*}\n")
  cat("& \\phantom{\\sim} ")
  printMat(A, fractions = fractions, digits = digits)
  cat("\\\\\n")
  
  i <- 1
  j <- 1
  
  while (i <= row && j <= col){
    currentColumn <- A[, j]
    currentColumn[(1:row) <= i] <- 0
    which <- which.max(abs(currentColumn))
    pivot <- currentColumn[which]
    # Colonne avec pivot égal à zéro
    if (abs(A[i, j]) <= tol){
      # Colonne avec des zéros sous le pivot
      if (abs(pivot) <= tol){
        A[(1:row) > i, j] <- 0
        j <- j + 1
      }
      # Colonne avec des nombres différents de zéro sous le pivot
      else{
        A <- echange_ligne(A, i, which)
        cat("& \\sim ")
        printMat(A, fractions = fractions, digits = digits)
        cat("\\\\\n")
      }
    }
    # Colonne où le pivot n'est pas zéro
    else{
      # Si vraie méthode de Gauss et pivot différent de 1, on change le pivot à 1
      if (gauss && abs(A[i, j] - 1) > tol){
        A <- mult_ligne(A, i, A[i, j])
        cat("& \\sim ")
        printMat(A, fractions = fractions, digits = digits)
        cat("\\\\\n")
      }
      # La colonne possède des zéros sous le pivot
      if (abs(pivot) <= tol){
        # On ne fait rien, déjà échelonnée
      }
      # La colonne possède d'autres nombres que zéro sous le pivot
      else{
        # On doit faire des opérations de ligne
        k <- i + 1
        while (k <= row){
          # Si vraie méthode de Gauss, le pivot est égal à 1
          if (gauss){
            A <- oper_ligne(A, i, A[k,j], k, -1)
          }
          # Sinon on doit trouver le plus petit commun multiple
          else{
            lowestCM <- numbers::LCM(A[i, j], A[k, j])
            # Si le plus petit commun multiple n'est pas zéro
            # On fait des opérations sur les lignes
            if (abs(lowestCM) > tol){
              A <- oper_ligne(A, i, lowestCM/A[i, j], k, -lowestCM/A[k, j])
            }
          }
          k <- k + 1
        }
        cat("& \\sim ")
        printMat(A, fractions = fractions, digits = digits)
        cat("\\\\\n")
      }
      i <- i + 1
      j <- j + 1
    }
  }
  
  cat("\\end{align*}\n")
  
  if (missing(B)){
    return(A[, (col/2+1):col])
  }
  else{
    return(A)
  }
}