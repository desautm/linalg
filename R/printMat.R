#' Affiche le code LaTeX de la matrice augmentée
#'
#' Cette fonction permet l'affichage du code LaTeX d'une matrice augmentée. Elle 
#' permet de choisir l'affichage entre \code{crochets} ou parenthèses.
#' Nous pouvons déterminer le nombre de chiffres après la virgule avec
#' \code{digits} et afficher sous forme de \code{fractions} ou non.
#' Le paramètre \code{sel} permet d'afficher la matrice augmentée d'un SEL ou la matrice
#' augmentée de l'inversion d'une matrice.
#'
#' @param A une matrice
#' @param sel logique; si \code{TRUE}, on affiche la matrice augmentée d'un SEL, sinon on affiche la matrice augmentée de l'inversion d'une matrice.
#' @param fractions logique; si \code{TRUE}, essaie d'exprimer les nombres non-entiers sous forme rationnelle.
#' @param crochets logique; si \code{TRUE}, affichage de la matrice entre crochets, si \code{FALSE}, affichage de la matrice entre parenthèses.
#' @param digits; permet de choisir le nombre de chiffres à droite de la virgule lors de l'affichage
#' @importFrom fractional numerators
#' @importFrom fractional denominators
#' @author Marc-André Désautels
#' @export
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3)
#' B <- matrix(c(2, 1, -1, 4,
#'              -3, -1, 2, 6,
#'              -2,  1, 2, -4,
#'              0, 4, 7, -5), 4, 4)
#'
#' printMat(A)
#' printMat(A/3, digits = 2)
#' printMat(A, crochets = FALSE)
#' printMat(A/7, fractions = TRUE)
#' printMat(B, sel = FALSE)
#' printMat(cbind(A,diag(ncol(A))), sel = FALSE)
printMat <- function(A, digits = 2, fractions = FALSE, crochets = TRUE, sel = TRUE){
  if (!sel && (ncol(A) %% 2 != 0)) stop("Le nombre de colonnes de A doit etre pair lors de l'inversion d'une matrice.")
  if (crochets)
    cat('\\left[\n')
  else
    cat('\\left(\n')
  cat('\\begin{array}{')
  if (sel){
    align <- c(rep('r',ncol(A)-1),'|r')
  }
  else{
    align <- c(rep('r',ncol(A)/2),'|',rep('r',ncol(A)/2))
  }
  cat(align, collapse = "", sep = "")
  cat('}')
  cat("\n")
  if (fractions){
    for (i in 1:nrow(A)){
      cat(dfrac(A[i,]), sep = " & ")
      cat(" \\\\")
      cat("\n")
    }
  }
  else{
    options(digits = digits)
    for (i in 1:nrow(A)){
      cat(A[i,], sep = " & ")
      cat(" \\\\")
      cat("\n")
    }
  }
  cat('\\end{array}')
  cat("\n")
  if (crochets)
    cat('\\right]\n')
  else
    cat('\\right)\n')
}

#' Affiche la fraction sous forme "\frac{}{}"
dfrac <- function(x){
  n <- fractional::numerators(x)
  d <- fractional::denominators(x)
  temp <- n
  for (i in 1 : length(n)){
    if (n[i] == 0){
      temp[i] <- 0
    }
    else{
      if (d[i] == 1){
        temp[i] <- n[i]
      }
      else{
        if (n[i] > 0){
          temp[i] <- paste(c("\\frac{",n[i],"}{",d[i],"}"), collapse = "")
        }
        else{
          temp[i] <- paste(c("-\\frac{",abs(n[i]),"}{",d[i],"}"), collapse = "")
        }
      }
    }
  }
  temp
}