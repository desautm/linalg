#' Trouve la position des pivots d'une matrice traingulaire supérieure
#'
#' Cette fonction permet de trouver la position des pivots dans une matrice
#' triangulaire supérieure. La variable \code{align} permet d'indiquer la taille de la matrice
#' initiale pour trouver les pivots.
#' 
#' @param A une matrice
#' @param align; le nombre de colonnes de A, avant d'insérer la matrice des constantes
#' @author Marc-André Désautels
#' 
#' @export
findPivot <- function(A,
                      col,
                      tol = sqrt(.Machine$double.eps)){
  row <- nrow(A)
  pivot <- matrix(0, row, 2)
  k <- 1
  
  for (i in (1:row)){
    for (j in (1:col)){
      if (abs(A[i, j]) > tol){
        pivot[k, 1] <- i
        pivot[k, 2] <- j
        k <- k + 1
        break
      }
    }
  }
  
  k <- 0
  for (i in (1:row)){
    if (pivot[i, 1]^2+pivot[i, 2]^2 > tol){
      k <- k + 1
    }
  }
  pivots <- matrix(0,k,2)
  for (i in (1:k)){
    pivots[i,] <- pivot[i,]
  }
  
  return(pivot)
}