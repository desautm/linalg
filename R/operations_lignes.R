#############################################
# -- Opérations élémentaires sur les lignes
#############################################

#' Échange deux lignes d'une matrice
#' 
#' Cette opération correspond à échanger deux lignes d'une matrice
#' 
#' @param A une matrice, possiblement une matrice augmentée pour la résoultion d'un SEL ou pour trouver l'inverse d'une matrice
#' @param apartir est le numéro de la première ligne
#' @param jusqua est le numéro de la deuxième ligne
#' @return la matrice \code{A}, avec les lignes \code{apartir} et \code{jusqua} échangées
#' @export
#' @family opérations élémentaires de lignes
#' 
echange_ligne <- function(A, apartir, jusqua){
  if (!is.numeric(A) || !is.matrix(A)) stop("A doit être une matrice numérique")
  B <- A
  B[jusqua,] <- A[apartir,]
  B[apartir,] <- A[jusqua,]
  B
}

#' Additionne des multiples de deux lignes d'une matrice
#' 
#' Cette opération correspond à multiplier la ligne i par une constante C1 et 
#' à lui additionner la ligne j multipliée par une constante C2
#' 
#' @param A une matrice, possiblement une matrice augmentée pour la résolution d'un SEL ou pour trouver l'inverse d'une matrice
#' @param apartir est le numéro de la première ligne
#' @param mult1 est le multiple de la première ligne
#' @param jusqua est le numéro de la deuxième ligne
#' @param mult1 est le multiple de la deuxième ligne
#' @return la matrice \code{A}, avec la ligne \code{jusqua} obtenue par l'addition
#' @export
#' @family opérations élémentaires de lignes
#' 
mult_ligne <- function(A, apartir, mult1, jusqua, mult2){
  if (!is.numeric(A) || !is.matrix(A)) stop("A doit être une matrice numérique")
  B <- A
  B[jusqua,] <- mult1*A[apartir,]+mult2*A[jusqua,]
  B
}