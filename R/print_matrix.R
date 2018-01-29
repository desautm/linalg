#' Affiche le code LaTeX de la matrice
#'
#' Cette fonction permet l'affichage du code LaTeX d'une matrice. Elle 
#' permet de choisir l'affichage entre \code{crochets} ou parenthèses.
#' Nous pouvons déterminer le nombre de chiffres après la virgule avec
#' \code{digits} et d'afficher sous forme de \code{fractions} ou non.
#'
#' @param A une matrice
#' @param fractions logique; si \code{TRUE}, essaie d'exprimer les nombres non-entiers sous forme rationnelle.
#' @param crochets logique; si \code{TRUE}, affichage de la matrice entre crochets, si \code{FALSE}, affichage de la matrice entre parenthèses.
#' @param digits; permet de choisir le nombre de chiffres à droite de la virgule lors de l'affichage
#' @param ... arguments additionnels que nous envoyons à \code{xtable::xtableMatharray()}
#' @importFrom xtable xtableMatharray
#' @importFrom MASS fractions
#' @author Marc-André Désautels
#' @export
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3)
#' B <- matrix(c(8/3, -11/2, -3), 3, 1)
#'
#' print_matrix(A)
#' print_matrix(A, digits = 2)
#' print_matrix(A, crochets = FALSE)
#' print_matrix(A/7, fractions = TRUE)
#' print_matrix(B, fractions = TRUE)
#'
print_matrix <- function(A, crochets = TRUE, fractions = FALSE, digits = 0, ...){
  if (fractions)
    ret <- xtable::xtableMatharray(as.character(MASS::fractions(A)), digits = digits, ...)
  else
    ret <- xtable::xtableMatharray(A, digits = digits, ...)
  if (crochets)
    cat('\\left[\n')
  else
    cat('\\left(\n')
  print(ret)
  if (crochets)
    cat('\\right]\n')
  else
    cat('\\right)\n')
  invisible(NULL)
}