#' Affiche le code LaTeX de la matrice augmentée
#'
#' Cette fonction permet l'affichage du code LaTeX d'une matrice augmentée. Elle 
#' permet de choisir l'affichage entre \code{crochets} ou parenthèses.
#' Nous pouvons déterminer le nombre de chiffres après la virgule avec
#' \code{digits} et d'afficher sous forme de \code{fractions} ou non.
#' Le paramètre \code{sel} permet d'afficher la matrice augmentée d'un SEL ou la matrice
#' augmentée de l'inversion d'une matrice.
#'
#' @param A une matrice
#' @param sel logique; si \code{TRUE}, on affiche la matrice augmentée d'un SEL, sinon on affiche la matrice augmentée de l'inversion d'une matrice.
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
#' B <- matrix(c(2, 1, -1, 4,
#'              -3, -1, 2, 6,
#'              -2,  1, 2, -4,
#'              0, 4, 7, -5), 4, 4)
#'
#' print_matrix_augm(A)
#' print_matrix_augm(A, digits = 2)
#' print_matrix_augm(A, crochets = FALSE)
#' print_matrix_augm(A/7, fractions = TRUE)
#' print_matrix_augm(B, sel = FALSE)
#' print_matrix_augm(cbind(A,diag(ncol(A))), sel = FALSE)
#'
print_matrix_augm <- function(A, sel = TRUE, crochets = TRUE, fractions = FALSE, digits = 0, ...){
  ret <- A
  if (fractions) ret <- as.character(MASS::fractions(A))
  if (!sel && (ncol(A) %% 2 != 0)) stop("Le nombre de colonnes de A doit être pair lors de l'inversion d'une matrice.")
  if (sel)
    ret <- xtable::xtableMatharray(ret, digits = digits, align = c(rep("r",ncol(A)),"|","r"), ...)
  else
    ret <- xtable::xtableMatharray(A, digits = digits, align = c(rep("r",ncol(A)/2+1),"|",rep("r",ncol(A)/2)), ...)
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