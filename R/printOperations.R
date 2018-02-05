#' @export
printOperations <- function(A,
                            pivot = NULL,
                            digits = 2, 
                            fractions = FALSE,
                            method,
                            oper = TRUE,
                            tol = sqrt(.Machine$double.eps)){
  if (ncol(A) != 2){
    stop("Le nombre de colonnes doit être égal à 2")
  }
  cat('\\begin{array}{l}\n')
  if (method == "echange"){
    for (i in (1:nrow(A))){
      if (abs(A[i,1]) > tol){
        cat(paste(c("L_{",i,"} \\rightarrow L_{",A[i,1],"} \\\\\n"), collapse = ""))
      }
      else{
        cat("\\\\\n")
      }
    }
  } 
  else if (method == "combinaison"){
    for (i in (1:nrow(A))){
      if (abs(A[i,1]) > tol){
        if (A[i, 1] * A[i, 2] > 0){
          cat(paste(c(dfrac(A[i,1], oper),"L_{",pivot,"} + ",dfrac(abs(A[i,2]), oper),"L_{",i,"} \\rightarrow L_{",i,"} \\\\\n"), collapse = ""))
        }
        else{
          cat(paste(c(dfrac(A[i,1], oper),"L_{",pivot,"} - ",dfrac(abs(A[i,2]), oper),"L_{",i,"} \\rightarrow L_{",i,"} \\\\\n"), collapse = ""))
        }
      }
      else{
        cat("\\\\\n")
      }
    }
  }
  else if (method == "reduit"){
    for (i in (1:nrow(A))){
      if (abs(A[i,1]) > tol){
        if (abs(A[i, 1]-1) <= tol) {
          cat(paste(c(-"L_{",i,"} \\rightarrow L_{",i,"} \\\\\n"), collapse =""))
        }
        else{
          cat(paste(c(dfrac(A[i, 1]),"L_{",i,"} \\rightarrow L_{",i,"} \\\\\n"), collapse =""))
        }
      }
      else{
        cat("\\\\\n")
      }
    }
  }
  cat('\\end{array}\\\\\n')
}