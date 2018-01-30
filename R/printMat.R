printMat <- function(x){
  cat("\\begin{array}{",paste(rep("r",ncol(x)),sep = "", collapse = ""),"}", sep = "")
  cat("\n")
  for (i in 1:nrow(x)){
    cat(x[i,], sep = " & ")
    cat(" \\\\ \n")
  }
  cat("\\end{array}")
}