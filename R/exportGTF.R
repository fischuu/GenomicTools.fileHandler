#' Exporting a GTF File.
#'
#' This function exports a standard gtf file.
#'
#' This function exports a gtf-object to a standard gtf file. 
#' 
#' @param x gtf-object
#' @param file Character, specifies filename/path
#' 
#' @return A gtf file
#' 
#' @author Daniel Fischer
#' 
#' @export

exportGTF <- function(x, file){
  featureColumns <- (1:ncol(x))[-(1:8)]
  V9 <- matrix("NA", ncol=length(featureColumns), nrow=nrow(x))
  index <- 1
  for(i in featureColumns){
    V9[,index] <- paste(colnames(x)[i],as.vector(as.matrix(x[, ..i])),sep="=")
    index <- index + 1
  }
  
  V9 <- apply(V9, 1, paste, collapse=";")
  x_out <- x[,1:8]
  x_out$V9 <- V9
  write.table(x_out, file=file, quote=FALSE, sep="\t", col.names = FALSE, row.names = FALSE)
}