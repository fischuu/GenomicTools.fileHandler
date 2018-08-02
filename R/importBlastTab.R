#' Import a Tab Delimited Blast Output File
#' 
#' This function imports a tab delimited blast output.
#' 
#' This function imports a tab delimited blast output file, currently the same as \code{read.table}
#' 
#' @param file Filename
#' 
#' @return A data.frame
#' 
#' @author Daniel Fischer       


importBlastTab <- function(file){
  out <- read.table(file,stringsAsFactors=FALSE, sep="\t")
  out
}