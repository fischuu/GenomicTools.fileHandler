#' Exporting a Fasta File.
#'
#' This function exports a standard fasta file.
#'
#' This function exports a fasta object to a standard fasta file. If no file name is given, the variable name will be used instead.
#' 
#' @param fa fasta object
#' @param file Character, specifies filename/path
#' 
#' @return A fasta file
#' 
#' @author Daniel Fischer
#' 
#' @examples 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.fasta", package="GenomicTools.fileHandler")
#'  # Import the example fasta file  
#'    fastaFile <- importFA(file=fpath)
#'    newFasta <- fastaFile[1:5]
#'    
#' myfile <- file.path(tempdir(), "myLocs.fa")
#'    
#'   exportFA(newFasta, file=myfile)
#'
#' 
#' @export

exportFA <- function(fa, file=NULL){
  faNames <- names(fa)
  if(is.null(file)){
    file <- deparse(substitute(fa))
    file <- paste(file,".fa",sep="")
    cat("No file name (option: 'file') given, use the variable name instead:", file, "\n")  
  }
  con <- file(file, "w")
  writeLines(mixVectors(faNames,fa),con)
  close(con)
}