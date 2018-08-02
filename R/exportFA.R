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
#' \dontrun{
#' novelBed <- data.frame(Chr=c(11,18,3),
#' Start=c(72554673, 62550696, 18148822),
#' End=c(72555273, 62551296, 18149422),
#' Gene=c("LOC1", "LOC2", "LOC3"))
#'
#' myFasta <- getFastaFromBed(novelBed, species="Bos taurus", 
#' +                          fastaFolder="/home/daniel/fasta/")
#'
#' exportFA(myFasta, file="myFasta.fa")
#' }

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