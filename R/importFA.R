#' Importing a Fasta File.
#' 
#' This function imports a standard fasta file
#' 
#' This function imports a standard fasta file. It assumes that label and sequence lines are alternating,
#' meaning in the odd lines are the sequence names given, starting with > and in the even rows are the 
#' corresponding sequences.
#' 
#' The example file was downloaded from here and was then further truncated respective transformed to fasta format:
#' 
#' ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase3/data/HG00096/sequence_read/
#' 
#' @param file Specifies the filename/path
#' 
#' @return An object of class \code{fa} containing the sequences. The names correspond to the sequence names given in the fasta file.
#' 
#' @author Daniel Fischer
#' 
#' @seealso print.fa, summary.fa
#' 
#' @examples 
#' 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.fasta", package="GenomicTools.fileHandler")
#'  # Import the example fasta file  
#'    fastaFile <- importFA(file=fpath)
#'  
#' @export

# This function reads in a fasta file and prepares the vector from it
  importFA <- function(file){
    res <- readLines(file)
  # Check if the Fasta file is alternating, one line label, the next line sequence
    sumAlternating <- sum(grepl(">",res)==c(TRUE,FALSE))
    if(sumAlternating!=length(res)) stop("Your Fasta file is malformed. Please ensure that name rows start with > and that names and sequence
                                         rows are alternating.")
    seq <- res[seq(2,length(res),2)]
    names(seq) <- res[seq(1,length(res)-1,2)]
    class(seq) <- "fa"
    seq
} 
