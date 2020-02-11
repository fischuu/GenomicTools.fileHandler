#' Importing a Fastq File.
#' 
#' This function imports a standard fastq file
#' 
#' This function imports a standard fastq file that consists out of blocks of four lines per entry
#' 
#' @param file Specifies the filename/path
#' 
#' @return An object of class \code{fq} containing the sequences and the quality meausure. The names correspond to the sequence names given in the fasta file.
#' 
#' @author Daniel Fischer
#' 
#' @seealso print.fq, summary.fq
#' 
#' @examples 
#' 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.fastq", package="GenomicTools.fileHandler")
#'  # Import the example fastq file  
#'    fastqFile <- importFQ(file=fpath)
#'  
#' @export
#' 
#' @importFrom graphics boxplot



# This function reads in a fasta file and prepares the vector from it
  importFQ <- function(file){
    res <- readLines(file)
  # Check if the Fastq file starts every fourth line with @
    sumAlternating <- sum(grepl("@",substr(res[seq(1,length(res),4)],1,1)))
    if(sumAlternating!=(length(res)/4)) stop("Your Fastq file is malformed. Please ensure that name rows (every fourth row...) start with @")
    seq <- res[seq(2,length(res),4)]
    qual <- res[seq(4,length(res),4)]
    names(seq) <- res[seq(1,length(res)-1,4)]
    names(qual) <- res[seq(1,length(res)-1,4)]
    
    res <- list(seq=seq, qual=qual)
    class(res) <- "fq"
    res
  } 
  
  
