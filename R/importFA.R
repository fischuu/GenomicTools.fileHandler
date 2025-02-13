#' Importing a Fasta File.
#' 
#' This function imports a standard fasta file
#' 
#' This function imports a standard fasta file. Hereby, it does not matter if the identifier and sequence are alternating or not,
#' as the rows starting with '>' are used as identifer.
#' 
#' The example file was downloaded from here and was then further truncated respective transformed to fasta format:
#' 
#' ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase3/data/HG00096/sequence_read/
#' 
#' @param file Specifies the filename/path
#' @param toupper Logical, input sequence will to rewriten to capital letters only
#' @param verbose Logical, verbose function output
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

importFA <- function(file, toupper=TRUE, verbose=TRUE){
  # Read in the fasta file line by line
    res <- readLines(file)
  
    if(toupper){
      res <- toupper(res)
      if(verbose) message("Input nucleotides were capitalised")
    } 
    
  # Getting messages on the imported lines
    if(verbose) message("Number of read lines: ", length(res),"\n")
  
  # Check if the Fasta file is alternating, one line label, the next line sequence
    greplRes <- grepl(">",res)
    if(verbose) message("Number of header lines:", sum(greplRes),"\n")
  
  # Test if header and content rows are alternating
    is_odd_sequence <- function(x) {
      diffs <- diff(x)
      all(diffs == 2)
    }
    
    alternatingRows <- FALSE
    if( is_odd_sequence(which(greplRes))) {
      if(verbose) message("It seems your fasta file has alternating header/sequence rows")
      alternatingRows <- TRUE
    } else {
      if(verbose) message("It seems your fasta file does not have alternating header/sequence rows")
    }
  
  # Now populate the output vector with the sequences
    if(alternatingRows){
    # Sequences are alternating, hence we can use this to quickly import the files
      seq <- res[seq(2,length(res),2)]
      names(seq) <- res[seq(1,length(res)-1,2)]
    } else {
      idRows <- which(greplRes)
      numberOfSequences <- length(idRows)
      
      # NOTE: Quick and dirty for now with a loop, fix that to be faster later!!!
      seq <- rep("", numberOfSequences)
      if(numberOfSequences>1){
        for(i in 1:(numberOfSequences-1)){
          seq[i] <- paste(res[(idRows[i]+1):(idRows[i+1]-1)], collapse="")
        }
      } else {
        seq[1] <- paste(res[-1],collapse="")
      }
      
      seq[numberOfSequences] <- paste(res[(idRows[i+1]+1):(length(res))], collapse="")
      names(seq) <- res[idRows]
    }
  class(seq) <- "fa"
  seq
}
