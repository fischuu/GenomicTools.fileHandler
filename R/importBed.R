#' Importing a Bed File.
#' 
#' This function imports a standard bed file
#' 
#' This function imports a standard bed-file into a data.frame. It is basically a convenience wrapper around \code{read.table}. However,
#' if no header lines is given, this function automatically assigns the column names, as they are given in the bed-specification on the
#' Ensembl page here: https://www.ensembl.org/info/website/upload/bed.html
#' 
#' @param file Specifies the filename/path
#' @param header Logical, is a header present
#' @param sep Column separator
#' 
#' @return A \code{data.frame}
#' 
#' @author Daniel Fischer
#' 
#' @seealso [exportBed], [read.table]
#' @examples
#'    
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.bed", package="GenomicTools.fileHandler")
#'  # Import the example bed file  
#'    novelBed.imp <- importBed(file=fpath)
#'  
#' @export

importBed <- function(file, header=FALSE, sep="\t"){

  headerNames <- c("chrom",
                   "chromStart",
                   "chromEnd",
                   "name",
                   "score",
                   "strand",
                   "thickStart",
                   "thickEnd",
                   "itemRgb",
                   "blockCount",
                   "blockSizes",
                   "blockStarts")
  
  if(header){
    out <- read.table(file=file, header=TRUE, sep=sep, stringsAsFactors = FALSE)    
  } else {
    out <- read.table(file=file, header=FALSE, sep=sep, stringsAsFactors = FALSE) 
    colnames(out) <- headerNames[1:ncol(out)]  
  }

  class(out) <- c("bed", "data.frame")
  out
}