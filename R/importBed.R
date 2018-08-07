#' Importing a Bed File.
#' 
#' This function imports a standard bed file
#' 
#' This function imports a standard bed-file into a data.frame. It is basically a convenience wrapper around \code{read.table}
#' 
#' @param file Specifies the filename/path
#' @param header Logical, is a header present
#' 
#' @return A \code{data.frame}
#' 
#' @author Daniel Fischer
#' 
#' @seealso [exportBed], [read.table]
#' @examples
#' \dontrun{
#'    novelBed <- data.frame(Chr=c(11,18,3),
#'                           Start=c(72554673, 62550696, 18148822),
#'                           End=c(72555273, 62551296, 18149422),
#'                           Gene=c("LOC1", "LOC2", "LOC3"))
#'    
#'    exportBed(novelBed, file="myLocs.bed")
#'    
#'    novelBed.imp <- importBed(file="myLocs.bed")
#' }
#'  
#' @export

importBed <- function(file, header=FALSE){

  if(header){
    out <- read.table(file=file, row.names=FALSE, header=TRUE, sep="\t")    
  } else {
    out <- read.table(file=file, row.names=FALSE, header=FALSE, sep="\t")    
  }

  out
}