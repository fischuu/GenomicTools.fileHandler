#' Exporting a Bed File.
#'
#' This function exports a standard bed file.
#'
#' This function exports a data.frame to a standard bed file. If no file name is given, the variable name will be used instead.
#' 
#' @param x data.frame
#' @param file Character, specifies filename/path
#' @param header Logical, shall a header be written
#' 
#' @return A bed file
#' 
#' @author Daniel Fischer
#' 
#' @examples 
#' \dontrun{
#' novelBed <- data.frame(Chr=c(11,18,3),
#'                       Start=c(72554673, 62550696, 18148822),
#'                       End=c(72555273, 62551296, 18149422),
#'                       Gene=c("LOC1", "LOC2", "LOC3"))
#'
#' exportBed(novelBed, file="myLocs.bed")
#' exportBed(novelBed, file="myLocs_wHeader.bed", header=TRUE)
#' }
#' 
#' @export

exportBed <- function(x, file=NULL, header=FALSE){

  if(is.null(file)){
    file <- deparse(substitute(x))
    file <- paste(file,".bed",sep="")
    cat("No file name (option: 'file') given, use the variable name instead:", file, "\n")  
  }
  
  if(header){
    cat("write out the bed-file, using the following column names:\n")
    cat(colnames(x))
    write.table(x, file=file, row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)    
  } else {
    write.table(x, file=file, row.names=FALSE, col.names=FALSE, sep="\t", quote=FALSE)    
  }

}