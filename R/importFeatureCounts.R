#' Import from FeatureCounts
#' 
#' This functions imports the output from FeatureCounts
#' 
#' FeatureCounts produces two files, the txt that contain the expression values and then the summary that containts
#' all the information about the mapping statistics. This function imports both and stores them in a corresponding list.
#' 
#' @param file Character, file name
#' @param skip Number of lines to skip from txt file
#' @param headerLine Linenumber that contains the header information
#' 
#' @return A list with expValues, geneInfo and summary
#' 
#' @author Daniel Fischer
#' 
#' @examples 
#' 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","featureCountsExample.txt", package="GenomicTools.fileHandler")
#'  # Import the example featureCounts file  
#'    fcFile <- importFeatureCounts(file=fpath)
#'  
#' @export

importFeatureCounts <- function(file, skip=0, headerLine=2){
  tmp <- read.table(file, header=TRUE, stringsAsFactors=FALSE, skip=skip)
  
# If we skip some of the head, we might cut off the header, so I import it here again.  
  if(skip>0){
    header <- read.table(file, header=FALSE, stringsAsFactors=FALSE, skip=headerLine-1, nrows = 1)
    colnames(tmp) <- header
  }


  expValues <- tmp[,c(1,7)]
  geneInfo <- tmp[,1:6]
  tmp <- read.table(paste(file,".summary",sep=""), header=TRUE, stringsAsFactors=FALSE)
  result <- list(expValues=expValues, geneInfo=geneInfo, summary=tmp)
  class(result) <- "featureCounts"
  result
}



