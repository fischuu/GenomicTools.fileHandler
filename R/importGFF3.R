#' importGFF3
#' 
#' Import a GFF3 file
#' 
#' This function imports a standard gff3 file.
#' 
#' @param gff file or folder
#' @param chromosome The chromosome to import
#' 
#' @return A gff object
#' 
#' @author Daniel Fischer
#' 
#' @export


importGFF3 <- function(gff, chromosomes){
# fread zip support is OS dependend
  os <- "linux"
  if(grepl("Windows", sessionInfo()$running)) os <- "windows"

  # Now create the input string, depending on the os
  if(os=="linux"){
    inputString <-  paste('zcat',gff)
  } else if(os=="windows"){
    inputString <- paste("gzip -dc",gff)
  }
    
  tmpDT <- fread(inputString, sep="\n", header=FALSE)
  #commentRows <- which(substring(tmpDT[[1]], 1, 1)=="#")
  #keepThose <- 1:nrow(tmpDT)
  #keepThose <- keepThose[!is.element(keepThose,commentRows)]
  
  rowStarts <- substring(tmpDT[[1]], 1, max(nchar(chromosomes)))
  chromosomeRows <- grep(paste(chromosomes,collapse="|"), rowStarts, value=FALSE)
  
  tmpDT2 <- tmpDT[chromosomeRows,]
  V1 <- NULL # For the Cran check...
  tmpDT2[, c(paste("V",1:9, sep="") ) := tstrsplit(V1, "\t", fixed=TRUE)]
  tmpDT2
}