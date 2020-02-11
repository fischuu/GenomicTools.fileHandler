#' Summary of a STARLog Object
#' 
#' Summarizes a \code{STARLog} object.
#' 
#' The summary function displays an informative summary of a STARLog object
#' 
#' @name summary.STARLog
#' @docType methods
#' @param object Object of class \code{STARLog}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods summary
#' @export
#' @importFrom stats sd


summary.STARLog <- function(object, ...){
  
  cat("STARLog Summary\n")
  cat("---------------\n")
  STARVersion <- unique(object$Log$starVersion)
  ifelse(length(STARVersion)==1, STARVersionPassed <- "PASSED!", STARVersionPassed <- "WARNING!")
  cat("Used STAR Version        :", STARVersion,"-",STARVersionPassed,"\n")    

  totalReads <- as.numeric(as.matrix(object$finalLog$basicStats[2,-1]))
  cat("Total reads              :", prettyNum(mean(totalReads), big.mark=","),"(",prettyNum(sd(totalReads), big.mark=","),")","\n")    
  
  uniqueReads <- as.numeric(gsub("%","",as.matrix(object$finalLog$uniqueReads[13,-1])))
  cat("Unique reads             :", prettyNum(mean(uniqueReads), big.mark=","),"(",prettyNum(sd(uniqueReads),big.mark=","),")","\n")    
  
  }