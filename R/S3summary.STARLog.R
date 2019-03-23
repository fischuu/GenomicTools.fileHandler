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

summary.STARLog <- function(object, ...){
  
  cat("STARLog Summary\n")
  cat("---------------\n")
  STARVersion <- unique(object$Log$starVersion)
  ifelse(length(STARVersion)==1, STARVersionPassed <- "PASSED!", STARVersionPassed <- "WARNING!")
  cat(STARVersionPassed," Used STAR Version        :", STARVersion,"\n")    
}