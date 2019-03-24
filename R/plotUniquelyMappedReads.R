#' plotUniquelyMappedReads
#' 
#' Plot the uniquely mapped reads
#' 
#' This function plots the percenage of uniquely reads from a STARlog object
#' 
#' Part of the diagnostic plot series for of the STARLog. The function accepts also a list of STARLogs and creates then comparative boxplots
#' 
#' @param STARLog A STARLog object
#' 
#' @return A plot
#' 
#' @author Daniel Fischer
#' 
#' @export

plotUniquelyMappedReads <- function(STARLog){
  options(scipen=999)
  if(class(STARLog)=="list"){
    STARitems <- length(STARLog)
    boxplot(as.numeric(gsub("%","",as.matrix(STARLog[[1]]$finalLog$uniqueReads[13,-1]))), main="Uniquely mapped reads (in %)", ylab="Reads", ylim=c(0,100), xlim=c(0.5,STARitems+0.5), at=1)
    for(i in 2:STARitems){
      boxplot(as.numeric(gsub("%","",as.matrix(STARLog[[i]]$finalLog$uniqueReads[13,-1]))), at=i, add=TRUE)
    }
  } else {
    boxplot(as.numeric(gsub("%","",as.matrix(STARLog$finalLog$uniqueReads[13,-1]))), main="Uniquely mapped reads (in %)", ylab="Reads")    
  }
}