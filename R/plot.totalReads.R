#' plot.totalReads
#' 
#' Plot the total reads
#' 
#' This function plots the total reads from a STARlog object
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

plot.totalReads <- function(STARLog){
  options(scipen=999)
  if(class(STARLog)=="list"){
    STARitems <- length(STARLog)
    boxplot(as.numeric(as.matrix(STARLog[[1]]$finalLog$basicStats[2,-1])), main="Total Reads", ylab="Reads", xlim=c(0.5,STARitems+0.5), at=1)
    for(i in 2:STARitems){
      boxplot(as.numeric(as.matrix(STARLog[[i]]$finalLog$basicStats[2,-1])), at=i, add=TRUE)
    }
  } else {
    boxplot(as.numeric(as.matrix(STARLog$finalLog$basicStats[2,-1])), main="Total Reads", ylab="Reads")    
  }
}