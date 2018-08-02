#' importSTARLog
#' 
#' Import the Log-File from STAR
#' 
#' This function imports the Log file from STAR
#' 
#' @param file The file name
#' 
#' @return a data frame
#' 
#' @author Daniel Fischer
#' 
#' @export

importSTARLog <- function(file){
  rawInput <- readLines(file)
  rawInput <- strsplit(rawInput," \\|\t")
  output <- cbind(sapply(rawInput,"[",1), sapply(rawInput,"[",2))
  output
}