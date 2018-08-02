#' prereadGTF
#' 
#'Preread a gtf file and prints features of it for importing it.
#'
#' This function reads in a gtf file and prints its features for the import step. 
#' 
#' By default this function only imports the first 1000 rows, in case all rows should be imported set \code{nrow=-1}.
#' 
#' The number to skip in the beginning can be adjusted by the \code{skip} option. The default is here \code{auto} so that
#' the function can identify the correct amount of header rows. Hence, this option should be changed only, if there is a
#' good reason.
#' 
#' @param file Filename
#' @param nrow Number of rows to read
#' @param skip Rows to skip from top
#' 
#' @return A list of available features
#' 
#' @author Daniel Fischer



prereadGTF <- function(file, nrow=1000, skip="auto"){
  gtf <- file
  
  if(skip=="auto"){
    if(last(strsplit(gtf,"\\.")[[1]])=="gz"){
      cat("dfg")
    } else {
    con  <- file(gtf, open = "r")
    search <- TRUE
    obsRow <- 0
    while(search){
      obsRow <- obsRow + 1
      tmp <- readLines(con, n = 1, warn = FALSE)  
      if(substr(tmp,1,1)!="#"){
        skip <- obsRow -1
        search <- FALSE
      }
      if(obsRow==1000) search <- FALSE
    }
    close(con)
    }
  } else {
    if(!is.numeric(skip)) stop("ERROR: skip needs to be a numeric value!")
  }
  
  if(last(strsplit(gtf,"\\.")[[1]])=="gz"){
    cuffLoaded <- fread(input = paste('zcat',gtf), nrows=nrow, skip=skip, colClasses = c("character",
                                                                             "character",
                                                                             "character",
                                                                             "integer",
                                                                             "integer",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character"))
  } else {
    cuffLoaded <- fread(input = gtf, skip=skip, nrows=nrow, colClasses = c("character",
                                                               "character",
                                                               "character",
                                                               "integer",
                                                               "integer",
                                                               "character",
                                                               "character",
                                                               "character",
                                                               "character"))      
  }
  
  # Split the variable V9
  V9 <- cuffLoaded$V9
  V9 <- strsplit(V9,"; ")
  
  
# Output of the function  
  cat("Overview of features to import:\n")
  cat("-------------------------------\n")
  cat("File:",file,"\n")
  cat("Number of header rows:", skip,"\n")

# Print the features, if requested
  v3Entries <- paste(names(table(cuffLoaded$V3)),"\n")
  cat("\nList of levels in column 3:\n")
  cat("-----------------------------\n")
  cat(paste(gsub(" ","",v3Entries,"\n"), collapse=""))
  
  for(i in 1:length(v3Entries)){
    cat("\nList of features in column 9 for level",gsub("\n","",v3Entries[i]),"\n")
    cat("-----------------------------\n")
    rowsOI <- which(cuffLoaded$V3==gsub(" \n","",v3Entries[i]))
    tmpFeatures <- c()
    for(j in 1:length(rowsOI)){
      tmpFeatures <- unique(c(tmpFeatures,sapply(strsplit(V9[[j]]," "),"[",1)))
    }
    cat(gsub(" ","",paste(paste(sort(tmpFeatures),collapse="\n"),"\n",sep="")))
  }
}