#' importSTARLog
#' 
#' Import the Log-File from STAR
#' 
#' This function imports the Log file from STAR
#' 
#' @param dir The directory name
#' @param recursive Logical, check for sub-directories
#' @param log boolean, import also log file
#' @param finalLog boolean, import also final_log file
#' @param verbose Logical, talkactive function feedback
#' 
#' @return a data frame
#' 
#' @author Daniel Fischer
#' 
#' @export

importSTARLog <- function(dir, recursive = TRUE, log=FALSE, finalLog=TRUE, verbose = TRUE){

# Get the file names and paths  
  files <- list()
  files$logFiles <- list.files(dir, recursive=recursive, full.names=TRUE, pattern="*Log.out")
  files$logFinalFiles <- list.files(dir, recursive=recursive, full.names=TRUE, pattern="*Log.final.out")
  files$logProgressFiles <- list.files(dir, recursive=recursive, full.names=TRUE, pattern="*Log.progress.out")
    
# Input checks, if input is complete
  files$n.logFiles <- length(files$logFiles)
  files$n.logFinalFiles <- length(files$logFinalFiles)
  files$n.logProgressFiles <- length(files$logProgressFiles)
  
  if(files$n.logFiles != files$n.logFinalFiles | files$n.logFiles != files$n.logProgressFiles){
    stop("Amount of log files does not match, probably incomplete input!")
  }
  
# Get the sample names
  files$sampleID <- gsub("Log.out","",basename(files$logFiles))
  
# Define the output vectors
  starVersion <- c()
  finalLine <- c()
  
# Now start to import the data from Log
  if(log){
    for(i in 1:files$n.logFiles){
      tmp.in <- readLines(files$logFiles[i])
    # Get the STAR version
      starVersion[i] <- sapply(strsplit(tmp.in[1],"="),"[",2)
      
    # Get the default values
      defaultParameters.start <- which(tmp.in=="##### DEFAULT parameters:")+1
      defaultParameters.end <- which(tmp.in=="##### Command Line:")-1
      dp <- tmp.in[defaultParameters.start:defaultParameters.end]
      tmp.split <- strsplit(dp," +")
      dp.1 <- sapply(tmp.split,"[",1)
      dp.2 <- sapply(sapply(tmp.split,"[",-1),paste,collapse=" ")
      if(i == 1){
        defaultParameter <- data.frame(Option=dp.1, dp.2)      
      } else {
        tmp.dp <- data.frame(Option=dp.1, dp.2)
        defaultParameter <- suppressWarnings(merge(defaultParameter, tmp.dp, by="Option"))
      }
      
    # Get the effective values
      effectiveParameters.start <- which(grepl("##### Final parameters after user input", tmp.in))+1
      effectiveParameters.end <- which(tmp.in=="----------------------------------------")-1
      ep <- tmp.in[effectiveParameters.start:effectiveParameters.end]
      tmp.split <- strsplit(ep," +")
      ep.1 <- sapply(tmp.split,"[",1)
      ep.2 <- sapply(sapply(tmp.split,"[",-1),paste,collapse=" ")
      if(i == 1){
        effectiveParameter <- data.frame(Option=ep.1, ep.2)      
      } else {
        tmp.ep <- data.frame(Option=ep.1, ep.2)
        effectiveParameter <- suppressWarnings(merge(effectiveParameter, tmp.ep, by="Option"))
      }
      
    # Process the genome generation
      gg.start <- which(tmp.in=="Reading genome generation parameters:")+2
      gg.end <- which(grepl("Number of real", tmp.in))-1
      gg <- tmp.in[gg.start:gg.end]
      tmp.split <- strsplit(gg," +")
      gg.1 <- sapply(tmp.split,"[",1)
      gg.2 <- sapply(sapply(tmp.split,"[",-1),paste,collapse=" ")
      gg.2 <- gsub(" ~RE-DEFINED", "", gg.2)
      if(i == 1){
        genomeGenerated <- data.frame(Option=gg.1, gg.2)      
      } else {
        tmp.gg <- data.frame(Option=gg.1, gg.2)
        genomeGenerated <- suppressWarnings(merge(genomeGenerated, tmp.gg, by="Option"))
      }
      
      finalLine[i] <- tmp.in[length(tmp.in)]
    }
  
    colnames(defaultParameter) <- c("Option",files$sampleID)
    colnames(effectiveParameter) <- c("Option",files$sampleID)
    colnames(genomeGenerated) <- c("Option",files$sampleID)
    
  } else {
    starVersion <- NULL
    defaultParameter <- NULL
    effectiveParameter <- NULL
    genomeGenerated <- NULL
    finalLine <- NULL
  }
  
  output.Log <- list(starVersion=starVersion,
                     defaultParameter=defaultParameter,
                     effectiveParameter=effectiveParameter,
                     genomeGenerated=genomeGenerated,
                     finalLine=finalLine)

# Now start to import the data from the final log  
  if(finalLog){
    for(i in 1:files$n.logFiles){
      tmp.in <- readLines(files$logFinalFiles[i])
  
      timeS <- tmp.in[1:4]
      tmp.split <- strsplit(timeS,"\\|\t")
      ts.1 <- sapply(sapply(tmp.split,"[",1),trim)
      ts.2 <- sapply(tmp.split,"[",2)
      if(i == 1){
        timeStats <- data.frame(Option=ts.1, ts.2)      
      } else {
        tmp.ts <- data.frame(Option=ts.1, ts.2)
        timeStats <- suppressWarnings(merge(timeStats, tmp.ts, by="Option"))
      }
      
      bS <- tmp.in[6:7]
      tmp.split <- strsplit(bS,"\\|\t")
      bs.1 <- sapply(sapply(tmp.split,"[",1),trim)
      bs.2 <- sapply(tmp.split,"[",2)
      if(i == 1){
        basicStats <- data.frame(Option=bs.1, bs.2)      
      } else {
        tmp.bs <- data.frame(Option=bs.1, bs.2)
        basicStats <- suppressWarnings(merge(basicStats, tmp.bs, by="Option"))
      }
      
      uS <- tmp.in[9:22]
      tmp.split <- strsplit(uS,"\\|\t")
      us.1 <- sapply(sapply(tmp.split,"[",1),trim)
      us.2 <- sapply(tmp.split,"[",2)
      if(i == 1){
        uniqueReads <- data.frame(Option=us.1, us.2)      
      } else {
        tmp.us <- data.frame(Option=us.1, us.2)
        uniqueReads <- suppressWarnings(merge(uniqueReads, tmp.us, by="Option"))
      }
      
      mmR <- tmp.in[24:27]
      tmp.split <- strsplit(mmR,"\\|\t")
      mm.1 <- sapply(sapply(tmp.split,"[",1),trim)
      mm.2 <- sapply(tmp.split,"[",2)
      if(i == 1){
        multiMapping <- data.frame(Option=mm.1, mm.2)      
      } else {
        tmp.mm <- data.frame(Option=mm.1, mm.2)
        multiMapping <- suppressWarnings(merge(multiMapping, tmp.mm, by="Option"))
      }
      
      umR <- tmp.in[29:31]
      tmp.split <- strsplit(umR,"\\|\t")
      um.1 <- sapply(sapply(tmp.split,"[",1),trim)
      um.2 <- sapply(tmp.split,"[",2)
      if(i == 1){
        unmapped <- data.frame(Option=um.1, um.2)      
      } else {
        tmp.um <- data.frame(Option=um.1, um.2)
        unmapped <- suppressWarnings(merge(unmapped, tmp.um, by="Option"))
      }
  
      cR <- tmp.in[33:34]
      tmp.split <- strsplit(cR,"\\|\t")
      c.1 <- sapply(sapply(tmp.split,"[",1),trim)
      c.2 <- sapply(tmp.split,"[",2)
      if(i == 1){
        chimeric <- data.frame(Option=c.1, c.2)      
      } else {
        tmp.c <- data.frame(Option=c.1, c.2)
        chimeric <- suppressWarnings(merge(chimeric, tmp.c, by="Option"))
      }    
    } 
   
    colnames(timeStats) <- c("Stat", files$sampleID)
    colnames(basicStats) <- c("Stat", files$sampleID)
    colnames(uniqueReads) <- c("Stat", files$sampleID)
    colnames(multiMapping) <- c("Stat", files$sampleID)
    colnames(unmapped) <- c("Stat", files$sampleID)
    colnames(chimeric) <- c("Stat", files$sampleID) 
    
  } else {
    timeStats <- NULL
    basicStats <- NULL
    uniqueReads <- NULL
    multiMapping <- NULL
    unmapped <- NULL
    chimeric <- NULL
  }
  
  output.finalLog <- list(timeStats=timeStats,
                          basicStats=basicStats,
                          uniqueReads=uniqueReads,
                          multiMapping=multiMapping,
                          unmappedReads=unmapped,
                          chimericReads=chimeric)
  
  output <- list(Log=output.Log,
                 finalLog=output.finalLog)

  class(output) <- "STARLog"
  output
}
