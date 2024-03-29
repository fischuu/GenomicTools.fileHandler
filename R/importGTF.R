#' Import a GTF File
#' 
#' This function imports a gtf file.
#' 
#' This function imports a gtf file. The features names to be imported are defined in \code{features}, several features are then
#' provided as vector. A list of available feature can beprinted, by setting \code{print.features=TRUE}.
#' 
#' The \code{skip} option allows to skip a given number of rows, the default is, however, \code{auto}. In that case, all rows that
#' start with the \code{#} symbol are skipped.
#'   
#' In case a set of expression values given in gtf format should be imported and to be merged into a single data table, the feature
#' that should be used for merging can be provided to the \code{merge.feature} option. In that case the function expects a folder
#' in \code{file} and it will import all gtfs located in that folder and merges them according to the \code{merge.feature} option.
#' With the option \code{class.names} a vector of prefixes for the merged features can be provided. If this is kept empty, then the 
#' filenames of the gtf will be used instead (without gtf extension).
#'   
#' By default the function imprts all features in column 9 as string character. However, for common labels (FPKM and TPM) the class
#' type is set automatically to numeric. Additional numerical feature names can be defined with the \code{num.feature} option.
#' 
#' @param file file or folder
#' @param skip numeric, lines to skip
#' @param nrow numeric, lines to read
#' @param use.data.table logical
#' @param level Character, read level, default: "gene"
#' @param features features to import
#' @param num.features names of the numeric features
#' @param print.features Logical, print available features
#' @param merge.feature Character, merge multiple samples to dataset
#' @param merge.all Logial, shall all samples be merged
#' @param class.names Vector with class names
#' @param verbose Logical, verbose function output
#' 
#' @return A gtf object
#' 
#' @author Daniel Fischer
#' 
#' @examples 
#' 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.gtf", package="GenomicTools.fileHandler")
#'  # Same file, but this time as gzipped version
#'    fpath.gz <- system.file("extdata","example2.gtf.gz", package="GenomicTools.fileHandler")
#'    
#'  # Import the example gtf file  
#'    importGTF(fpath, level="transcript", features=c("gene_id","FPKM"))
#'    
#'  \dontrun{
#'  # For the current you need to have zcat installed (should be standard on a Linux system)
#'    importGTF(fpath.gz, level="transcript", features=c("gene_id","FPKM"))
#'    }
#'  
#' @export

importGTF <- function(file, skip="auto", nrow=-1, use.data.table=TRUE, level="gene", features=NULL, num.features=c("FPKM", "TPM"), print.features=FALSE, merge.feature=NULL, merge.all=TRUE, class.names=NULL, verbose=TRUE){

# If no merge feature is given, we assume that only a single gtf is to be imported
   if(is.null(merge.feature)){
   out <- importGTF.internal(file=file, skip=skip, nrow=nrow, use.data.table=use.data.table, level=level, features=features, num.features=num.features, print.features=print.features, verbose=verbose)
 } else {
# In case we have a merge feature, we assume that file gives a folder location to gtfs that should be merged and merge feature gives the feature to use to merge.
   if(verbose) cat ("Start to import several gtfs and merge them using the feature",merge.feature,"\n")
   if(!is.element(merge.feature, features)){
     # features <- c(features, merge.feature)
     cat("Added",merge.feature,"to features-Option.")
    }
   
   gtfFiles <- list.files(file)
   gtfFiles <- gtfFiles[grep(".gtf",gtfFiles)]
   gtfNames <- gsub(".gtf","",gtfFiles)
   gtfNames <- gsub(".gz","",gtfNames)
   if(verbose) cat("Start to import", length(gtfFiles),"files:\n", paste(gtfFiles , collapse=" \n"),"\n")
   
  # Take here the feature list with names that are not used for merging
   features.small <- features[!is.element(features, merge.feature)]
   mergeThose <- c()
   
   allGTFs <- list()
   for(i in 1:length(gtfFiles)){
     if(verbose) cat("Start to import: ", gtfFiles[i],"\n")
     allGTFs[[i]] <- importGTF.internal(file=file.path(file,gtfFiles[i]), skip=skip, nrow=nrow, use.data.table=use.data.table, level=level, features=features, num.features=num.features, print.features=print.features, verbose=verbose)
     setkeyv(allGTFs[[i]], merge.feature)
     
     takeThose <- which(is.element(colnames(allGTFs[[i]]),features.small))
     for(j in 1:length(takeThose)){
       mergeThose <- c(paste(gtfNames[i],colnames(allGTFs[[i]])[takeThose[j]],sep="."))
       colnames(allGTFs[[i]])[takeThose[j]] <- mergeThose[length(mergeThose)]          
     }
     # Once the first two samples are read, merge them
     if(i==2) tmp <- merge(allGTFs[[1]], allGTFs[[2]][,unique(c(merge.feature,mergeThose)),with=FALSE], all=merge.all)     
     # Then add consecutive every turn the next one
     if(i>2){
       tmp <- merge(tmp, allGTFs[[i]][,unique(c(merge.feature,mergeThose)),with=FALSE], all=merge.all)       
     }
   }

   # Now set the columns names
   
   out <- tmp
   class(out) <- append(class(out), "gtf")
 }
  out
}

importGTF.internal <- function(file, skip="auto", nrow=-1, use.data.table=TRUE, level="gene", features=NULL, num.features=num.features, print.features=FALSE, merge.feature=NULL, verbose){
  gtf <- file
  
  if(skip=="auto"){
    if(last(strsplit(gtf,"\\.")[[1]])=="gz"){
    # Say something here!
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
  
  if(verbose) cat("Automatically detected number of rows to skip: ", skip,"\n")
  
  if(use.data.table){
    if(last(strsplit(gtf,"\\.")[[1]])=="gz"){
      cuffLoaded <- fread(input = paste('zcat',gtf), sep ="\t", colClasses = c("character",
                                                                               "character",
                                                                               "character",
                                                                               "integer",
                                                                               "integer",
                                                                               "character",
                                                                               "character",
                                                                               "character",
                                                                               "character"))
    } else {
      cuffLoaded <- fread(input = gtf, sep="\t", skip=skip, colClasses = c("character",
                                                                           "character",
                                                                           "character",
                                                                           "integer",
                                                                           "integer",
                                                                           "character",
                                                                           "character",
                                                                           "character",
                                                                           "character"))      
    }
    
    if(verbose){
      if(sum(cuffLoaded$V3==level)==0){
        availLevels <- unique(cuffLoaded$V3)
        stop("The given import level '",level,"' was not found in the gtf! Available level are: \n", paste(availLevels, collapse=" \n "))
      }
    }
    
    if(!is.null(level)) cuffLoaded <- cuffLoaded[cuffLoaded$V3==level,]
    if(nrow>0) cuffLoaded <- cuffLoaded[1:nrow,]
  } else {
    stop("Currently the importGTF function supports only data tables.")
    cuffLoaded <- read.csv(file=gtf, sep="\t", header=FALSE, stringsAsFactors=FALSE, skip=skip, nrow=nrow)
  }
  # Split the variable V9 and remove leading whitespaces
  V9 <- cuffLoaded$V9
  V9 <- strsplit(V9,";")
  V9 <- lapply(V9, trimws)
    
  # Print the features, if requested
  if(print.features || verbose){
    cat("List of features in column 9:\n")
    cat("-----------------------------\n")
    cat(paste(paste(sapply(strsplit(V9[[1]]," "),"[",1),"\n"), collapse=""))
  }
  
  # Remove the non-informative parts from that vectors
  if(is.null(features)){
    # Now get the required information from V9
    gene_id <- sapply(V9, function(x) x[grep("^gene_id",x)])
    gene_name <- sapply(V9, function(x) x[grep("^gene_name",x)])
    gene_biotype <- sapply(V9, function(x) x[grep("^gene_biotype",x)])
    gene_id <- gsub("gene_id ","",gene_id)
    gene_name <- gsub("gene_name ","",gene_name)
    gene_biotype <- gsub("gene_biotype ","",gene_biotype)
    gene_id <- gsub('\"',"",gene_id)
    gene_name <- gsub('\"',"",gene_name)
    gene_biotype <- gsub('\"',"",gene_biotype)
    gene_id <- gsub(';',"",gene_id)
    gene_name <- gsub(';',"",gene_name)
    gene_biotype <- gsub(';',"",gene_biotype)
    
    cuffLoaded[,V9:=NULL]
    cuffLoaded[,gene_id:=gene_id]
    cuffLoaded[,gene_name:=gene_name]
    cuffLoaded[,gene_biotype:=gene_biotype]
    
  } else {
    for(frun in 1:length(features)){
      tmpFeature <- sapply(V9, function(x) x[grep(paste("^",features[frun],sep=""),x)])
      tmpFeature <- gsub(" ","",tmpFeature)
      tmpFeature <- gsub(";","",tmpFeature)
      tmpFeature <- gsub("=","",tmpFeature)
      tmpFeature <- gsub(eval(features[frun]),"",tmpFeature)
      tmpFeature <- gsub('\"',"",tmpFeature)
      if(sum(is.element(features[frun],num.features))>0) tmpFeature <- as.numeric(tmpFeature)
      cuffLoaded[,eval(features[frun]):=tmpFeature]
    }
    cuffLoaded[,V9:=NULL]
  }
  
  class(cuffLoaded) <- append(class(cuffLoaded), "gtf")
  cuffLoaded
}
