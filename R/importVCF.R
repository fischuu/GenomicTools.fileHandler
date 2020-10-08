#' importVCF
#' 
#' Import a VCF function
#' 
#' This function imports a VCF file.
#' 
#' In case the logicl flag 'phased' is set to TRUE then the genotypes are expected to be in the format 0|0, otherwise they are expected
#' to be like 0/1 . If the flag simplify is set genotypes like 0/2 or 1/2 will be set to 0,1,2 coding and multi-alternatives are ignored.
#' 
#'  If you would like to extract in addition to the genotype information further any other data from th vcf file formatted in the
#'  FORMAT field, you can specify their names in the \data{formatFields} option. Currently, it only accepts a single value.
#' 
#' The example file was downloaded from here:
#' 
#' ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/pilot_data/release/2010_07/exon/snps/
#' 
#' 
#' @param file The file name
#' @param na.seq The missing value definition
#' @param simplify Logical
#' @param getInfo Logical
#' @param formatFields Vector with names
#' 
#' @return A vcf object
#' 
#' @author Daniel Fischer
#' 
#' @examples 
#' 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.vcf", package="GenomicTools.fileHandler")
#'  # Import the example vcf file  
#'    importVCF(fpath)
#' 
#' @export

importVCF <- function(file, na.seq="./.", simplify=TRUE, getInfo=FALSE, formatFields=NULL){
# Necessary variable declaration for Cran checks
  V3 <- NULL
  rn <- NULL
  
 # First read in the header lines and determine the skip variable for the body
  con <- file(file) 
  open(con);
  results.list <- list();
  headerComplete <- FALSE
  headerLines <- 0
  header <- c()
  while (!headerComplete) {
    # Read in line by line
      oneLine <- readLines(con, n = 1, warn = FALSE)

    # Check if current line belongs to header
      lineStart <- substr(oneLine,1,2)
      if(lineStart=="##"){
        headerLines <- headerLines + 1
        header <- c(header, oneLine)
      } else {
        headerComplete <- TRUE
      }
    } 
  close(con)
  
  vcfBody <- fread(file, skip = headerLines, header=TRUE)
  
# Extract the map information
  map <- vcfBody[, .SD, .SDcols = c(1,3,2,4,5)]
  map[,V3:=0]
  setnames(map, c("V1", "snp.names", "V4", "allele.1", "allele.2", "V3"))
  setcolorder(map, c(1,2,6,3,4,5))
  map[[2]] <- as.character(map[[2]])
  
# If SNP names are missing, name them according to position:
  missingNames <- map[[2]]=="."
  if(sum(missingNames)>0){
    newLabels <- paste(map[[1]],map[[4]],sep=".")    
  # Test still for multi SNP per loci  
    tableNames <- table(newLabels)
    multLoci <- tableNames>1
    if(sum(multLoci)>0){
      lociOI <- tableNames[multLoci]
      for(locRun in 1:length(lociOI)){
        origLoc <- which(newLabels==names(lociOI)[locRun])
        for(indRun in 1:length(origLoc)){
          newLabels[origLoc[indRun]] <- paste(newLabels[origLoc[indRun]],indRun,sep=".")
        }
      }
    }
    map[[2]][missingNames] <- newLabels[missingNames]  
  }

# Get the information field
  if(getInfo){
    info <- do.call(rbind,strsplit(vcfBody$INFO,";"))
    colnames(info) <- sapply(strsplit(info[1,],"="),"[",1)
    for(i in 1:ncol(info)){
      info[,i] <- gsub(paste(colnames(info)[i],"=",sep=""), "", info[,i])
    }
    info <- apply(info,2,as.numeric)
  } else {
    info <- NULL
  }
    
# Extract the genotype information
  genotypes <- vcfBody[, .SD, .SDcols = -c(1:9)]
  gts <- vcfBody[, .SD, .SDcols = -c(1:9)]
  genotypesInfo <- vcfBody[, .SD, .SDcols = -c(1:9)]

# Get the different format fields
  gtFormat <- as.matrix(unique(vcfBody[, .SD, .SDcols = 9]))
  formats <- unlist(strsplit(gtFormat,":"))
  
# Remove the additional FORMAT fields
  cols = names(genotypes) 
  genotypes[ , (cols) := lapply(.SD, function(x) {gsub("\\:.*","",x)}), .SDcols = cols]
  genotypesInfo[ , (cols) := lapply(.SD, function(x) {gsub(".*?:","",x)}), .SDcols = cols]
  
# Earlier quick and dirty way:  
#  genotypes <- as.data.table(data.frame(lapply(genotypes, function(x) {gsub("\\:.*","",x)}), stringsAsFactors=FALSE))
  
# Change them to raw format look alike, it is NOT raw!!
  genotypes[genotypes==na.seq] <- "03"
    
  genotypes[genotypes=="0|0"] <- "00"
  genotypes[genotypes=="0|1"] <- "01"
  genotypes[genotypes=="1|0"] <- "01"
  genotypes[genotypes=="1|1"] <- "02"

  genotypes[genotypes=="0/0"] <- "00"
  genotypes[genotypes=="0/1"] <- "01"
  genotypes[genotypes=="1/0"] <- "01"
  genotypes[genotypes=="1/1"] <- "02"
  
  if(simplify){
    genotypes[genotypes=="0/2"] <- "01"
    genotypes[genotypes=="1/2"] <- "02"
    genotypes[genotypes=="2/2"] <- "02"
  }
  
  genotypesRN <- colnames(genotypes)
  genotypes <- genotypes[, data.table(t(.SD), keep.rownames=TRUE)]  # Takes long, IMPROVE IT!!!

  genotypes[,rn:=NULL]
  setnames(genotypes, map[[2]])
  rownames(genotypes) <- genotypesRN
  
# Create the FORMAT list
  formatMatrixList <- list()
  if(length(formatFields)>0){
    for(i in 1:length(formatFields)){
      tmpFormat <- which(is.element(formats, formatFields[i]))
      if(length(tmpFormat)==0) stop("Element",formatFields[i],"cannot be found in vcf.")
      tmpGts <- vcfBody[, .SD, .SDcols = -c(1:9)]
      n <- tmpFormat
      pat <- paste0("^([^:]*:){",n-1,"}")
      cols <- names(tmpGts) 
      tmpGts[ , (cols) := lapply(.SD, function(x, patInt=pat) {gsub(patInt,"",x)}), .SDcols = cols]
      tmpGts[ , (cols) := lapply(.SD, function(x) {gsub("\\:.*","",x)}), .SDcols = cols]
      formatMatrixList[[i]] <- tmpGts
    }
    names(formatMatrixList) <- formatFields
  }
  
# Then import the body
  out <- list(header=header, vcfBody, map=map, genotypes=genotypes, info=info, genotypesInfo=genotypesInfo, format=formatMatrixList)
  class(out) <- "vcf"
  out
}
