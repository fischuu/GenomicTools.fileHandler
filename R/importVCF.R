#' importVCF
#' 
#' Import a VCF function
#' 
#' This function imports a VCF file.
#' 
#' In case the logicl flag 'phased' is set to TRUE then the genotypes are expected to be in the format 0|0, otherwise they are expected
#' to be like 0/1 .
#' 
#' The example file was downloaded from here:
#' 
#' ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/pilot_data/release/2010_07/exon/snps/
#' 
#' 
#' @param file The file name
#' @param na.seq The missing value definition
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

importVCF <- function(file, na.seq="./."){
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

  
# Extract the genotype information
  genotypes <- vcfBody[, .SD, .SDcols = -c(1:9)]

# Remove the additional FORMAT fields (THIS INFORMATION COULD LATER ALSO STILL BE EXTRACTED!!!)
  cols = names(genotypes) 
  genotypes[ , (cols) := lapply(.SD, function(x) {gsub("\\:.*","",x)}), .SDcols = cols]

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
  genotypes[genotypes=="1/1"] <- "02"
  
  
  genotypesRN <- colnames(genotypes)
  genotypes <- genotypes[, data.table(t(.SD), keep.rownames=TRUE)]  # Takes long, IMPROVE IT!!!

  genotypes[,rn:=NULL]
  setnames(genotypes, map[[2]])
  rownames(genotypes) <- genotypesRN
  
# Then import the body
  out <- list(header=header, vcfBody, map=map, genotypes=genotypes)
  class(out) <- "vcf"
  out
}
