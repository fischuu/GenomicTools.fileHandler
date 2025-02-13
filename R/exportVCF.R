#' Exporting a VCF object.
#'
#' This function exports a vcf object into a standard vcf file.
#'
#' This function exports a vcf object to a standard vcf file. If no file name is given, the variable name will be used instead.
#' 
#' @param vcf vcf object
#' @param file Character, specifies filename/path
#' 
#' @return A vcf file
#' 
#' @author Daniel Fischer
#' 
#' @examples 
#'  # Define here the location on HDD for the example file
#'    fpath <- system.file("extdata","example.vcf", package="GenomicTools.fileHandler")
#'  # Import the example fasta file  
#'  #  vcfFile <- importVCF(file=fpath)
#'    
#'  # myfile <- file.path(tempdir(), "myLocs.vcf")
#'    
#'   exportVCF(newVCF, file=myfile)
#'
#' 
#' @export

exportVCF <- function(vcf, file=NULL){
  
  if(is.null(file)){
    file <- deparse(substitute(vcf))
    file <- paste(file,".vcf",sep="")
    cat("No file name (option: 'file') given, use the variable name instead:", file, "\n")  
  }
  
  columnHead <- paste(c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", rownames(vcf$genotypes)), collapse="\t")
  
  writeHeader <- c(vcf$header,
                 columnHead)
  
  
  
  con <- file(file, "w")
  writeLines(writeHeader,con)
  
  # Construct INFO field efficiently
  info_str <- apply(vcf$info, 1, function(row) {
    paste0(colnames(vcf$info), "=", row, collapse = ";")
  })
  
  # Create a new genotype table for export (avoid modifying vcf$genotypes)
  genotype_export <- transpose(vcf$genotypes)
  genotype_export[genotype_export=="00"] <- "0/0"
  genotype_export[genotype_export=="01"] <- "0/1"
  genotype_export[genotype_export=="02"] <- "1/1"
  genotype_export[genotype_export=="03"] <- "./."
  
  # genotype_export <- apply(vcf$genotypes, c(1, 2), function(gt) genotype_map[gt])
  
  # Concatenate the genotypes and genotypeInfo row by row, separating by ":"
  # Step 2: Concatenate cell-by-cell (this can be done with mapply)
  genotype_combined_dt <- mapply(function(gt, info) paste0(gt, ":", info),
                                 genotype_export[], vcf$genotypesInfo[])
    

  genotype_combined <- apply(genotype_combined_dt, 1, paste, collapse = "\t")
  
  
  # Construct VCF body efficiently
  vcf_body <- paste(
    vcf$map$V1,                         # CHROM
    vcf$map$V4,                         # POS
    ".",                                # ID (missing)
    vcf$map$allele.1,                   # REF
    vcf$map$allele.2,                   # ALT
    ".",                                # QUAL (missing)
    "PASS",                             # FILTER
    info_str,                           # INFO (precomputed)
    "GT:AD",                            # FORMAT
    genotype_combined,                  # Genotype data
    sep = "\t"
  )
  
  # Write data to file
  writeLines(vcf_body, con)
  close(con)
}
