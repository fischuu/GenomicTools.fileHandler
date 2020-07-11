#' Summary of a bed Object
#' 
#' Summarizes a \code{bed} object.
#' 
#' The summary function displays an informative summary of a bed object
#' 
#' @name summary.bed
#' @docType methods
#' @param object Object of class \code{bed}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods summary
#' @export

summary.bed <- function(object, ...){
  cat("Bed Summary\n")
  cat("---------------\n")
  cat("Number of lines        :", nrow(object),"\n")
  cat("Number of chromosomes  :", length(unique(object[,1])), "\n")
}

#' Summary of a fa Object
#' 
#' Summarizes a \code{fa} object.
#' 
#' The summary function displays an informative summary of a fa object
#' 
#' @name summary.fa
#' @docType methods
#' @param object Object of class \code{fa}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods summary
#' @export
#' 
summary.fa <- function(object, ...){
  
  nCharObj <- nchar(object)
  out <- data.frame(c("Sequences      :",
                      "Minimum length :",
                      "1st quartile   :",
                      "Median length  :",
                      "Average length :",
                      "3rd quartile   :",
                      "Maximum length :"),
                    c(length(object),
                      min(nCharObj),
                      quantile(nCharObj, 0.25),
                      median(nCharObj),
                      mean(nCharObj),
                      quantile(nCharObj, 0.75),
                      max(nCharObj))
  )
  colnames(out) <- out
  print(out, row.names=FALSE)  
} 

#' Summary of a fq Object
#' 
#' Summarizes a \code{fq} object.
#' 
#' The summary function displays an informative summary of a fq object
#' 
#' @name summary.fq
#' @docType methods
#' @param object Object of class \code{fq}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods summary
#' @export
#' 
summary.fq <- function(object, ...){
  
  nCharObj <- nchar(object$seq)
  cat("Summary of fq object\n")
  cat("--------------------\n")
  cat("Sequences      :",length(object$seq),"\n")
  cat("Minimum length :",min(nCharObj),"\n")
  cat("1st quartile   :",quantile(nCharObj, 0.25),"\n")
  cat("Median length  :",median(nCharObj),"\n")
  cat("Average length :",mean(nCharObj),"\n")
  cat("3rd quartile   :",quantile(nCharObj, 0.75),"\n")
  cat("Maximum length :",max(nCharObj),"\n")
  invisible(object)
  
} 

#' Summary of a featureCounts Object
#' 
#' Summarizes a \code{featureCounts} object.
#' 
#' The summary function displays an informative summary of a featureCounts object
#' 
#' @name summary.featureCounts
#' @docType methods
#' @param object Object of class \code{featureCounts}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods summary
#' @export
#' 

summary.featureCounts <- function(object, ...){
  object$summary
}

#' Summary of a gtf Object
#' 
#' Summarizes a \code{gtf} object.
#' 
#' The summary function displays an informative summary of a gtf object
#' 
#' @name summary.gtf
#' @docType methods
#' @param object Object of class \code{gtf}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods summary
#' @export
#' 

summary.gtf <- function(object, ...){
  cat("GTF Summary\n")
  cat("---------------\n")
  object$summary
}

summary.PedMap <- function(object, ...){
  
  cat("PedMap Summary\n")
  cat("---------------\n")
  cat("# of imported SNPs       :",nrow(object$map),"\n")
  cat("# of imported samples    :",nrow(object$fam),"\n")
  cat("# of missing sites       :",object$meta$missing,"\n")
  cat("# of monomorphic sites   :",object$meta$mono,"\n")
  cat("# of multiallelic sites  :",object$meta$multiallelic,"\n")
  cat("Used ped file            :",object$meta$pedFile,"\n")
  invisible(object)
} 