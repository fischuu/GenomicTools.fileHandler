#' Summary of a bed Object
#' 
#' Summarizes a \code{bed} object.
#' 
#' The summary function displays an informative summary of a bed object
#' 
#' @name summary.bed
#' @docType methods
#' @param x Object of class \code{bed}.
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


summary.featureCounts <- function(object, ...){
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