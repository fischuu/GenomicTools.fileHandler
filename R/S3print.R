#' Print a featureCounts Object
#' 
#' Prints an \code{featureCounts} object.
#' 
#' The print function displays a featureCounts object
#' 
#' @name print.featureCounts
#' @docType methods
#' @param x Object of class \code{featureCounts}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export

print.fa <- function(x, n=2, seq.out=50, ...){
  if(!is.numeric(n)) stop("The argument n has to be numeric.")
  if(n>length(x)){
    n <- length(x)
    warning("n cannot be larger than length(x). Hence, I set n <- length(x)")
  }
  X <- x[1:n]
  if(!is.null(seq.out)){
    if(!is.numeric(seq.out)) stop("The argument seq.out has to be numeric.")
    for(i in 1:n){
      X[i] <- paste(substr(X[i],1,seq.out),"...",sep="")
    }
  }
  print(X,...)
  if(n<length(x)) message("Fasta sequences ommited to print: ", length(x)-n)
} 


print.featureCounts <- function(x, ...){
  cat("$expValues \n")
  print(head(x$expValues))
  cat("...\n",nrow(x$expValues)-6,"more rows!\n")
  cat("\n")
  cat("$geneInfo \n")
  print(head(x$geneInfo))
  cat("...\n",nrow(x$geneInfo)-6,"more rows!\n")
  cat("\n")
  cat("$summary \n")
  print(x$summary)
}


print.PedMap <- function(x, n=6, m=6, ...){
  # Correct for too large n and m
  nm <- dim(x$genotypes)
  n <- min(n,nm[1])
  m <- min(m,nm[2])
  
  cat("First",n,"rows and",m,"columns of $genotypes:\n")
  print(x$genotypes[1:n,x$map$snp.names[1:m], with=FALSE])
  cat("...",nrow(x$genotypes)-n,"rows and",ncol(x$genotypes)-m," columns omited \n\n")
  cat("First",n,"rows of $fam:\n\n")  
  print(x$fam[1:n,])
  cat("...",nrow(x$fam)-n,"rows omited \n\n")
  cat("First",n,"rows of $map:\n")  
  print(x$map[1:n,])
  cat("...",nrow(x$map)-n,"rows omited \n")
} 


print.vcf <- function(x, n=6, m=6, fullHeader=FALSE,...){
  
  # Correct for too large n and m
  nm <- dim(x$genotypes)
  nHeader <- min(n, length(x$header))
  n <- min(n,nm[1])
  m <- min(m,nm[2])
  
  # Print the genotypes  
  cat("First",n,"rows and",m,"columns of $genotypes:\n")
  print(x$genotypes[1:n,x$map$snp.names[1:m], with=FALSE])
  cat("...",nrow(x$genotypes)-n,"rows and",ncol(x$genotypes)-m," columns omited \n\n")
  
  # Print the header
  cat("First",n,"rows of $header:\n\n")  
  if(fullHeader){
    for(i in 1:nHeader){
      cat(x$header[i],"\n")
    }
  } else {
    for(i in 1:nHeader){
      cat(x$header[i],"\n")
    }
  }
  cat("...",length(x$header)-n,"rows omited \n\n")
  
  cat("First",n,"rows of $map:\n")  
  print(x$map[1:n,])
  cat("...",nrow(x$map)-n,"rows omited \n")
} 