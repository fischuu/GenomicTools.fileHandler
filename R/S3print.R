#' Print a bed Object
#' 
#' Prints a \code{bed} object.
#' 
#' The print function displays a bed object
#' 
#' @name print.bed
#' @docType methods
#' @param x Object of class \code{bed}.
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export

print.bed <- function(x, n=6, ...){
  n <- min(n, nrow(x))
  out <- as.data.frame(x)
  print(out[1:n,])
  if(n<nrow(x)) message("...\n",nrow(x)-n," rows not displayed.")
}

#' Print a fa Object
#' 
#' Prints a \code{fa} object.
#' 
#' The print function displays a fa object
#' 
#' @name print.fa
#' @docType methods
#' @param x Object of class \code{fa}.
#' @param n Number of sequences to display
#' @param seq.out Length of the subsequence to display
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export


`print.fa` <- function(x, n=2, seq.out=50, ...){
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

#' Print a fa Object
#' 
#' Prints a \code{fa} object.
#' 
#' The print function displays a fa object
#' 
#' @name print.fq
#' @docType methods
#' @param x Object of class \code{fq}.
#' @param n Number of sequences to display
#' @param seq.out Length of the subsequence to display
#' @param print.qual Logical, shall the quality measures also be printed
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export


`print.fq` <- function(x, n=2, seq.out=50, print.qual=TRUE, ...){
  seq <- x$seq
  qual <- x$qual
  if(!is.numeric(n)) stop("The argument n has to be numeric.")
  if(n>length(seq)){
    n <- length(seq)
    warning("n cannot be larger than length(x). Hence, I set n <- length(x)")
  }
  seq <- seq[1:n]
  qual <- qual[1:n]
  if(!is.null(seq.out)){
    if(!is.numeric(seq.out)) stop("The argument seq.out has to be numeric.")
    for(i in 1:n){
      seq[i] <- paste(substr(seq[i],1,seq.out),"...",sep="")
      qual[i] <- paste(substr(qual[i],1,seq.out),"...",sep="")
    }
  }
  if(print.qual) seq <- rbind(seq, qual)
  print(seq,...)
  if(n<length(seq)) message("Fastq sequences ommited to print: ", length(x$seq)-n)
} 

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

#' Print a pedMap Object
#' 
#' Prints an \code{pedMap} object.
#' 
#' The print function displays a pedMap object
#' 
#' @name print.pedMap
#' @docType methods
#' @param x Object of class \code{pedMap}.
#' @param n Number of samples to display
#' @param m Number of columns to display
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export
 
print.pedMap <- function(x, n=6, m=6, ...){
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

#' Print a vcf Object
#' 
#' Prints an \code{vcf} object.
#' 
#' The print function displays a vcf object
#' 
#' @name print.vcf
#' @docType methods
#' @param x Object of class \code{vcf}.
#' @param n Number of samples to display
#' @param m Number of columns to display
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export


print.vcf <- function(x, n=6, m=6, fullHeader=FALSE,...){
  
  # Correct for too large n and m
  nm <- dim(x$genotypes)
  nHeader <- min(n, length(x$header))
  n <- min(n,nm[1])
  m <- min(m,nm[2])
  
  # Print the genotypes  
  cat("First",n,"rows and",m,"columns of $genotypes:\n")
  print(cbind(rownames(x$genotypes)[1:n], x$genotypes[1:n,x$map$snp.names[1:m], with=FALSE] ))
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
