importFeatureCounts <- function(file, skip=0, headerLine=2){
  tmp <- read.table(file, header=TRUE, stringsAsFactors=FALSE, skip=skip)
  
# If we skip some of the head, we might cut off the header, so I import it here again.  
  if(skip>0){
    header <- read.table(file, header=FALSE, stringsAsFactors=FALSE, skip=headerLine-1, nrows = 1)
    colnames(tmp) <- header
  }


  expValues <- tmp[,c(1,7)]
  geneInfo <- tmp[,1:6]
  tmp <- read.table(paste(file,".summary",sep=""), header=TRUE, stringsAsFactors=FALSE)
  result <- list(expValues=expValues, geneInfo=geneInfo, summary=tmp)
  class(result) <- "featureCounts"
  result
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

summary.featureCounts <- function(x, ...){
  x$summary
}