% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importGFF.R
\name{importGFF}
\alias{importGFF}
\title{importGFF}
\usage{
importGFF(
  file,
  skip = "auto",
  nrow = -1,
  use.data.table = TRUE,
  level = "gene",
  features = NULL,
  num.features = c("FPKM", "TPM"),
  print.features = FALSE,
  merge.feature = NULL,
  merge.all = TRUE,
  class.names = NULL,
  verbose = TRUE
)
}
\arguments{
\item{file}{file or folder}

\item{skip}{numeric, lines to skip}

\item{nrow}{numeric, lines to read}

\item{use.data.table}{logical}

\item{level}{Character, read level, default: "gene"}

\item{features}{features to import}

\item{num.features}{names of the numeric features}

\item{print.features}{Logical, print available features}

\item{merge.feature}{Character, merge multiple samples to dataset}

\item{merge.all}{Logical, shall all samples be merged together}

\item{class.names}{Definition of class name sin V9}

\item{verbose}{Logical, verbose function output}
}
\value{
A gff object
}
\description{
Import a GFF file
}
\details{
This function imports a standard gff file.
}
\examples{

 # Define here the location on HDD for the example file
   fpath <- system.file("extdata","example.gff", package="GenomicTools.fileHandler")
 # Import the example gff file  
   importGFF(fpath)
   
}
\author{
Daniel Fischer
}
