\name{visualise}
\alias{visualise}
\title{Visualise}
\usage{
visualise(FPKM,classed)
}
\arguments{
    \item{classed}{A data frame where each row represents the probability for given a sample to fit each of the four classes as well as its overall classification based on the chosen thresholds.}
     \item{FPKM}{A matrix of fragments/reads per kilobase per million. Rows are genes and columns are samples.}
}
\value{Produces a multidimensional scaling (MDS) plot for all of the samples in your fpkm table and colours them based on the classification in table classed.}
\description{
Produces a multidimensional scaling (MDS) plot for all of the samples in your fpkm table and colours them based on the classification in table classed. 
}
\examples{
cf <- system.file("data","test_data.txt",package="AllSorts") #Get path to raw text file (a tsv)
counts <- read.table(file=cf,sep=' ',stringsAsFactors = FALSE,header=TRUE)
FPKM <- streamline(counts[,c(1:6)],counts$Gene_Length)
thresh <- c(0.2,0.2,0.8,0.9)
classed <- classify(FPKM,thresh)
visualise(FPKM,classed)
}
