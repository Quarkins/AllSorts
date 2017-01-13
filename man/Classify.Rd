\name{classify}
\alias{classify}
\title{classify}
\usage{
classify(FPKM,thresh)
}
\arguments{
    \item{FPKM}{A matrix of fragments/reads per kilobase per million. Rows are genes and columns are samples.}
    \item{thresh}{A character vector of four floats (between 0 and 1) which are the thresholds for ERG,ETV,Phlike and Other respectively.}
}
\value{
A data frame where each row represents the probability for given a sample to fit each of the four classes as well as its overall classification based on the chosen thresholds.
}
\description{
Classifies the streamlined log FPKM samples into one of the four classes: Phlike, ETV, ERG, Other or returns Unclassified if none of the classes have a probability of classification above the threshold (default: ERG:0.25, ETV:0.25, Phlike:0.5, Other:0.75)
}
\examples{
library(AllSorts)
cf <- system.file("data","test_data.txt",package="AllSorts") #Get path to raw text file (a tsv)
counts <- read.table(file=cf,sep=' ',stringsAsFactors = FALSE,header=TRUE)
FPKM <- streamline(counts[,c(1:6)],counts$Gene_Length)
thresh <- c(0.2,0.2,0.8,0.9)
classed <- classify(FPKM,thresh)
}
