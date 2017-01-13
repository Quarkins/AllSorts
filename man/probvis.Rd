\name{probvis}
\alias{probvis}
\title{probvis}
\usage{
probvis(classed)
}
\arguments{
    \item{classed}{A data frame where each row represents the probability for given a sample to fit each of the four classes as well as its overall classification based on the chosen thresholds.}
}
\value{Plots a stacked bar chart depicting the probabilities a given sample has of being classified for each class (hence the sum is always 1.0)}
\description{
Produces a stacked bar chart depicting the probabilities a given sample has of being classified for each class (hence the sum is always 1.0). 
}
\examples{
library(AllSorts)
cf <- system.file("data","test_data.txt",package="AllSorts") #Get path to raw text file (a tsv)
counts <- read.table(file=cf,sep=' ',stringsAsFactors = FALSE,header=TRUE)
FPKM <- streamline(counts[,c(1:6)],counts$Gene_Length)
thresh <- c(0.2,0.2,0.8,0.9)
classed <- classify(FPKM,thresh)
probvis(classed)
}
