\name{streamline}
\alias{streamline}
\title{Streamline}
\usage{
streamline(counts,glengths)
}
\arguments{
    \item{counts}{A matrix of raw integer counts where each row is a gene and each column a different sample.}
    \item{glengths}{The length in bases of all genes (rows) in the counts matrix for which the FPKM is normalised to.}
}
\value{A matrix of fragments/reads per kilobase per million. Rows are genes and columns are samples.
}
\description{
Produces a log FPKM (fragment/read per kilobase per million) of the
genes necessary for classification. Requires a data frame of raw counts per gene (each row corresponds to a gene) and a vector of gene lengths.
Note: in order to succesfully classify all twenty of the genes are required by the classifier.
}
\examples{
cf <- system.file("data","test_data.txt",package="AllSorts") #Get path to raw text file (a tsv)
counts <- read.table(file=cf,sep=' ',stringsAsFactors = FALSE,header=TRUE)
FPKM <- streamline(counts[,c(1:6)],counts$Gene_Length)
}
