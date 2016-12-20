\name{streamline}
\alias{streamline}
\title{Streamline}
\usage{
streamline(counts,genelengths)
}
\description{
Produces a log FPKM (fragment/read per kilobase per million) of the
genes necessary for classification. Requires a data frame of raw counts per gene (each row corresponds to a gene) and a vector of gene lengths.
Note: in order to succesfully classify all twenty of the genes are required by the classifier.
}
\examples{
streamline(counts,genelengths)
}
