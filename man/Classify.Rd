\name{classify}
\alias{classify}
\title{classify}
\usage{
classify(sfpkm,thresh)
}
\description{
Classifies the streamlined log FPKM samples into one of the four classes: Phlike, ETV, ERG, Other or returns Unclassified if none of the classes have a probability of classification above the threshold (default: ERG:0.5, ETV:0.25, Phlike:0.5, Other:0.75)
}
\examples{
spfkm <- streamline(counts,genelengths)
thresh = c(0.2,0.2,0.8,0.9)
classes <- classify(spkm,thresh)
}
