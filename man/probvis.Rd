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
%\examples{
%probvis(classed)
%}
