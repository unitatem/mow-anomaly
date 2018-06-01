\name{grouping_hclust}
\alias{grouping_hclust}
\title{Group data using hclust}
\description{
This function groups given data using hclust algorithm - it works as a wrapper for hclust.
It can be used as an "algorithm" argument for training \code{anomaly_detector} model.
}
\usage{
grouping_hclust(data, clusters)
}
\arguments{
  \item{data}{ training data. }
  \item{clusters}{ number of clusters data should be split into. }
}
\value{
  Factor representing clustering of training data.
}
\examples{
##This function is currently defined as:
grouping_hclust <- function(data, clusters) {
  model <- hclust(dist(data))
  return(cutree(model, clusters))
}
}