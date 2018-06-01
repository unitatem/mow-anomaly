\name{grouping_kmeans}
\alias{grouping_kmeans}
\title{Group data using kmeans}
\description{
This function groups given data using kmeans algorithm - it works as a wrapper for kmeans.
It can be used as an "algorithm" argument for training \code{anomaly_detector} model.
}
\usage{
grouping_kmeans(data, clusters)
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
grouping_kmeans <- function(data, clusters) {
  model <- kmeans(data, clusters, iter.max=50)
  return(model$cluster)                               
}
}