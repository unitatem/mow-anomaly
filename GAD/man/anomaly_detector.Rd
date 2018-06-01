\name{anomaly_detector}
\alias{anomaly_detector}
\alias{anomaly_detector.default}
\alias{anomaly_detector.formula}
\title{Anomaly detection by grouping}
\description{
This function trains chosen grouping model which can later be used for anomaly detection. Usage
requires wrapping chosen grouping algorithm by function which accepts training_data and number
of clusters and returns clustering vector. Examples can be found in documentation of
\code{grouping_kmeans} and \code{grouping_hclust}.
}
\usage{
anomaly_detector(..., algorithm, data, clusters)

\method{anomaly_detector}{default}(..., algorithm, data, clusters)
\method{anomaly_detector}{formula}(formula, subset, ..., algorithm, data, clusters)
}
\arguments{
  \item{\dots}{ not used. }
  \item{algorithm}{ chosen grouping algorithm wrapped in function specified in description. }
  \item{data}{ data on which to train the model. }
  \item{clusters}{ number of clusters data should be split into. }
  \item{formula}{ a symbolic description of the model. }
  \item{subset}{ an index vector indicating which rows should be used. }
}
\value{
  An object of class \code{anomaly_detector}, a list including elements
  \item{training_data}{ normalized data used for training }
  \item{clustering}{ clustering vector corresponding to training set }
  \item{clusters}{ number of clusters the training set was split into }
  \item{max}{ vector of max for all columns in training set - for normalization }
  \item{min}{ vector of min for all columns in training set - for normalization }
  \item{border}{ vector of hyper-n-ellipsoids' radii }
  \item{silh}{ mean silhouette width of all points in training set }
}
