\name{predict.anomaly_detector}
\alias{predict.anomaly_detector}
\alias{predict.anomaly_detector.formula}
\title{predict}
\description{
Binary or score prediction for \code{anomaly_detector} model.
}
\usage{
\method{predict}{anomaly_detector}(object, data, binary = TRUE, ...)
}
\arguments{
  \item{object}{ model trained by \code{anomaly_detector}. }
  \item{data}{ data to be predicted. }
  \item{binary}{ flag indicating if the result vector sholud be logical or numerical. }
  \item{\dots}{ not used. }
}
\value{
  Logical vector indicating if given element is normal or score vector (higher values mean more anomalous) if argument binary = FALSE.
}
