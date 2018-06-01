\name{discard_outliers}
\alias{discard_outliers}
\title{Discard outliers}
\description{
Removes objects from vector which are lower than q1 - 1.5*(q3-q1) or greater than q3 + 1.5*(q3-q1), where q stands for quantile.
}
\usage{
discard_outliers(vec)
}
\arguments{
  \item{vec}{ vector to be cleaned from outliers. }
}
\value{
  Vector cleaned from outliers.
}
