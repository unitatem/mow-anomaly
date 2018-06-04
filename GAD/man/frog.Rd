\name{frog}
\alias{frog}
\docType{data}
\title{
Frog calls data for anomaly detection
}
\description{
This data set was taken from Kaggle and contains data of frog calls. This set has been adjusted for anomaly detection via treating "Scinax Rubber" species as anomaly
and eliminating useless rows.
}
\usage{data("frog")}
\format{
  List with two fields: "normal" and "anomaly" containing respective data frames. Normal contains 7047 rows and anomaly contains 148 rows. There are 22 colums in each data frame.
}
\source{
https://archive.ics.uci.edu/ml/datasets/Anuran+Calls+\%28MFCCs\%29
}
