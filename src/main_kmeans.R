source("src/grouping_experiment.R")
source("src/kmeans_wrapper.R")

message("START")
grouping_experiment(grouping_kmeans())
message("END")
rm(list = ls())
