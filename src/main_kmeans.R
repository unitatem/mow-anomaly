rm(list = ls())

source("src/config.R")
source("src/experiment.R")
source("src/kmeans_wrapper.R")

message("START")
grouping_experiment(grouping_kmeans(), extract_normal_anomaly, clusters_count)
message("END")
