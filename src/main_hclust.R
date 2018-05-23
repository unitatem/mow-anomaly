rm(list = ls())

source("src/config.R")
source("src/grouping_experiment.R")
source("src/hclust_wrapper.R")

message("START")
grouping_experiment(grouping_hclust(), extract_normal_anomaly, clusters_count)
message("END")
