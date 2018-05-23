rm(list = ls())

source("src/config.R")
source("src/grouping_experiment.R")
source("src/pam_wrapper.R")

message("START")
grouping_experiment(grouping_pam(), extract_normal_anomaly, clusters_count)
message("END")
