rm(list = ls())

source("src/config.R")
source("src/experiment.R")

message("START")
random_forest_experiment(extract_normal_anomaly)
message("END")
