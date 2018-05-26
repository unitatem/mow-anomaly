rm(list = ls())

source("src/config.R")
source("src/experiment.R")

message("START")
svm_experiment(extract_normal_anomaly)
message("END")