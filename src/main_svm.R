rm(list = ls())

source("src/config.R")
source("src/experiment.R")

message("START")
classification_experiment(extract_normal_anomaly, nu, gamma)
message("END")
