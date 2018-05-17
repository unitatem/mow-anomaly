source("src/grouping_experiment.R")
source("src/pam_wrapper.R")

message("START")
grouping_experiment(grouping_pam())
message("END")
rm(list = ls())
