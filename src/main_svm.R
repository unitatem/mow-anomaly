rm(list = ls())

source("src/config.R")
source("src/experiment.R")

message("START")
df = classification_experiment(svm, extract_normal_anomaly)
write.csv(df, file=paste(result_prefix, "_svm.csv"))
message("END")
