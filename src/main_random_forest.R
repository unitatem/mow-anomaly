rm(list = ls())

source("src/config.R")
source("src/experiment.R")

message("START")
df = classification_experiment(randomForest, extract_normal_anomaly)
write.csv(df, file=paste(result_prefix, "_random_forest.csv"))
message("END")
