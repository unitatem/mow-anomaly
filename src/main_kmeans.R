rm(list = ls())

source("src/config.R")
source("src/experiment.R")
source("src/kmeans_wrapper.R")

message("START")
df = grouping_experiment(grouping_kmeans(), extract_normal_anomaly, clusters_count, samples)
write.csv(df, file=paste(result_prefix, "_kmeans.csv"))
message("END")
