rm(list = ls())

source("src/config.R")
source("src/experiment.R")
source("src/hclust_wrapper.R")

message("START")
df <- grouping_experiment(grouping_hclust, extract_normal_anomaly, clusters_count, samples)
# write.csv(df, file=paste(result_prefix, "_hclust.csv"))
message("END")
