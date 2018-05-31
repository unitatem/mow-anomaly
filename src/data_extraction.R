source("src/utils.R")

extract_normal_anomaly_card <- function(path) {
  mixed_data <- read_csv_file(path)
  # first column is ID, second column is transaction time, last column is class
  idx_col <- 3:(ncol(mixed_data) - 1)
  
  data <- list(normal = 0,
              anomaly = 0,
              normal_nrow = 0)
  
  data$normal <- mixed_data[mixed_data$Class == 0, idx_col]
  data$anomaly <- mixed_data[mixed_data$Class == 1, idx_col]
  
  return(data)
}

extract_normal_anomaly_frog <- function(path) {
  mixed_data <- read_csv_file(path)
  # last 4 columns are classification data
  idx_col <- 1:(ncol(mixed_data) - 4)
  anomalous_species <- "ScinaxRuber"
  
  data <- list(normal = 0,
              anomaly = 0,
              normal_nrow = 0)
  
  data$normal <- mixed_data[mixed_data$Species != anomalous_species, idx_col]
  data$anomaly <- mixed_data[mixed_data$Species == anomalous_species, idx_col]
  
  return(data)
}
