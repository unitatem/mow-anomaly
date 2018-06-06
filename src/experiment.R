library(e1071)
library(randomForest)
source("src/grouping_wrapper.R")

experiment <- function(model_gen, extract_normal_anomaly, anomaly2train) {
  normal_anomaly <- extract_normal_anomaly()
  
  training_test <- extract_training_test_list(normal_anomaly, anomaly2train)

  data_training <- training_test$training
  data_test <- training_test$test
  
  model <- model_gen(data_training)
  
  correct_decision_rate <- calculate_success(data_test, model)
  return(correct_decision_rate)
}

grouping_experiment <- function(algorithm, extract_normal_anomaly, clusters_count, samples) {
  df <- data.frame("clusters"=integer(),
                  "SR_anomaly"=double(),
                  "SR_normal"=double(),
                  "SR_overall"=double(),
                  "mean_silhouette"=double())

  for (i in clusters_count) {
    message("Clusters count: ", i, "/", tail(clusters_count, 1))
    for (j in 1:samples) {
      message("Sample: ", j, "/", samples)
      
      exp_result <- experiment(model_gen <- function(training_data) {
        return(anomaly_detector(algorithm=algorithm, data=training_data$normal, clusters=i))
      }, extract_normal_anomaly, 0)
      
      row <- data.frame(i, exp_result$true_anomaly, exp_result$true_normal, exp_result$true_class, exp_result$silh)
      names(row) <- c("clusters", "SR_anomaly", "SR_normal", "SR_overall", "mean_silhouette")

      df <- rbind(df, row)
    }
  }
  return(df)
}

classification_experiment <- function(algorithm, extract_normal_anomaly) {
  df <- data.frame("SR_anomaly"=double(),
                  "SR_normal"=double(),
                  "SR_overall"=double())
  
  for (i in 1:samples) {
    message("Sample: ", i, "/", samples)
    
    exp_result <- experiment(model_gen <- function(training_data) {
      x <- rbind(training_data$normal, training_data$anomaly)
      y <- as.factor(c(rep(1, nrow(training_data$normal)), rep(0, nrow(training_data$anomaly))))
      
      return(algorithm(x, y))
    }, extract_normal_anomaly, 0.5)
    
    row <- data.frame(exp_result$true_anomaly, exp_result$true_normal, exp_result$true_class)
    names(row) <- c("SR_anomaly", "SR_normal", "SR_overall")
    
    df <- rbind(df, row)
  }
  return(df)
}
