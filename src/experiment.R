library(e1071)
library(randomForest)
source("src/grouping_wrapper.R")

experiment = function(model_gen, extract_normal_anomaly, anomaly2train) {
  normal_anomaly = extract_normal_anomaly()
  
  message("Number of anomalies: ", nrow(normal_anomaly$anomaly))
  message("Number of normal: ", nrow(normal_anomaly$normal))
  
  training_test = extract_training_test_list(normal_anomaly, anomaly2train)

  data_training = training_test$training
  data_test = training_test$test
  
  model = model_gen(data_training)
  
  correct_decision_rate = calculate_success(data_test, model)
  
  message("Success rate for anomaly: ", correct_decision_rate$true_anomaly)
  message("Success rate for normal: ", correct_decision_rate$true_normal)
  message("Overall success rate: ", correct_decision_rate$true_class)
  message("Mean silhouette width: ", model$silh)
}

grouping_experiment = function(algorithm, extract_normal_anomaly, clusters_count) {
  experiment(model_gen = function(training_data) {
    return(anomaly_detector(algorithm=algorithm, data=training_data$normal, clusters=clusters_count))
  }, extract_normal_anomaly, 0)
}

svm_experiment = function(extract_normal_anomaly) {
  experiment(model_gen = function(training_data) {
    x = rbind(training_data$normal, training_data$anomaly)
    y = as.factor(c(rep(1, nrow(training_data$normal)), rep(0, nrow(training_data$anomaly))))
    
    return(svm(x, y, type="C-classification"))
  }, extract_normal_anomaly, 0.5)
}

random_forest_experiment = function(extract_normal_anomaly) {
  experiment(model_gen = function(training_data) {
    x = rbind(training_data$normal, training_data$anomaly)
    y = as.factor(c(rep(1, nrow(training_data$normal)), rep(0, nrow(training_data$anomaly))))
    
    return(randomForest(x, y))
  }, extract_normal_anomaly, 0.5)
}
