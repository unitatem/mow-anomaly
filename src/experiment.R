library(e1071)
source("src/grouping_wrapper.R")

experiment = function(model_gen, extract_normal_anomaly) {
  normal_anomaly = extract_normal_anomaly()
  
  message("Number of anomalies: ", nrow(normal_anomaly$anomaly))
  message("Number of normal: ", nrow(normal_anomaly$normal))
  
  training_test = extract_training_test(normal_anomaly)
  data_training = training_test$training
  data_test = training_test$test
  
  model = model_gen(data_training$normal)
  
  correct_decision_rate = calculate_success(data_test, model)
  
  message("Success rate for anomaly: ", correct_decision_rate$true_anomaly)
  message("Success rate for normal: ", correct_decision_rate$true_normal)
  message("Overall success rate: ", correct_decision_rate$true_class)
}

grouping_experiment = function(algorithm, extract_normal_anomaly, clusters_count) {
  experiment(model_gen = function(training_data) {
    return(anomaly_detector(algorithm, training_data, clusters_count))
  }, extract_normal_anomaly)
}

classification_experiment = function(extract_normal_anomaly, nu, gamma) {
  experiment(model_gen = function(training_data) {
    return(svm(training_data, y=!vector(length=nrow(training_data)), type="one-classification", nu=nu, gamma=gamma))
  }, extract_normal_anomaly)
}