source("src/config.R")
source("src/grouping_wrapper.R")

grouping_experiment = function(algorithm) {
  normal_anomaly = extract_normal_anomaly()
  
  message("Number of anomalies: ", nrow(normal_anomaly$anomaly))
  message("Number of normal: ", nrow(normal_anomaly$normal))
  
  training_test = extract_training_test(normal_anomaly)
  data_training = training_test$training
  data_test = training_test$test
  
  model = anomaly_detector()
  model$train(algorithm, data_training$normal, clusters_count, tolerance)
  
  correct_decision_rate = calculate_success(data_test, model)
  
  message("Success rate for anomaly: ", correct_decision_rate$true_anomaly)
  message("Success rate for normal: ", correct_decision_rate$true_normal)
}
