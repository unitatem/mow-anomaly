read_csv_file = function(path) {
  data = read.csv(file = path,
                  header = TRUE,
                  sep = ",")
  return(data)
}

extract_training_test = function(normal_anomaly) {
  perm_vect = sample(1:normal_anomaly$normal_nrow)
  divide_idx = nrow(normal_anomaly$anomaly)
  
  idx_normal_2_test = perm_vect[1:divide_idx]
  idx_normal_2_training = perm_vect[(divide_idx + 1):normal_anomaly$normal_nrow]
  
  data_training = list(normal = normal_anomaly$normal[idx_normal_2_training, ])
  
  data_test = list(normal = normal_anomaly$normal[idx_normal_2_test,],
                   anomaly = normal_anomaly$anomaly)
  
  result = list(training = data_training, test = data_test)
  return(result)
}

calculate_success = function(data_test, trained_model) {
  correct_decision = list(true_anomaly = 0, true_normal = 0)
  
  classification = predict(trained_model, data_test$anomaly)
  correct_decision$true_anomaly = sum(!classification) / nrow(data_test$anomaly)
  
  classification = predict(trained_model, data_test$normal)
  correct_decision$true_normal = sum(classification) / nrow(data_test$normal)
  
  return(correct_decision)
}
