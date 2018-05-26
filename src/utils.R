read_csv_file = function(path) {
  data = read.csv(file = path,
                  header = TRUE,
                  sep = ",")
  return(data)
}

extract_training_test = function(data, training_count) {
  if (training_count == 0)
    return(list(test = data, training = data.frame()))
  else if (training_count == nrow(data))
    return(list(test = data.frame(), training = data))
  
  perm_vect = sample(1:nrow(data))
  
  idx_normal_2_training = perm_vect[1:training_count]
  idx_normal_2_test = perm_vect[(training_count + 1):nrow(data)]
  
  return(list(test = data[idx_normal_2_test, ], training = data[idx_normal_2_training, ]))
}

extract_training_test_list = function(normal_anomaly, anomaly_2_train = 0) {
  anomaly_training_count = as.integer(nrow(normal_anomaly$anomaly) * anomaly_2_train)
  normal_training_count = nrow(normal_anomaly$normal) - nrow(normal_anomaly$anomaly)
  
  anomaly = extract_training_test(normal_anomaly$anomaly, anomaly_training_count)
  normal = extract_training_test(normal_anomaly$normal, normal_training_count)
  
  data_training = list(normal = normal$training, anomaly = anomaly$training)
  
  data_test = list(normal = normal$test, anomaly = anomaly$test)
  
  result = list(training = data_training, test = data_test)
  return(result)
}

toLogical = function(x) {
  if (is.factor(x))
    x = as.numeric(x) - 1
  if (is.numeric(x))
    x = as.logical(x)
  return(x)
}

calculate_success = function(data_test, trained_model) {
  correct_decision = list(true_anomaly = 0, true_normal = 0, true_class = 0)
  
  classification = toLogical(predict(trained_model, data_test$anomaly))
  correct_decision$true_anomaly = sum(!classification) / nrow(data_test$anomaly)
  correct_decision$true_class = sum(!classification)
  
  classification = toLogical(predict(trained_model, data_test$normal))
  correct_decision$true_normal = sum(classification) / nrow(data_test$normal)
  correct_decision$true_class = correct_decision$true_class + sum(classification)
  
  correct_decision$true_class = correct_decision$true_class / (nrow(data_test$normal) + nrow(data_test$anomaly))
  
  return(correct_decision)
}
