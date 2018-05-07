source("src/grouping_wrapper.R")

read_csv_file = function(path) {
    data = read.csv(file = path,
                    header = TRUE,
                    sep = ",")
    return(data)
}

extract_normal_anomaly = function(mixed_data) {
    # first column is ID, second column is transaction time, last column is class
    idx_col = 3:(ncol(mixed_data) - 1)

    data = list(normal = 0,
                anomaly = 0,
                normal_nrow = 0)

    data$normal = mixed_data[mixed_data$Class == 0, idx_col]
    data$anomaly = mixed_data[mixed_data$Class == 1, idx_col]

    data$normal_nrow = nrow(data$normal)

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

get_cluster_parameters = function(data, model) {
    centers = model$centers
    distance = matrix()
    for (i in 1:nrow(model$centers))
        distance[i] = mean(dist(data[model$cluster == i, ]))

    df = list(centers = centers, distance = distance)
    return(df)
}

record_in_known_clusters = function(element, params) {
    for (i in 1:nrow(params$centers)) {
        distance = dist(rbind(element, params$centers[i, ]))
        if (distance <= params$distance[i])
            return(TRUE)
    }
    return(FALSE)
}

# calculate_success = function(data_test, clusters_param) {
#     correct_decision = list(true_anomaly = 0, true_normal = 0)
# 
#     for (i in 1:nrow(data_test$anomaly))
#         correct_decision$true_anomaly = correct_decision$true_anomaly + (record_in_known_clusters(data_test$anomaly[i,], clusters_param) == FALSE)
#     correct_decision$true_anomaly = correct_decision$true_anomaly / nrow(data_test$anomaly)
# 
#     for (i in 1:nrow(data_test$normal))
#         correct_decision$true_normal = correct_decision$true_normal + (record_in_known_clusters(data_test$normal[i, ], clusters_param) == TRUE)
#     correct_decision$true_normal = correct_decision$true_normal / nrow(data_test$normal)
# 
#     return(correct_decision)
# }

calculate_success = function(data_test, trained_model) {
  correct_decision = list(true_anomaly = 0, true_normal = 0)
  
  classification = trained_model$predict(data_test$anomaly)
  correct_decision$true_anomaly = sum(!classification) / nrow(data_test$anomaly)
  
  classification = trained_model$predict(data_test$normal)
  correct_decision$true_normal = sum(classification) / nrow(data_test$normal)
  
  return(correct_decision)
}

message("START")

source("src/config.R")
source("src/kmeans_wrapper.R")

data_raw = read_csv_file(creadit_card_10k)
print(head(data_raw, 3))

normal_anomaly = extract_normal_anomaly(data_raw)

message("Number of anomalies: ", nrow(normal_anomaly$anomaly))
message("Number of normal: ", nrow(normal_anomaly$normal))

training_test = extract_training_test(normal_anomaly)
data_training = training_test$training
data_test = training_test$test

# model = kmeans(data_training$normal, 10)
# clusters_param = get_cluster_parameters(data_training$normal, model)

model = anomaly_detector()
model$train(grouping_kmeans(), data_training$normal, 10)

correct_decision_rate = calculate_success(data_test, model)

message("Success rate for anomaly: ", correct_decision_rate$true_anomaly)
message("Success rate for normal: ", correct_decision_rate$true_normal)

message("END")
