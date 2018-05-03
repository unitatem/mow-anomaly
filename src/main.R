source("src/config.R")

read_csv_file = function(path) {
    data = read.csv(file = path,
                    header = TRUE,
                    sep = ",")
    return(data)
}

# closure, awesome
wrap_function_over_columns = function(lambda) {
    embeded_function = function(data) {
        cols_cnt = ncol(data)
        results = 1:cols_cnt
        for (c in 1:cols_cnt) {
            results[c] = lambda(data_safe[,c])
        }
        return(results)
    }
    return(embeded_function)
}

independent_probability = function(data, means, std) {
    rows_cnt = nrow(data)
    cols_cnt = ncol(data)

    result = 1:rows_cnt
    for (r in 1:rows_cnt) {
        result[r] = 1
        for (c in 1:cols_cnt)
            result[r] = result[r] * dnorm(data[r,c], mean = means[c], sd = deviations[c])
    }

    return(result)
}

print_atributes_parameters= function(prob, name = "") {
    message(name)
    message("min :", min(prob))
    message("max :", max(prob))
    message("mean :", mean(prob))
    message("sd :", sd(prob))
}

get_cluster_parameters = function(data, model) {
    centers = model$centers
    distance = matrix()
    for (i in 1:nrow(model$centers))
        distance[i] = mean(dist(data[model$cluster == i,]))

    df = list(centers = centers, distance = distance)
    return(df)
}

record_in_known_clusters = function(element, params) {
    for (i in 1:nrow(params$centers)) {
        distance = dist(rbind(element, params$centers[i,]))
        if (distance <= params$distance[i])
            return(TRUE)
    }
    return(FALSE)
}

message("START")

data = read_csv_file(creadit_card_10k)
print(head(data, 3))
message("Rate of frauds: ", sum(data$Class) / nrow(data))

# first column is ID, second column is transaction time, last column is class
data_x = data[,3:(ncol(data) - 1)]
data_y = data[,ncol(data)]

print(head(data_x, 3))
print(head(data_y, 3))

data_safe = data_x[data_y == 0,]
data_fraud = data_x[data_y == 1,]

message("Number of frauds: ", nrow(data_fraud))
message("Number of good transactions: ", nrow(data_safe))

calculate_mean = wrap_function_over_columns(mean)
calculate_sd = wrap_function_over_columns(sd)

means = calculate_mean(data_safe)
deviations = calculate_sd(data_safe)

prob_safe = independent_probability(data_safe, means, deviations)
print_atributes_parameters(prob_safe, "safe")
prob_fraud = independent_probability(data_fraud, means, deviations)
print_atributes_parameters(prob_fraud, "fraud")

model = kmeans(data_safe, 10)

clusters_param = get_cluster_parameters(data_safe, model)

detected_anomaly = 1:nrow(data_fraud)
for (i in 1:nrow(data_fraud)) {
    detected_anomaly[i] = (record_in_known_clusters(data_fraud[i,], clusters_param) == FALSE)
}

message("Detected anomaly ", sum(detected_anomaly), " out of ", nrow(data_fraud))

message("END")
