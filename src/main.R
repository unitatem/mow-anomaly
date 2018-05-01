source("src/config.R")

message("START")

read_csv_file = function(path) {
    data = read.csv(file = path,
                    header = TRUE,
                    sep = ",")
    return(data)
}

data = read_csv_file(creadit_card_10k)
print(head(data, 3))
message("Rate of frauds: ", sum(data$Class) / nrow(data))

# first column is ID, second column is transaction time
data_x = data[,3:(ncol(data) - 1)]
data_y = data[,ncol(data)]

print(head(data_x, 3))
print(head(data_y, 3))

data_safe = data_x[data_y == 0,]
data_fraud = data_x[data_y == 1,]

message("Number of frauds: ", nrow(data_fraud))
message("Number of good transactions: ", nrow(data_safe))

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

calculate_mean = wrap_function_over_columns(mean)
calculate_sd = wrap_function_over_columns(sd)

means = calculate_mean(data_safe)
deviations = calculate_sd(data_safe)

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

fast_log = function(prob, name = "") {
    message(name)
    message("min :", min(prob))
    message("max :", max(prob))
    message("mean :", mean(prob))
    message("sd :", sd(prob))
}

prob_safe = independent_probability(data_safe, means, deviations)
fast_log(prob_safe, "safe")
prob_fraud = independent_probability(data_fraud, means, deviations)
fast_log(prob_fraud, "fraud")

message("END")

# kmeans_model = kmeans(data_safe, 1)
