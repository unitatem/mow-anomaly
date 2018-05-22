library(RANN)

normalize = function(data, upper_border, lower_border) {
  data = sweep(data, 2, lower_border)
  data = sweep(data, 2, upper_border-lower_border, "/")
  return(data)
}

discard_outliers = function(vec) {
  qnt = quantile(vec, probs=c(.25, .75))
  H = 1.5 * IQR(vec)
  vec[vec < (qnt[1] - H)] <- NA
  vec[vec > (qnt[2] + H)] <- NA
  
  return(vec)
}

grouping_algorithm <- setRefClass("grouping_algorithm", 
                                  methods=list(
                                    get_cluster_params = function(data, clusters) {
                                      stop("get_cluster_params method must be implemented! It should return clustering vector.")
                                    }
                                  ))

anomaly_detector <- setRefClass("anomaly_detector", 
                                fields=list(alg="grouping_algorithm",
                                            clustering="numeric",
                                            training_data="data.frame",
                                            border="numeric",
                                            clusters="numeric",
                                            max="numeric", 
                                            min="numeric"),
                                
                                methods=list(
                                  
                                  train = function(algorithm, data, clusters, tolerance) {
                                    alg <<- algorithm
                                    clusters <<- clusters
                                    border <<- vector("numeric", length=clusters)
                                    max <<- apply(data, 2, max)
                                    min <<- apply(data, 2, min)
                                    training_data <<- normalize(data, max, min)
                                    
                                    clustering <<- alg$get_cluster_params(training_data, clusters)
                                    
                                    
                                    for (i in 1:clusters) {
                                      current_cluster = training_data[clustering == i, ]
                                      
                                      dist_vec = apply(as.matrix(dist(current_cluster)), 1, mean)
                                      dist_vec = discard_outliers(dist_vec)
                                      
                                      border[i] <<- max(dist_vec, na.rm=TRUE)
                                    }
                                  },
                                  
                                  predict = function(data) {
                                    data = normalize(data, max, min)
                                    result = vector(length=nrow(data))
                                    
                                    for (i in 1:clusters) {
                                      current_cluster = training_data[clustering == i, ]
                                      
                                      partial_result = nn2(current_cluster, data, k=nrow(current_cluster))$nn.dists
                                      partial_result = as.numeric(apply(partial_result, 1, mean))
                                      partial_result = (partial_result <= border[i])
                                      
                                      result = result | partial_result
                                    }
                                    return(result)
                                  }
                                ))

