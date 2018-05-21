library(RANN)

grouping_algorithm <- setRefClass("grouping_algorithm", 
                                  methods=list(
                                    get_cluster_params = function(data, clusters) {
                                      stop("get_cluster_params method must be implemented! It should return clustering vector.")
                                    },
                                    calc_dist = function(point1, point2) {
                                      stop("calc_dist method must be implemented! It should return distance between two points in metric specific to grouping algorithm")
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
                                    
                                    max <<- apply(data, 2, max)
                                    min <<- apply(data, 2, min)
                                    data = sweep(data, 2, min)
                                    data = sweep(data, 2, max-min, "/")
                                    
                                    clustering <<- alg$get_cluster_params(data, clusters)
                                    training_data <<- data
                                    
                                    border <<- vector("numeric", length=clusters)
                                    clusters <<- clusters
                                    
                                    for (i in 1:clusters) {
                                      current_cluster = data[clustering == i, ]

                                      dist_vec = apply(as.matrix(dist(current_cluster)), 1, mean)
                                      
                                      qnt <- quantile(dist_vec, probs=c(.25, .75))
                                      H <- 1.5 * IQR(dist_vec)
                                      dist_vec[dist_vec < (qnt[1] - H)] <- NA
                                      dist_vec[dist_vec > (qnt[2] + H)] <- NA
                                      
                                      border[i] <<- max(dist_vec, na.rm=TRUE)
                                    }
                                  },
                                  
                                  predict = function(data) {
                                    data = sweep(data, 2, min)
                                    data = sweep(data, 2, max-min, "/")
                                    result = vector(length=nrow(data))
                                    
                                    for (i in 1:clusters) {
                                      current_cluster = training_data[clustering == i, ]
                                      partial_result = as.numeric(apply(nn2(current_cluster, query=data, k=nrow(current_cluster))$nn.dists, 1, mean))
                                      partial_result = (partial_result <= border[i])
                                      result = result | partial_result
                                    }
                                    return(result)
                                  }
                                ))

