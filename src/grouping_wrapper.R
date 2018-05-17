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
                                            centers="matrix", 
                                            distance="numeric", 
                                            std_dev="numeric", 
                                            max="numeric", 
                                            min="numeric",
                                            dist_coeff="numeric"),
                                
                                methods=list(
                                  
                                  train = function(algorithm, data, clusters, tolerance) {
                                    alg <<- algorithm
                                    
                                    max <<- apply(data, 2, max)
                                    min <<- apply(data, 2, min)
                                    data = sweep(data, 2, min)
                                    data = sweep(data, 2, max-min, "/")
                                    
                                    clustering = alg$get_cluster_params(data, clusters)
                                    
                                    centers <<- matrix(nrow=clusters, ncol=ncol(data))
                                    for (i in 1:clusters)
                                      centers[i, ] <<- apply(data[clustering == i, ], 2, mean)
                                    
                                    norm_distance = vector("numeric")
                                    for (i in 1:nrow(centers)) {
                                      current_cluster = data[clustering == i, ]
                    
                                      dist_vec = 0
                                      
                                      for (j in 1:nrow(current_cluster))
                                        dist_vec[j] = alg$calc_dist(current_cluster[j, ], centers[i, ])
                                      
                                      distance[i] <<- mean(dist_vec)
                                      std_dev[i] <<- sd(dist_vec, na.rm=TRUE)
                                      if (is.na(std_dev[i]))  #in case of just one observation in the cluster
                                        std_dev[i] <<- 0
                                      
                                      norm_distance = c(norm_distance, (dist_vec - distance[i]) / std_dev[i])
                                    }
                                    dist_coeff <<- quantile(norm_distance, (1 - tolerance))
                                  },
                                  
                                  predict = function(data) {
                                    data = sweep(data, 2, min)
                                    data = sweep(data, 2, max-min, "/")
                                    result = 0
                                    
                                    for (i in 1:nrow(data)) {
                                      result[i] = FALSE
                                      for (j in 1:nrow(centers)) {
                                        d = alg$calc_dist(data[i, ], centers[j, ])
                                        if (d <= distance[j] + dist_coeff*std_dev[j]) {
                                          result[i] = TRUE
                                          break
                                        }
                                      }
                                    }
                                    return(result)
                                  }
                                ))

