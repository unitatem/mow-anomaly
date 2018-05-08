cluster_params <- setClass("model_params", slots=list(centers="matrix", clusters="numeric"))

grouping_algorithm <- setRefClass("grouping_algorithm", 
                                  methods=list(
                                    get_cluster_params = function(data, clusters) {
                                      stop("get_cluster_params method must be implemented! It should return correctly filled object of \"cluster_params\" class.")
                                    }
                                  ))

anomaly_detector <- setRefClass("anomaly_detector", 
                                fields=list(centers="matrix", distance="numeric", std_dev="numeric", max="numeric", min="numeric"),
                                methods=list(
                                  
                                  train = function(algorithm, data, clusters) {
                                    if (!extends(class(algorithm), "grouping_algorithm"))
                                      stop("algorithm variable should extend \"grouping_algorithm\" class!")
                                    
                                    max <<- apply(data, 2, max)
                                    min <<- apply(data, 2, min)
                                    data = sweep(data, 2, min)
                                    data = sweep(data, 2, max-min, "/")
                                    
                                    model_output = algorithm$get_cluster_params(data, clusters)
                                    
                                    centers <<- model_output@centers
                                    
                                    for (i in 1:nrow(model_output@centers)) {
                                      current_cluster = data[model_output@clusters == i, ]
                    
                                      dist_vec = 0
                                      
                                      for (j in 1:nrow(current_cluster))
                                        dist_vec[j] = dist(rbind(current_cluster[j, ], centers[i, ]))
                                      
                                      distance[i] <<- mean(dist_vec)
                                      std_dev[i] <<- sd(dist_vec, na.rm=TRUE)
                                      if (is.na(std_dev[i]))  #in case of just one observation being in the centre
                                        std_dev[i] <<- 0
                                    }
                                  },
                                  
                                  predict = function(data, dist_coeff="numeric") {
                                    data = sweep(data, 2, min)
                                    data = sweep(data, 2, max-min, "/")
                                    result = 0
                                    
                                    for (i in 1:nrow(data)) {
                                      result[i] = FALSE
                                      for (j in 1:nrow(centers)) {
                                        d = dist(rbind(data[i, ], centers[j, ]))
                                        if (d <= distance[j] + dist_coeff*std_dev[j]) {
                                          result[i] = TRUE
                                          break
                                        }
                                      }
                                    }
                                    return(result)
                                  }
                                ))

