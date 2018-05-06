cluster_params <- setClass("model_params", slots=list(centers="numeric", clusters="numeric"))

grouping_algorithm <- setRefClass("grouping_algorithm", 
                                  methods=list(
                                    get_cluster_params = function(data, clusters) {
                                      stop("get_cluster_params method must be implemented! It should return correctly filled object of \"cluster_params\" class.")
                                    }
                                  ))

anomaly_detector <- setRefClass("anomaly_detector", 
                                fields=list(centers="numeric", distance="numeric"),
                                methods=list(
                                  
                                  train = function(algorithm, data, clusters) {
                                    if (!extends(class(algorithm), "grouping_algorithm"))
                                      stop("algorithm variable should extend \"grouping_algorithm\" class!")
                                    
                                    model_output = algorithm$get_cluster_params(data, clusters)
                                    
                                    centers <<- model_output$centers
                                    distance <<- matrix()
                                    for (i in 1:nrow(model$centers))
                                      distance[i] <<- mean(dist(data[model_output$clusters == i, ]))
                                  },
                                  
                                  predict = function(data) {
                                    result = matrix()
                                    
                                    for (i in 1:nrow(data)) {
                                      result[i] = FALSE
                                      for (j in 1:nrow(centers)) {
                                        d = dist(rbind(data[i, ], params$centers[j, ]))
                                        if (d <= distance[i]) {
                                          result[i] = TRUE
                                          break
                                        }
                                      }
                                    }
                                    return(result)
                                  }
                                ))

