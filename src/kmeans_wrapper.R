source("src/grouping_wrapper.R")

grouping_kmeans <- setRefClass("grouping_kmeans", contains="grouping_algorithm",
                               methods=list(
                                 get_cluster_params = function(data, clusters) {
                                   model = kmeans(data, clusters, iter.max=50)
                                   result = cluster_params(centers=model$center, clusters=model$cluster)
                                   return(result)
                                 }
                               ))
