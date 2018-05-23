source("src/grouping_wrapper.R")

grouping_hclust <- setRefClass("grouping_hclust", contains="grouping_algorithm",
                               methods=list(
                                 get_cluster_params = function(data, clusters) {
                                   model = hclust(dist(data))
                                   return(cutree(model, clusters))
                                 }
                               ))
