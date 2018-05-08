source("src/grouping_wrapper.R")
library("cluster")

grouping_pam <- setRefClass("grouping_pam", contains="grouping_algorithm",
                               methods=list(
                                 get_cluster_params = function(data, clusters) {
                                   model = pam(data, clusters)
                                   result = cluster_params(centers=unname(model$medoids), clusters=unname(model$clustering))
                                   return(result)
                                 }
                               ))