library(cluster)
source("src/grouping_wrapper.R")

grouping_pam <- setRefClass("grouping_pam", contains="grouping_algorithm",
                               methods=list(
                                 get_cluster_params = function(data, clusters) {
                                   model = pam(data, clusters)
                                   return(model$clustering)
                                 }
                               ))
