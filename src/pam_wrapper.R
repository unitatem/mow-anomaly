source("src/grouping_wrapper.R")
library("cluster")

grouping_pam <- setRefClass("grouping_pam", contains="grouping_algorithm",
                               methods=list(
                                 get_cluster_params = function(data, clusters) {
                                   model = pam(data, clusters, metric="manhattan")
                                   return(unname(model$clustering))
                                 },
                                 calc_dist = function(point1, point2) {
                                   return(dist(rbind(point1, point2), method="manhattan"))
                                 }
                               ))