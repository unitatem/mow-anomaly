library(RANN)
library(cluster)

grouping_algorithm <- setRefClass("grouping_algorithm", 
                                  methods=list(
                                    get_cluster_params = function(data, clusters) {
                                      stop("get_cluster_params method must be implemented! It should return clustering vector.")
                                    }
                                  ))

normalize = function(data, max, min) {
  data = sweep(data, 2, min)
  data = sweep(data, 2, max-min, "/")
  return(data)
}

discard_outliers = function(vec) {
  qnt = quantile(vec, probs=c(.25, .75))
  H = 1.5 * IQR(vec)
  vec[vec < (qnt[1] - H)] <- NA
  vec[vec > (qnt[2] + H)] <- NA
  
  return(vec)
}

anomaly_detector = function(algorithm, data, clusters) {
  max = apply(data, 2, max)
  min = apply(data, 2, min)
  data = normalize(data, max, min)
  
  clustering = algorithm$get_cluster_params(data, clusters)
  
  border = vector("numeric", length=clusters)
  for (i in 1:clusters) {
    current_cluster = data[clustering == i, ]
    
    dist_vec = apply(as.matrix(dist(current_cluster)), 1, mean)
    dist_vec = discard_outliers(dist_vec)
    
    border[i] = max(dist_vec, na.rm=TRUE)
  }
  silh = mean(silhouette(clustering, dist=dist(data))[, 3])
  
  model = structure(list(training_data=data, clustering=clustering, clusters=clusters, 
                         max=max, min=min, border=border, silh=silh),
                    class="anomaly_detector_class")
  return(model)
}

predict.anomaly_detector_class = function(model, data, binary=TRUE) {
  data = normalize(data, model$max, model$min)
  result = matrix(0, nrow=nrow(data), ncol=model$clusters)
  
  for (i in 1:model$clusters) {
    current_cluster = model$training_data[model$clustering == i, ]
    
    partial_result = nn2(current_cluster, data, k=nrow(current_cluster))$nn.dists
    partial_result = as.numeric(apply(partial_result, 1, mean))
    partial_result = partial_result - model$border[i]
    
    if (model$border[i] != 0)
      partial_result = partial_result / model$border[i]
    
    result[, i] <- partial_result
  }
  result = apply(result, 1, min)
  if (binary)
    return(result <= 0)
  else
    return(result)
}
