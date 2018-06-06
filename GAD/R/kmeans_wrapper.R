grouping_kmeans <- function(data, clusters) {
  model <- kmeans(data, clusters, iter.max=50)
  return(model$cluster)                               
}
