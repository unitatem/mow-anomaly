grouping_hclust <- function(data, clusters) {
  model <- hclust(dist(data))
  return(cutree(model, clusters))
}
