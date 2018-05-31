library(RANN)
library(cluster)

grouping_algorithm <- setRefClass("grouping_algorithm", 
                                  methods=list(
                                    get_cluster_params = function(data, clusters) {
                                      stop("get_cluster_params method must be implemented! It should return clustering vector.")
                                    }
                                  ))

normalize <- function(data, max, min) {
  data <- sweep(data, 2, min)
  data <- sweep(data, 2, max-min, "/")
  return(data)
}

discard_outliers <- function(vec) {
  qnt <- quantile(vec, probs=c(.25, .75))
  H <- 1.5 * IQR(vec)
  vec[vec < (qnt[1] - H)] <- NA
  vec[vec > (qnt[2] + H)] <- NA
  
  return(vec)
}

anomaly_detector <- function (..., algorithm, data, clusters) {
  UseMethod("anomaly_detector")
}

anomaly_detector.default <- function(..., algorithm, data, clusters) {
  max <- apply(data, 2, max)
  min <- apply(data, 2, min)
  data <- normalize(data, max, min)
  
  clustering <- algorithm$get_cluster_params(data, clusters)
  
  border <- vector("numeric", length=clusters)
  for (i in 1:clusters) {
    current_cluster <- data[clustering == i, ]
    
    dist_vec <- apply(as.matrix(dist(current_cluster)), 1, mean)
    dist_vec <- discard_outliers(dist_vec)
    
    border[i] <- max(dist_vec, na.rm=TRUE)
  }
  silh <- mean(silhouette(clustering, dist=dist(data))[, 3])
  
  model <- structure(list(training_data=data, clustering=clustering, clusters=clusters, 
                         max=max, min=min, border=border, silh=silh),
                    class="anomaly_detector")
  return(model)
}

anomaly_detector.formula <- function(formula, subset, ..., algorithm, data, clusters) {
  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  
  call <- match.call()
  mf <- call
  
  if (identical(class(eval.parent(mf$data)), "matrix"))
    mf$data <- as.data.frame(eval.parent(mf$data))
  
  mf$algorithm <- NULL
  mf$clusters <- NULL
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  
  mt <- attr(mf, "terms")
  attr(mt, "intercept") <- 0
  data <- model.frame(mt, mf)
  
  result <- anomaly_detector.default(algorithm=algorithm, data=data, clusters=clusters)
  
  result$call <- call
  result$call[[1]] <- as.name("anomaly_detector")
  result$terms <- mt
  class(result) <- c("anomaly_detector.formula", class(result))

  return (result)
}

predict.anomaly_detector <- function(object, data, binary=TRUE, ...) {
  data <- normalize(data, object$max, object$min)
  result <- matrix(0, nrow=nrow(data), ncol=object$clusters)
  
  for (i in 1:object$clusters) {
    current_cluster <- object$training_data[object$clustering == i, ]
    
    partial_result <- nn2(current_cluster, data, k=nrow(current_cluster))$nn.dists
    partial_result <- as.numeric(apply(partial_result, 1, mean))
    partial_result <- partial_result - object$border[i]
    
    if (object$border[i] != 0)
      partial_result <- partial_result / object$border[i]
    
    result[, i] <- partial_result
  }
  result <- apply(result, 1, min)
  if (binary)
    return(result <= 0)
  else
    return(result)
}

predict.anomaly_detector.formula <- function(object, data, binary=TRUE, ...) {
  data <- model.frame(object$terms, data)
  return(predict.anomaly_detector(object, data, binary))
}
