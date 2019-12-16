#' @name aiRlayer
#'
#' @title aiRlayer
#'
#' @param dim1 row dimensions
#' @param dim2 column dimensions
#'
#' @return generates aiRlayer object of specified dimensions
aiRlayer <- function(dim1,dim2) {
  weights <- matrix(runif(n = dim1*dim2, min = -10, max = 10), nrow = dim1, ncol = dim2)
  colnames(weights) <- paste0("node",1:dim2)
  rownames(weights) <- paste0("weights",1:dim1)
  change.w <- matrix(0, nrow = dim1, ncol = dim2)
  bias <- runif(n = dim2, min = -5, max = 5)
  change.b <- rep(0, dim2)
  layer <- list(weights, bias, change.w, change.b)
  names(layer) <- c("weights","bias","change.w","change.b")
  class(layer) <- "aiRlayer"
  return(layer)

}
#' @name is.aiRlayer
#'
#' @title is.aiRlayer
#'
#' @param x object
#'
#' @return logical value
#'
#' @export
is.aiRlayer <- function(x) inherits(x, "aiRlayer")

#' @name aiRnet
#'
#' @title aiRnet
#'
#' @description Builds a network of aiRlayers objects with random weight and bias as a list
#'
#' @param nodes specifies how many nodes in each layer,
#' first value should equal input values, last value should
#' equal classification values
#'
#' @return generates aiRnet object of specified dimensions
#'
#' @export
aiRnet <- function(nodes) {
  n <- length(nodes)-1
  aiR <- vector("list",n)
  for(i in 1:n){
    aiR[[i]] <- aiRlayer(nodes[i],nodes[i+1])
  }
  names(aiR) <- paste0("layer",1:n)
  class(aiR) <- "aiRnet"
  return(aiR)
}

#' @name is.aiRnet
#'
#' @title is.aiRnet
#'
#' @param x object
#'
#' @return logical value
#'
#' @export
is.aiRnet <- function(x) inherits(x, "aiRnet")

#' @name is.aiRcost
#'
#' @title is.aiRcost
#'
#' @param x object
#'
#' @return logical value
#'
#' @export
is.aiRcost <- function(x) inherits(x,"aiRcost")

#' @name is.aiRactivation
#'
#' @title is.aiRactivation
#'
#' @param x object
#'
#' @return logical value
#'
#' @export
is.aiRactivation <- function(x) inherits(x, "aiRactivation")



#' @name is.aiR
#'
#' @title is.aiR
#'
#' @param x object
#'
#' @return logical value
#'
#' @export
is.aiR <- function(x) inherits(x, "aiR")
