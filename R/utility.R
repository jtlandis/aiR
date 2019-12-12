#' @name sigmoid
#'
#' @title sigmoid
#'
#' @param x numeric vector
#'
#' @return sigmoid value for each x
#'
#' @export
sigmoid <- function(x) {
  (1 / (1 + exp(-x)))
}

#' @name dsigmoid
#'
#' @title dsigmoid
#'
#' @param x numeric vector
#'
#' @return first dirivative of sigmoid for each x
dsigmoid <- function(x){
  sigmoid(x)*(1-sigmoid(x))
}

#' @name zero
#'
#' @description Numbers under threshold are assigned numeric zero. Threshold default equals 0.00001
#'
#' @title zero
#'
#' @param x numeric vector to turn to zero
#' @param power the power associated. default = -5
#' @param base Base of the exponent. default = 10
#' @param coef coefficent of the power and base. default = 1
#'
#' @export
zero <- function(x, power = -5, base = 10, coef = 1){
  val <- coef*(base^(power))
  x <- ifelse(abs(x)<val,0,x)
  return(x)
}

#' @name mat.opperation
#'
#' @title mat.opperation
#'
#' @description allows simple operations between matrix and constant vector. column size of x must equal length of y
#'
#' @param x matrix of column size m, row size n
#' @param y vector of length m
#' @param opperation opperation to use: +, -, *, /
#'
#' @return returns n by m matrix where values of y are done onto the columns of x.
#'
#' @export
mat.opperation <- function(x,y, opperation){
  type.opperation <- c("+","-","*","/")
  if(!is.element(opperation, type.opperation)) {
    stop("train.method must be \"+\" or \"-\" or \"*\" or \"/\"")
  }
  if(ncol(x)!=length(y)) {
    stop("column of x must equal length of y.")
  }
  if(opperation=="+"){
    mat <- sweep(x, 2, y, "+")
  } else if(opperation=="-"){
    mat <- sweep(x, 2, y, "-")
  } else if(opperation=="*"){
    mat <- sweep(x, 2, y, "*")
  } else if(opperation=="/"){
    mat <- sweep(x, 2, y, "/")
  }
  return(mat)
}



#' @name aiRrate
#'
#' @title aiRrate
#'
#' @description reports error rate of network
#'
#' @param data data of input values
#' @param factor character vector of factors to classify. levels must be in same order as
#' levels presented in original data.
#' @param aiRnet aiRnet object
#' @param report.class report results from aiRclassify, Default set to FALSE
#' @param warning Suppress warning from aiRclassify that multiple nodes are activated.
#' Default set to TRUE. TRUE: allow warning. FALSE: Suppress warning
#'
#' @return error rate of aiRnet on data and how many activated non uniquely.
#'
#' @export
aiRrate <- function(data, factor, aiRnet, report.class = FALSE, warning = TRUE) {
  if(!is.aiRnet(aiRnet)){
    stop("aiRnet must be of class \"aiRnet\"")
  }
  if(!is.element(report.class, c(TRUE,FALSE))){
    stop("report.class must be logical, either: TRUE or FALSE")
  }

  class.save <- data[[factor]]
  data <- data[,!colnames(data)%in%factor]
  classify <- aiRclassify(data = data, factor = levels(class.save), aiRnet = aiRnet, warning = warning)
  rate <- 1-(mean(class.save==classify$classify))
  if(report.class) {
    ret <- list(rate, classify$classify, classify$node.max, classify$failed.instances)
    names(ret) <- c("MeanError","classify","node.max", "failed.instances")
  } else {
    ret <- list(rate, classify$failed.instances)
    names(ret) <- c("MeanError","failed.instances")
  }
  return(ret)
}

#' @name aiRclassify
#'
#' @title aiRclassify
#'
#' @description classifies each observation by taking the max value of the last layer.
#'
#' @param data data of input values
#' @param factor character vector of factors to classify. levels must be in same order as
#' levels presented in original data.
#' @param aiRnet aiRnet object
#' @param warning Suppress warning from aiRclassify that multiple nodes are activated.
#' Default set to TRUE. TRUE: allow warning. FALSE: Suppress warning
#'
#' @return returns a list of classification values for each
#' observation along with its activation in the last layer.
#'
#' @export
aiRclassify <- function(data, factor, aiRnet, warning = TRUE) {
  if(!is.aiRnet(aiRnet)){
    stop("aiRnet must be of class \"aiRnet\"")
  }
  trans <- aiRtransform(data = data, aiRnet = aiRnet)
  node.max <- apply(trans, 1, max)
  indexes <- apply(trans==node.max, 1, which)
  if(class(indexes)=="list") {
    if(warning) {
      warning("for at least one observation, more than one node have the same max activation. More training advised.")
    }
    classify <- unlist(lapply(lapply(indexes,FUN = function(x, y = factor) {y[x]}),str_flatten))
    fails <- sum(!classify %in% factor)
  } else {
    classify <- factor[indexes]
    fails <- 0
  }

  ret <- list(classify, node.max, fails)
  names(ret) <- c("classify","node.max", "failed.instances")
  return(ret)
}
