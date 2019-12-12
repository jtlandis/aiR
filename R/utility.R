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
