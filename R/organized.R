#' @name aiRsubset
#'
#' @title aiRsubset
#'
#' @param x input
#' @param sample.method Method to train data. "Sample" to take a sample of values in data. "Factor"
#' @param sample.size Number between (0-1) that modifies how much of training data is used.
#' @param sample.Factor Necessary when sample.method set to "Factor". Assign as vector of length 2 in the following form
#' c("column.index/name","factor.level"). Can assign only column name or index but first level is chosen in this case.
#'
#' @return Row numbers to subset
aiRsubset <- function(x, sample.method, sample.size, sample.Factor = NULL) {
  type.sample.method <- c("Sample","Factor")
  if(!is.element(sample.method, type.sample.method)) {
    stop("sample.method must be \"Sample\" or \"Factor\"")
  }
  total.length <- nrow(x)
  if(sample.method == "Sample") {
    n <- ceiling(nrow(x)*sample.size)
    sample.rows <- sample(as.numeric(row.names(x)),n, replace = F)
  } else if(sample.method == "Factor") {
    if(is.null(sample.Factor)) {
      stop("sample.Factor must be assigned index or column name when sample.method is set to \"Factor\"")
    }
    factor.vec <- x[[sample.Factor[1]]]
    if(!is.factor(factor.vec)) {
      stop(paste("is.factor returned FALSE on column named \"",sample.Factor[1],"\". Change column to a factor.",sep = ""))
    }
    if(length(sample.Factor)==1) {
      warning("Only the factor column was specified in sample.Factor. Assuming first level of training vector is training set.")
      x <- x[factor.vec%in%levels(factor.vec)[1],]
      n <- round(nrow(x)*sample.size,0)
      sample.rows <- sample(as.numeric(row.names(x)),n)
    } else if(length(sample.Factor)==2) {
      if(!any(sample.Factor[2] %in% levels(factor.vec))) {
        stop(paste(sample.Factor[2]," is not a level of indicated column vector.", sep = ""))
      }
      if(!any(sample.Factor[2] %in% factor.vec)) {
        stop(paste(sample.Factor[2]," is not contained in indicated column vector.", sep = ""))
      }
      x <- x[factor.vec%in%sample.Factor[2],]
      n <- round(nrow(x)*sample.size,0)
      sample.rows <- sample(as.numeric(row.names(x)),n)
    } else {
      stop("sample.Factor must either be length 1 or 2.")
    }
  }
  sub <- seq(1,total.length) %in% sample.rows
  return(sub)
}


#' @name aiRbatch
#'
#' @title aiRbatch
#'
#' @description randomly splits data into batches through sample. No repeated samples.
#'
#' @param data data of input values
#' @param batch.size how many observations are used per batch
#'
#' @return a list of data.frames containing random samples of input data.
aiRbatch <- function(data, batch.size) {
  #
  n <- nrow(data)
  n.batch <- floor(n/(batch.size))
  batches <- vector("list",n.batch)
  batch.names <- paste("batch.",seq(1:n.batch),sep = "")
  rows <- seq(1:n)
  rows.work <- rows
  available.rows <- !logical(n)
  for(i in 1:n.batch) {
    sample.row <- sample(x = rows.work[available.rows], size = batch.size)
    batches[[i]] <- rows %in% sample.row
    available.rows <- available.rows&!(rows %in% sample.row)
  }
  names(batches) <- batch.names
  return(batches)
}
