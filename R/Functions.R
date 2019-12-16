

#' @import stringr





#' @name aiRtransform
#'
#' @title aiRtransform
#'
#' @param data data of input values
#' @param aiRnet aiRnet object
#' @param n Which layer to transform to.
#'
#' @return returns aiRnet output of data
#'
#' @export
aiRtransform <- function(data, aiRnet, n = NULL) {
  if(!is.aiRnet(aiRnet)){
    stop("aiRnet must be of class \"aiRnet\"")
  }
  if(is.null(n)) {
    n <- length(aiRnet)
  }
  for(i in 1:n) {   #Transform data through aiRnet
    data <- as.matrix(data)%*%aiRnet[[i]]$weights
    data <- mat.opperation(x = data, y = aiRnet[[i]]$bias, opperation = "+")
    data <- sigmoid(data)
  }
  data <- apply(data,2,zero)
  return(data)
}

#' @name merge_multi
#'
#' @title merge_multi
#'
#' @description extends merge function to 3 or more vectors
#'
#' @param ... vectors to be merged. argument names become column names in returned value.
#' If no arguments specified, default column names set to LETTERS set.
#'
#' @return data frame of combinded vectors.
#'
#' @examples
#'
#' merge_multi(x = c(1,2,3), y = c(4,5,6), z = c(7,8,9))
#' merge_multi(c(1,2,3),c(4,5,6),c(7,8,9))
#'
#' @export
merge_multi <- function(...) {
  z <- list(...)
  modes <- mode.type(z)
  if(is.null(names(z))) {
    name <- letters[seq(1,length(z))]
  } else {
    name <- names(z)
  }
  z.new <- interaction(merge(z[[1]],z[[2]]),sep = ",")
  if(length(z)>2) {
    for(i in 3:(length(z))) {
      z.new <- interaction(merge(z.new,z[[i]]),sep = ",")
    }
  }
  z.new <- as.data.frame(z.new)
  colnames(z.new) <- "combind"
  new <- separate(z.new,col = "combind",into = name,sep = ",")
  new <- correct.mode(df = new,mode.vec = modes)
  return(new)
}

#' @name aiRdevelop
#'
#' @title aiRdevelop
#'
#' @param data Data frame that contains all named columns needed
#' @param var.classify index or column name of vector that contains classifying values
#' @param aiRnet aiRnet object
#' @param train.method Method to save internal subset of data as the training data. "Sample" to take
#'  a random sample of all rows in data as training set. "Factor" to indicate if you are training
#'  on rows containing a particular factor level.
#' @param sample.size Number between (0-1) that modifies how much of desired train.method data is used.
#' Default set to 0.5. Excluded rows will be used as test examples and not affect aiRaiRnet.
#' @param batch.size Number indicating how many rows to make batches from training sample. Default set to "all"
#' for no batches to be made.
#' @param train.Factor Necessary when train.method set to "Factor". Assign as vector of length 2 in the following form
#' c("column.index/name","factor.level"). Can assign only column name or index but first level is chosen in this case.
#' @param steps Number of classification captures desired to view
#' @param range.size total amount of data points used to visualize input space.
#' @param na.rm remove NAs, default set to TRUE. Function likely to fail with NAs
#'
#' @return tidy data frame of the classification and max.node at each step
#'
#' @export
aiRdevelop <- function(data,
                       var.classify,
                       aiRnet,
                       train.method = "Sample",
                       sample.size = .5,
                       batch.size,
                       train.Factor = NULL,
                       steps = 15,
                       range.size = 1000) {
  if(!is.aiRnet(aiRnet)){
    stop("aiRnet must be of class \"aiRnet\"")
  }
  n <- floor(floor(nrow(data)*sample.size)/batch.size)
  index <- index.o.coln(vec = var.classify, v.size = 1, v.name = "var.classify", name.col = colnames(data))
  data.n.class <- data[,-index]
  space.var <- aiRactivation(data = data.n.class, aiRnet = aiRnet, range.size = range.size)
  space.var <- space.var$data_model
  space.var.save <- space.var
  m.t <- mode.type(space.var)
  for(i in 1:steps) {
    if(i==1){
      step.loss <- aiRrun(data = data, var.classify = var.classify, train.method = train.method,aiRnet = aiRnet, cycles = n, sample.size = sample.size, batch.size = batch.size, train.Factor = train.Factor)
    } else {
      step.loss <- aiRrun(data = data, var.classify = var.classify, train.method = train.method,aiRnet = step.loss$aiRnet, cycles = n, sample.size = sample.size, batch.size = batch.size)
    }
    new <- aiRclassify(data = space.var.save, factor = levels(data[,index]),aiRnet = step.loss$aiRnet)
    step <- interaction(new$classify, new$node.max, sep = "_break_")
    eval(parse(text = paste("space.var$step",i," <- step", sep = "")))
  }

  space.var <- melt(space.var, id.vars = commoncol(space.var, data.n.class), variable.name = "step")
  space.var <- separate(data = as.data.frame(space.var), col = "value", into = c("classify","node.max"),sep = "_break_")
  space.var <- correct.mode(space.var, mode.vec = c(m.t,"Factor","Factor","num"))
  space.var$step <- factor(space.var$step, levels=c(paste("step",1:steps, sep = "")))

  # gg <- ggplot(data = space.var, aes(x = x, y = y, color = classify, alpha = node.max,frame = step)) +
  #   geom_point()
  #
  # gganimate(gg)
  return(space.var)
}



