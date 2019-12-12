


aiRrun2 <- function(data, aiRnet, classif, cycles, batch.size){
  #browser()
  #subdivide into batches
  batches <- aiRbatch(data = data, batch.size = batch.size)
  nb <- length(batches)
  #browser()
  tot.cost <- numeric(cycles)
  aiRbest <- aiRnet
  min.cost <- batch.size*ncol(data)
  for(i in 1:cycles){
    if(i%%nb==0){
      b <- batches[[nb]]
      batches <- aiRbatch(data = data, batch.size = batch.size)
    } else {
      b <- batches[[i%%nb]]
    }
    #find activation
    aiRactiv <- activation(data = data[b,], aiRnet = aiRnet)
    n <- length(aiRactiv)
    # find cost
    aiRloss <- aiRloss2(data = aiRactiv[[n]], class.levels = levels(classif), classify = classif[b])
    tot.cost[i] <- aiRloss$total.cost
    if(tot.cost[i]==min(tot.cost[i],min.cost)){
      aiRbest <- aiRnet
    }
    newaiR <-  backprop(aiRnet = aiRnet, aiRloss2 = aiRloss, aiRactivation = aiRactiv)
    aiRnet <- aiRfresh2(aiRnet = newaiR)
  }
  d <- data.frame(cycles = 1:cycles, cost = tot.cost)
  #ggplot(data = NULL, aes(x = 1:cycles, y = tot.cost)) + geom_point()
  ret <- list(aiRnet,aiRbest, aiRloss, d)
  names(ret) <- c("aiRnet","aiRbest","aiRloss","Cost")
  return(ret)
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


activation <- function(aiRnet, data) {
  #
  if(!is.aiRnet(aiRnet)){
    stop("aiRnet must be of class \"aiRnet\"")
  }
  n <- length(aiRnet)

  l <- list(n+1)
  l[[1]] <- data
  for(i in 2:(n+1)) {   #Transform data through aiRnet
    data <- as.matrix(data)%*%aiRnet[[i-1]]$weights
    data <- mat.opperation(x = data, y = aiRnet[[i-1]]$bias, opperation = "+")
    data <- sigmoid(data)
    l[[i]] <- data
  }
  l[[n+1]] <- apply(l[[n+1]],2,zero)
  return(l)

}
#making good progress
#Need to get recursive part down and decide how to with accumelating sums...
#after first iteration, what constitutes as a cost value?
#reference: https://www.youtube.com/watch?v=tIeHLnjs5U8

aiRloss2 <- function(data, class.levels, classify) {
  class.mat <- diag(nrow = length(class.levels),ncol = length(class.levels))
  correct <- class.mat[classify,]
  cost <- (data-correct)^2
  dcost <- 2*(data-correct)
  dsig <- dsigmoid(data)
  if(is.null(dim(cost))){
    obs.cost <- sum(cost)
  } else {
    obs.cost <- apply(cost,1,sum)
  }
  total.cost <- sum(obs.cost)
  ret <- list(cost, dcost, dsig, obs.cost, total.cost)
  names(ret) <- c("cost","dcost","dsig","obser.cost","total.cost")
  class(ret) <- "aiRloss"
  return(ret)
}

backprop <- function(aiRnet, aiRloss2, aiRactivation) {
  #
  n <- length(aiRactivation)
  n.obs <- nrow(aiRactivation[[n]])
  gencost <- aiRloss2$dcost*aiRloss2$dsig
  # if(is.null(dim(gencost))){
  #   cost.vec <- sum(gencost)
  # } else {
  #   cost.vec <- apply(gencost,1,sum)
  # }
 # cost.vec <- cost.vec*scales::rescale(abs(cost.vec), c(1,2))
  deltaW <- t(aiRactivation[[n-1]])%*%gencost #/n.obs
  deltaA <- gencost%*%t(aiRnet[[n-1]]$weights)
  deltaB <- apply(gencost,2, sum)
  # start.adjust <- aiRactivation[[n-1]]*cost.vec
  aiRnet[[n-1]]$change.w <- deltaW
  aiRnet[[n-1]]$change.b <- deltaB
  if(n>2){
    for(i in (n-1):2){
      gencost <- deltaA*dsigmoid(aiRactivation[[i]])
      # if(is.null(dim(gencost))){
      #   cost.vec <- sum(gencost)
      # } else {
      #   cost.vec <- apply(gencost,1,sum)
      # }
     # cost.vec <- cost.vec*scales::rescale(abs(cost.vec), c(1,2))
      # start.adjust <- aiRactivation[[i-1]]*cost.vec
      deltaW <- t(aiRactivation[[i-1]])%*%gencost #/n.obs
      deltaA <- gencost%*%t(aiRnet[[i-1]]$weights)
      deltaB <- apply(gencost,2, sum)
      aiRnet[[i-1]]$change.w <- deltaW #ideally we wouldnt do the apply
      aiRnet[[i-1]]$change.b <- deltaB
    }
  }
  return(aiRnet)
}

aiRfresh2 <- function(aiRnet){
  n <- length(aiRnet)
  for(i in 1:n){
    aiRnet[[i]]$weights <- aiRnet[[i]]$weights - aiRnet[[i]]$change.w
    aiRnet[[i]]$bias <- aiRnet[[i]]$bias - aiRnet[[i]]$change.b
    aiRnet[[i]]$change.w <- aiRnet[[i]]$change.w*0
    aiRnet[[i]]$change.b <- aiRnet[[i]]$change.b*0
  }
  return(aiRnet)
}


aiRrun2_test <- function(data, aiRnet, classif, cycles = 100){
  browser()
  tot.cost <- numeric(cycles)
  aiRbest <- aiRnet
  min.cost <- nrow(data)*ncol(data)
  for(i in 1:cycles){

    #find activation
    aiRactiv <- activation(data = data, aiRnet = aiRnet)
    n <- length(aiRactiv)
    # find cost
    aiRloss <- aiRloss2(data = aiRactiv[[n]], class.levels = levels(classif), classify = classif)
    tot.cost[i] <- aiRloss$total.cost
    if(tot.cost[i]==min(tot.cost[i],min.cost)){
      aiRbest <- aiRnet
    }
    newaiR <-  backprop(aiRnet = aiRnet, aiRloss2 = aiRloss, aiRactivation = aiRactiv)
    aiRnet <- aiRfresh2(aiRnet = newaiR)
  }
  d <- data.frame(cycles = 1:cycles, cost = tot.cost)
  #ggplot(data = NULL, aes(x = 1:cycles, y = tot.cost)) + geom_point()
  ret <- list(aiRnet,aiRbest, aiRloss, d)
  names(ret) <- c("aiRnet","aiRbest","aiRloss","Cost")
  return(ret)
}


A <- function(data, aiRnet, classif, cycles = 100){
  browser()
  tot.cost <- numeric(cycles)
  aiRbest <- aiRnet
  min.cost <- nrow(data)*ncol(data)
  for(i in 1:cycles){
    newaiR <- aiRnet
    for(j in 1:nrow(data)){
      #find activation
      aiRactiv <- activation(data = data[j,], aiRnet = newaiR)
      n <- length(aiRactiv)
      # find cost
      aiRloss <- aiRloss2(data = aiRactiv[[n]], class.levels = levels(classif), classify = classif[j])
      tot.cost[i] <- sum(aiRloss$total.cost,tot.cost[i])
      # if(tot.cost[i]==min(tot.cost[i],min.cost)){
      #   aiRbest <- aiRnet
      # }
      newaiR <-  backpropA(aiRnet = newaiR, aiRloss2 = aiRloss, aiRactivation = aiRactiv)
    }
  aiRnet <- aiRfresh2(aiRnet = newaiR)
  }
  d <- data.frame(cycles = 1:cycles, cost = tot.cost)
  #ggplot(data = NULL, aes(x = 1:cycles, y = tot.cost)) + geom_point()
  ret <- list(aiRnet,aiRbest, aiRloss, d)
  names(ret) <- c("aiRnet","aiRbest","aiRloss","Cost")
  return(ret)
}

#Need to think about deltaw and deltaactivation

backpropA <- function(aiRnet, aiRloss2, aiRactivation) {
  #
  n <- length(aiRactivation)
  gencost <- aiRloss2$dcost*aiRloss2$dsig
  if(is.null(dim(gencost))){
    cost.vec <- sum(gencost)
  } else {
    cost.vec <- apply(gencost,1,sum)
  }
  # cost.vec <- cost.vec*scales::rescale(abs(cost.vec), c(1,2))
  start.adjust <- aiRactivation[[n-1]]*cost.vec
  aiRnet[[n-1]]$change.w <- aiRnet[[n-1]]$change.w + aiRnet[[n-1]]$weights*apply(start.adjust,2,sum) #ideally we wouldnt do the apply
  aiRnet[[n-1]]$change.b <- aiRnet[[n-1]]$change.b + sum(cost.vec)*aiRnet[[n-1]]$bias
  if(n>2){
    for(i in (n-1):2){
      gencost <- start.adjust*sigmoid(aiRactivation[[i]])
      if(is.null(dim(gencost))){
        cost.vec <- sum(gencost)
      } else {
        cost.vec <- apply(gencost,1,sum)
      }
      # cost.vec <- cost.vec*scales::rescale(abs(cost.vec), c(1,2))
      start.adjust <- aiRactivation[[i-1]]*cost.vec
      aiRnet[[i-1]]$change.w <- aiRnet[[i-1]]$change.w + aiRnet[[i-1]]$weights*apply(start.adjust,2,sum) #ideally we wouldnt do the apply
      aiRnet[[i-1]]$change.b <- aiRnet[[i-1]]$change.b + sum(cost.vec)*aiRnet[[i-1]]$bias
    }
  }
  return(aiRnet)
}
