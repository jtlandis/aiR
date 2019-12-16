dsigmoid_dx <- function(x) {
  x <- sigmoid(x)*(1-sigmoid(x))
  return(x)
}


first <- function(loss, aiRnet, rate = 100, method = "fast") {
  n <- length(aiRnet)
  if(method == "fast") {
    if(class(aiRnet[[n]])=="aiRhidden"){
      hidden.len <- lapply(aiRnet, length)
      g <- length(aiRnet[[n]])
      mean.node <- vector("numeric",g)
      for(i in 1:g) {
        mean.node[i] <- aiRnet[[i]][[hidden.len[i]]]$mean.node
      }
      gradiant <- loss*mean.node*(1-mean.node)*mean.node
      for(i in 1:g) {
        aiRnet[[i]][[hidden.len[i]]]$change.w <- mat.opperation(x = aiRnet[[i]][[hidden.len[i]]]$change.w, y = -1*rate*gradiant[i], opperation = "+")
      }
      for(i in n:1) {
        output <- second(aiRinner = aiRnet[[i]], gradiant = sum(gradiant), rate = rate, method = method)
        aiRnet[[i]] <- output[[1]]
        gradiant <- output[[2]]
      }
    } else if(class(aiRnet[[n]])=="aiRlayer"){
      gradiant <- loss*aiRnet[[n]]$mean.node*(1-(aiRnet[[n]]$mean.node))*aiRnet[[n]]$mean.node  # THIS WILL NEED TO BE CHANGED FOR METHOD.PROPIGATEE == SLOW .... Will not use mean.node.
      aiRnet[[n]]$change.w <- mat.opperation(aiRnet[[n]]$change.w,y = -1*rate*gradiant, opperation = "+")
      aiRnet[[n]]$change.b <- -1*rate*gradiant 
    }
    for(i in (n-1):1) {
      output <- second(aiRinner = aiRnet[[i]], gradiant = sum(gradiant), rate = rate, method = method)
      aiRnet[[i]] <- output[[1]]
      gradiant <- output[[2]]
    }
  } else if (method == "slow") {
    if(class(aiRnet[[n]])=="aiRhidden"){
      hidden.len <- lapply(aiRnet, length)
      g <- length(aiRnet[[n]])
      mean.node <- vector("numeric",g)
      for(i in 1:g) {
        mean.node[i] <- aiRnet[[i]][[hidden.len[i]]]$mean.node
      }
      gradiant <- loss*mean.node*(1-mean.node)*mean.node
      for(i in 1:g) {
        aiRnet[[i]][[hidden.len[i]]]$change.w <- mat.opperation(x = aiRnet[[i]][[hidden.len[i]]]$change.w, y = -1*rate*gradiant[i], opperation = "+")
      }
      for(i in n:1) {
        output <- second(aiRinner = aiRnet[[i]], gradiant = sum(gradiant), rate = rate, method = method)
        aiRnet[[i]] <- output[[1]]
        gradiant <- output[[2]]
      }
    } else if(class(aiRnet[[n]])=="aiRlayer"){
      aiRoj <- aiRtransform(data = data, aiRnet = aiRnet, n = n)
      gradiant <- loss*aiRoj*(1-aiRoj)*aiRoj  # THIS WILL NEED TO BE CHANGED FOR METHOD.PROPIGATEE == SLOW .... Will not use mean.node.
      aiRnet[[n]]$change.w <- mat.opperation(aiRnet[[n]]$change.w,y = apply(-1*rate*gradiant,2,mean), opperation = "+")
      aiRnet[[n]]$change.b <- -1*rate*gradiant 
    }
    for(i in (n-1):1) {
      gradiant <- mat.opperation(x = gradiant, y = apply(aiRnet[[i+1]]$weights,1,sum), opperation = "*")
      output <- second(aiRinner = aiRnet[[i]], gradiant = gradiant, rate = rate, method = method, n = i, aiRnet = aiRnet)
      aiRnet[[i]] <- output[[1]]
      gradiant <- output[[2]]
    }
  } else {
    stop("Error: method.propigate must be either \"fast\" or \"slow\"  \n")
  }
  return(aiRnet)
}

second <- function(aiRinner,gradiant, rate = 100, method = "fast", ...) {UseMethod("second",aiRinner)}

second.aiRlayer <- function(aiRinner,gradiant, rate, method = "fast", n = NULL, aiRnet = NULL, ...) {
  if(method=="fast"){
    gradiant <- aiRinner$mean.node*(1-(aiRinner$mean.node))*aiRinner$mean.node*gradiant
    aiRinner$change.w <- mat.opperation(aiRinner$change.w,y = -1*rate*gradiant, opperation = "+")
    aiRinner$change.b <- -1*rate*gradiant 
  } else if(method=="slow") {
    aiRoj <- aiRtransform(data = data, aiRnet = aiRnet, n = n)
    gradiant <- aiRoj*(1-aiRoj)*aiRoj*gradiant
    aiRinner$change.w <- mat.opperation(aiRinner$change.w,y = -1*rate*gradiant, opperation = "+")
    aiRinner$change.b <- -1*rate*gradiant
  }
  return(list(aiRinner,gradiant))
}
second.aiRnet <- function(aiRinner,gradiant,rate, ...) {
  n <- length(aiRinner)
  for(i in n:1) {
    output <- second(aiRinner = aiRinner[[i]], gradiant = sum(gradiant), rate = rate)
    aiRinner[[i]] <- output[[1]]
    gradiant <- output[[2]]
  }
  return(list(aiRinner,gradiant))
}
second.aiRhidden <- function(aiRinner,gradiant,rate, ...) {
  output <- lapply(aiRinner,second, gradiant = gradiant, rate = rate)
  aiRinner <- lapply(output,getlayer, layer = 1)
  gradiant <- unlist(lapply(output,getlayer, layer = 2))
  return(list(aiRinner,gradiant))
}

getlayer <- function(x,layer) {
  return(x[[layer]])
}