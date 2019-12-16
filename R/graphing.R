#' @name layerfunc
#'
#' @title layerfunc
#'
#' @param layer aiRlayer object
#' @param layer.str aiRlayer name
layerfunc <- function(layer, layer.str){
  #browser()
  data <- as.data.frame(rbind(layer$weights, layer$bias))
  rownames(data)[nrow(data)] <- "bias"
  data$wb <- rownames(data)
  data$layer <- layer.str
  data <- tidyr::gather(data, key = "node", value = "value", -wb, -layer)
  return(data)
}

#' @name aiRnetdata
#'
#' @title aiRnetdata
#'
#' @param aiRnet aiRnet object
aiRnetdata <- function(aiRnet, layers = NULL){
  n <- length(aiRnet)
  if(is.null(layers)){
    layers <- 1:n
  }
  layername <- names(aiRnet)[layers]
  data <- layerfunc(aiRnet[[layername[1]]], layername[1])

  if(length(layers)>1){
    for(i in 2:length(layers)){
      work <- layerfunc(aiRnet[[layername[i]]], layername[i])
      data <- rbind(data, work)
    }
  }

  wb.n <- length(unique(data$wb))
  data$wb <- factor(data$wb, levels = c(paste0("weights",1:wb.n), "bias"))
  node.n <- length(unique(data$node))
  data$node <- factor(data$node, levels = c(paste0("node",node.n:1)))

  return(data)
}


#' @name plot.aiRnet
#'
#' @title plot aiRnet
#'
#' @description plots the weights and biases of each node in each layer of an aiRnet object
#'
#' @param aiRnet aiRnet object
#' @param layers non zero integer vector of layers to plot
#'
#' @return ggplot object
#'
#' @export
plot.aiRnet <- function(aiRnet, layers = NULL){
  n <- length(aiRnet)
  if(is.null(layers)){
    layers <- 1:n
  } else if(!(is.numeric(layers)&&all(layers%%1==0))){
    stop("layers must be a non zero integer vector or NULL")
  }
  if(any(layers<0)||any(layers>n)){
    stop(paste0("aiRnet has a maximum of ", n, "layers. Please assign numbers between [1,",n,"] to the \"layers\" argument"))
  }

  netdata <- aiRnetdata(aiRnet, layers = layers)

  vmin <- min(netdata$value)
  vmax <- max(netdata$value)
  gg <- ggplot(netdata, aes(x = wb, y = node, fill = value)) +
    geom_tile(color = "black") + facet_wrap(~layer, scales = "free") +
    scale_fill_gradientn(colours = c("#00079C","#1AE1D5","#FFFFFF", "#E3F925","#A10000"),
                         values = scales::rescale(c(vmin, vmin/2, 0, vmax/2, vmax))) +
    scale_x_discrete(expand = expand_scale()) +
    scale_y_discrete(expand = expand_scale()) +
    labs(x = "\nWeights & Biasses",y="Nodes\n") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", color = "black"))
  return(gg)
}


#' @name plot.aiRactivation
#'
#' @title plot aiRactivation
#'
#' @description plotting function for aiRactivation
#'
#' @param aiRactivation aiRactivation object
#' @param x.val column name of value to plot on x axis
#' @param y.val column name of value to plot on y axis
#' @param layer name of which layer in aiRactivation to use
#' @param nodes non zero vector of integers that specify which nodes to view.
#' by default all nodes are plotted.
#'
#' @return ggplot object
#'
#' @export
plot.aiRactivation <- function(aiRactivation, x.val, y.val, layer, nodes = NULL){
  #browser()
  data <- aiRactivation[["data"]]
  layer.str <- layer
  layer <- aiRactivation[[layer]]
  n <- ncol(layer)
  # if(invert){
  #   layer <- inverseSigmoid(layer)
  # }
  if(is.null(nodes)){
    nodes <- 1:n
  } else if(!(is.numeric(nodes)&&all(nodes%%1==0))){
    stop("nodes must be a non zero integer vector or NULL")
  }
  if(any(nodes<0)||any(nodes>n)){
    stop(paste0(layer.str, "has a maximum of ", n, "nodes. Please assign numbers between [1,",n,"] to the \"nodes\" argument"))
  }
  ggdat <- cbind(data[,c(x.val,y.val)],layer[,nodes])
  ggdat <- tidyr::gather(ggdat, key = "node", value = "activation", -x.val, -y.val)
  gg <- ggplot(ggdat, aes_string(x = x.val, y = y.val)) +
    geom_point(aes(color = activation)) +
    scale_color_gradientn(colors = c("orange", "white","blue"), limits = c(0,1)) +
    facet_wrap(~node) +
    theme_classic() +
    theme(strip.text = element_text(face = "bold"))
  return(gg)
}


#' @name plot.aiR
#'
#' @title plot aiR
#'
#' @description plotting the cost of the aiR object over all cycles
#'
#' @param aiRcost aiR object
#'
#' @return ggplot object
#'
#' @export
plot.aiR <- function(aiR){
  dat <- aiR$Cost
  dat <- na.exclude(tidyr::gather(dat, key = "cost.type", value = "cost.value", -cycles))
  gg <- ggplot(data = dat, aes(x = cycles, y = cost.value, color = cost.type)) +
    geom_point() +
    scale_y_log10() +
    labs(x = "\nCycles", y = "Cost Value\n", color = "Cost Type") +
    theme_classic() +
    theme(strip.text = element_text(face = "bold"))
  return(gg)
}
