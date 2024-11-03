## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 03
##
## Date Created: 2023-09-05
##
## Copyright (c) MIT
## ---------------------------
#' Function to find the greatest common divisor of two numbers.
#' This function uses Euclidian algorithm to calculate a GCD of 2 numbers(x and y) 
#'
#' Author: Satya Sai Naga Jaya Koushik Pilla
#' @param x Integer
#' @param y Integer  
#' @return gcd of x ad y
#' @export 
euclidean <- function(x, y){
  while (y != 0) {
    remainder <- x %% y
    x <- y
    y <- remainder
  }
  return(x)
}


#' Function to calculate the shortest path from the initial node to other node 
#' in the graph
#' 
#' This function implements the Dijkstras algorithm and finds the shortest distance 
#' from the initial node to every other node in the graph
#' Author: Qinyuan QI
#' @param graph An data frame store the edge information
#' @param init_node Init node number
#' @return An vector that stores the distance 
#' @export
dijkstra <- function(graph, init_node) {

  # we get the max of graph data frame elements(v1 and v2) to get the number of nodes
  # For example, if max=6, but in the real graph, there exists orphan nodes, in the case
  # of orphan node id > max value, this case will be ignored, but if orphan node id <= max
  # then this one will still be considered, and the length from init_node to it will be INF
  # which is the init value.
  node_num <- max(max(graph$v1),max(graph$v2))
  
  len <- length(graph$w)
  
  # init the cost matrix with Inf then assign the distance value using the values provided
  # in graph['w']
  cost <- matrix(Inf, node_num, node_num)
  for(i in 1:len){
    cost[graph$v1[i],graph$v2[i]] <- graph$w[i]
  }
  
  # init the visited vector to FALSE
  visited <- rep(FALSE, times = node_num)
  
  # init distance
  dist <- rep(Inf, times = node_num)
  
  for(i in 1:node_num){
    dist[i] <- cost[init_node, i];
  }

  dist[init_node] <- 0
  
  for(i in 1:(node_num-1)) {
    minDist <- Inf
    p <- 0
    # find a unvisited node which has min distance to the init node
    for(j in 1:node_num){
      if ((j != init_node) && (!visited[j]) && (dist[j] < minDist)){
        minDist <- dist[j]
        p <- j
      }
    }
    
    visited[p] <- TRUE
    
    for(j in 1:node_num){
      if (j == p || cost[p,j] == Inf){
        next
      }
      if (dist[p] + cost[p,j] < dist[j]){
        dist[j] <- dist[p] + cost[p,j]
      }
    }
    
  }
  
  return(dist)
}


