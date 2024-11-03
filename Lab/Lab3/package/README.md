# 732A94 Advanced Programming in R Lab3
732A94 Advanced Programming in R Lab3 Homework

<!-- badges: start -->
[![R-CMD-check](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab3/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`lab03`   is a homework assignment of advanced R programming.

## Features

* Euclidian function provides the GCD of two numbers
* Dijkstra Function calculates the shortest path from the starting node to other remaining nodes.
* 
## Installation

install.packages("lab03")

## Usage

Use the sample below as a reference or check man file for more information.

```r
library(lab03)

euclidean(123612, 13892347912)

wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)

```
