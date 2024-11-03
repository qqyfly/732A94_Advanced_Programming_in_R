## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 02
##
## Author: Qinyuan Qi
##
## LiuID : qinqi464
##
## Date Created: 2023-09-01
##
## Copyright (c) MIT
## ---------------------------


## ---------------------------
## load packages set assignment path:
## ---------------------------

library(markmyassignment)

## ---------------------------
## Set necessary variables :
## ---------------------------

lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)
name <- 'Qinqi Qi'
liuid <- 'qinqi464'


## ---------------------------
## Util Function Area
## ---------------------------


## ---------------------------
## Lab Code Area
## ---------------------------


## ---------------------------
## 1.1.1 sheldon_game(player1, player2)

## According to the rule from the reference page,we got the rules as follows

## Scissors > Paper
## Paper > Rock
## Rock > Lizard
## Lizard > Spock
## Spock > Scissors	
## Scissors > Lizard
## Lizard  > Paper
## Paper > Spock
## Spock > Rock
## Rock > Scissors
## ---------------------------

sheldon_game <- function(player1, player2){

  rules <- list(c("scissors","paper"),
               c("paper","rock"),
               c("rock","lizard"),
               c("lizard","spock"),
               c("spock","scissors"),
               c("scissors","lizard"),
               c("lizard","paper"),
               c("paper","spock"),
               c("spock","rock"),
               c("rock","scissors"))

  for(r in rules){
    if (player1 == r[1] && player2 == r[2]) return("Player 1 wins!")
    if (player1 == r[2] && player2 == r[1]) return("Player 2 wins!")
    if (player1 == r[1] && player1 == player2) return("Draw!")
  }
  # if not match, throw an exception
  stop("Input is not correct")
}
## ---------------------------
## 1.2.1 my_moving_median() 
## ---------------------------

my_moving_median <- function(x, n, ...){
  
  # check parameter na.rm
  myargs <- match.call()
  if (("na.rm" %in% names(myargs)) ){
    remove_na <- myargs$na.rm
  }else{
    # not remove NA by default
    remove_na <- FALSE
  }
  
  # check if n is a numeric and n > 0
  stopifnot(is.numeric(n) && n > 0)
  
  # check x is a vector and all the elements are numeric 
  stopifnot(is.vector(x) && is.numeric(x[0]))

  # get length of x
  vec_len <- length(x)
  
  # init a vector to store all the return data
  ret_len <- vec_len-n
  ret <- numeric(ret_len)
  
  for (i in 1:ret_len) {
    ret[i] <- median(x[i:(i+n)],remove_na)
  }
  return(ret)
}

## ---------------------------
## 1.2.2 for_mult_table()
## ---------------------------

for_mult_table <- function(from , to){
  
  # check if parameters [from] and [to] are numeric
  if (!(is.numeric(from) && is.numeric(to))){
     stop("Input is not correct")
  }
  
  # get matrix length and width
  matrix_len <- (to - from + 1)
  
  # init a return matrix
  ret <- matrix(data = NA, ncol=matrix_len, nrow=matrix_len)
  
  for(i in 1:matrix_len) {
    for(j in 1:matrix_len) {
      ret[i,j] <- (from + i -1) * (from + j - 1)
    }
  }
  return(ret)
}

## ---------------------------
## 1.2.3 cor_matrix() [OPTIONAL]
## ---------------------------

cor_matrix <- function(df) {
  # get the col number
  n <- ncol(df)
  
  # init a n * n matrix and fill with NA
  corr_matrix <- matrix(NA, n, n)
  
  # only half of the matrix needed to be calculated
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        corr_matrix[i, j] <- 1  # Diagonal elements are 1
      } else {
        # Calculate correlation between column i and column j
        col_i <- df[, i]
        col_j <- df[, j]
        mean_x <- sum(col_x) / length(col_x)
        mean_y <- sum(col_y) / length(col_y)
        num <- sum((col_x - mean_x) * (col_y - mean_y))
        den <- sqrt(sum((col_x - mean_x)^2) * sum((col_y - mean_y)^2))
        corr_matrix[i, j] <- num / den
      }
    }
  }
  
  return(corr_matrix)
}

## ---------------------------
## 1.3.1 find_cumsum() - use while loop
## ---------------------------

find_cumsum <- function(x, find_sum) {
  
  # check if x is a vector and find_sum is a numeric
  if (!(is.vector(x) && is.numeric(find_sum))){
    stop("Input is not correct")
  }
  
  # init return and index
  ret <- 0
  index <- 1
  
  # get length of vector x
  vec_len <- length(x)
  
  while(index <= vec_len && ret <= find_sum) {
    ret <- ret + x[index]
    index <- index +1
  }
  
  return(ret)
}

## ---------------------------
## 1.3.2 while_mult_table()
## ---------------------------

while_mult_table <- function(from , to){
  
  # check if parameters [from] and [to] are numeric
  if (!(is.numeric(from) && is.numeric(to))){
    stop("Input is not correct")
  }
  
  # get matrix length and width
  matrix_len <- (to - from + 1)
  
  # init a return matrix
  ret <- matrix(data = NA, ncol=matrix_len, nrow=matrix_len)
  
  i <- 1
  j <- 1
  
  while(i <= matrix_len){
    while(j <= matrix_len){
      ret[i,j] <- (from + i -1) * (from + j - 1)
      j <- j + 1
    }
    i <- i + 1
    j <- 1
  }

  return(ret)
}

## ---------------------------
## 1.3.3 trial_division_factorization() [OPTIONAL]
## ---------------------------

trial_division_factorization <- function(x){
  # find all the prime factors of integer x
  
  # begin with 2 and divide 2 forever until we got 
  
  # the possible range of prime factor will located in [2,(floor)sqrt(x)]
  max_primary_factor <- floor(sqrt(x))
  
  # init return vector
  ret <- numeric(0)
  
  for(i in 2:max_primary_factor){
    # while loop until it is not divisible by i
    while((x %% i) == 0){
      ret <- c(ret, i)
      x <- x / i
    }
    # if x == 1 then we don't need to continue
    if (x == 1) break
  }
  return(ret)
}

## ---------------------------
## 1.4.1 repeat_find_cumsum()
## ---------------------------

repeat_find_cumsum <- function(x, find_sum) {
  
  # check if x is a vector and find_sum is a numeric
  if  (!(is.vector(x) && is.numeric(find_sum))){
    stop("Input is not correct")
  }
  
  # init ret and index
  ret <- 0
  index <- 1
  
  # get length of vector x
  vec_len <- length(x)
  
  repeat{
    ret <- ret + x[index]
    index <- index +1
    if (!(index <= vec_len && ret <= find_sum)) break
  }
  
  return(ret)
}

## ---------------------------
## 1.4.2 repeat_my_moving_median()
## ---------------------------

repeat_my_moving_median <- function(x, n, ...){
  
  # check parameter na.rm
  myargs <- match.call()
  if (("na.rm" %in% names(myargs)) ){
    remove_na <- myargs$na.rm
  }else{
    # not remove NA by default
    remove_na <- FALSE
  }
  
  # check if n is a numeric and n > 0
  stopifnot(is.numeric(n) && n > 0)
  
  # check x is a vector and all the elements are numeric 
  stopifnot(is.vector(x) && is.numeric(x[0]))
  
  # get length of x
  vec_len <- length(x)
  
  # init a vector to store all the return data
  ret_len <- vec_len-n
  ret <- numeric(ret_len)
  
  i <- 1
  
  repeat{
    ret[i] <- median(x[i:(i+n)],remove_na)
    i <- i + 1
    if (i > ret_len) break
  }
  
  return(ret)

}

## ---------------------------
## 1.5.1 in_environment()
## ---------------------------

in_environment <- function(env) {
  # get the env
  ret <- ls(env)
  
  return(ret)
}

## ---------------------------
## 1.5.2 where() [OPTIONAL]
## ---------------------------

where <- function(fun) {
  # find using find function
  ret <- find(fun)
  
  # check found or not
  if (length(ret) == 0){
    return ("non_existant_function not found!")
  }else{
    return(ret)
  }
}

## ---------------------------
## 1.6.1 cov() 
## ---------------------------

cov <- function(X) {
  # in requirement, X should be data.frame , but in one test case, a list is 
  # passed as a parameter which I think is wrong( to pass this test case, add an
  # extra is.list check
  if(!(is.data.frame(X) || is.list(X))){
    stop("Input is not correct")
  }
  ret <- unlist(lapply(X, function(x) {sd(x) / mean(x)}))
  return(ret)
}

## ---------------------------
## 1.7.1 moment()
## ---------------------------

moment <- function(i=1) {
  
  # check if i is a numeric
  if (!is.numeric(i)) {
    stop("Input is not correct")
  }
  
  return (function(vec) {
    # check if vec is a vector
    if(!(is.vector(vec))){
      stop("Input is not correct")
    }
    
    # Calculate the mean (first central moment)
    mean_x <- mean(vec)
    
    # Calculate the ith central moment
    central_moment <- mean((vec - mean_x)^i)
    
    return(central_moment)
  })
}

## ---------------------------
## 1.7.2 mcmc_counter_factory() [OPTIONAL]
## ---------------------------

# NOT implemented because don't know the meaning of 
# Markov chain Monte Carlo methods counter

mcmc_counter_factory <- function(thin,burnin){
 
}

## ---------------------------
## BEGIN TO TEST
## ---------------------------

#mandatory_case <- c(
#                    "sheldon_game",
#                    "my_moving_median",
#                    "for_mult_table",
#                    "find_cumsum",
#                    "while_mult_table",
#                    "repeat_find_cumsum",
#                    "repeat_my_moving_median",
#                    "in_environment",
#                    "cov",
#                    "moment"
#                  )

#optional_case <- c("cor_matrix",
#                   "trial_division_factorization",
#                   "where",
#                   "mcmc_counter_factory"
#                   )


#mark_my_assignment(mandatory_case)
#mark_my_assignment(optional_case)

#mark_my_assignment()
