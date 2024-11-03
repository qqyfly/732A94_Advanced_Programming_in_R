## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 01
##
## Author: Qinyuan Qi
##
## LiuID : qinqi464
##
## Date Created: 2023-08-31
##
## Copyright (c) MIT
## ---------------------------


## ---------------------------
## load packages set assignment path:
## ---------------------------

library(markmyassignment)

## ---------------------------
## Set necessary variables
## ---------------------------

lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)
name <- 'Qinqi Qi'
liuid <- 'qinqi464'

## ---------------------------
## Code Area
## ---------------------------

## ---------------------------
## 1.1.1 my_num_vector()
## ---------------------------

my_num_vector <- function(){
  ret <- c(log10(11),cos(pi/5),exp(pi/3),((1173 %% 7) / 19))
  return(ret)
}


## ---------------------------
## 1.1.2 filter_my_vector(x, leq)
## ---------------------------

filter_my_vector <- function(x, leq) {
  ret <- ifelse(x<leq, x, NA)
  return(ret)
}

## ---------------------------
## 1.1.3 dot prod(a, b)
## ---------------------------

dot_prod <- function(a,b){
  ret <- as.integer(a %*% b)
  return(ret)
}

## ---------------------------
## 1.1.4 approx_e(N)
## ---------------------------

approx_e <- function(N){
  ret <- 0
  for(i in 0:N){
    ret <- ret + (1 / factorial(i))
  }
  return(ret)
}

## ---------------------------
## 1.2.1 my_magic_matrix()
## ---------------------------

my_magic_matrix <- function(){
  ret <- matrix(data=c(4,3,8,9,5,1,2,7,6),nrow=3,ncol=3,byrow=FALSE)
  return(ret)
}

## ---------------------------
## 1.2.2 calculate_elements(A)
## ---------------------------

calculate_elements <- function(A){
  return(length(A))
}

## ---------------------------
## 1.2.3 row_to_zero(A,i) 
## ---------------------------

row_to_zero <- function(A,i) {
  ret <- A
  ret[i,] <- 0
  return (ret)
}

## ---------------------------
## 1.2.4 add_elements_to_matrix(A, x, i, j)
## ---------------------------

add_elements_to_matrix <- function(A, x, i, j){
  ret <- A
  ret[i,j] <- ret[i,j] + x
  return(ret)
}


## ---------------------------
## 1.3.1 my_magic_list()
## ---------------------------

my_magic_list <- function() {
  ret <- list(info="my own list",my_num_vector(),my_magic_matrix())
  return(ret)
}

## ---------------------------
## 1.3.2 change_info(x, text)
## ---------------------------

change_info <- function(x, text){
  ret <- x
  ret$info <- text
  return(ret)
}

## ---------------------------
## 1.3.3 add_note(x, note)
## ---------------------------

add_note <- function(x,note){
  ret <- x
  ret$note <- note
  return(ret)
}

## ---------------------------
## 1.3.4 sum_numeric_parts(x)
## ---------------------------

sum_numeric_parts <- function(x){
  ret <- as.numeric(unlist(x))
  ret <- sum(ifelse(is.na(ret),0,ret))
  return(ret)
}

## ---------------------------
## 1.4.1 my_data.frame()
## ---------------------------

my_data.frame <- function() {
  id_column <- c(1, 2, 3)
  name_column <- c("John", "Lisa", "Azra")
  income_column <- c(7.30, 0.00, 15.21)
  rich_column <- c(FALSE,FALSE,TRUE)
  ret <- data.frame(id=id_column, name=name_column, income=income_column, rich=rich_column)
  return(ret)
}

## ---------------------------
## 1.4.2 sort_head(df, var.name, n)
## ---------------------------

sort_head <- function(df, var.name, n){
  ret <- df[order(-df[[var.name]]),]
  ret <- head(ret,n)
  return(ret)
}

## ---------------------------
## 1.4.3 add_median_variable(df, j)  
## ---------------------------

add_median_variable <- function(df,j) {
    ret <- df
    med <- median(df[[j]])
    compared_to_median <- ifelse(df[[j]] > med, "Greater",ifelse(df[[j]] < med, "Smaller","Median"))
    ret$compared_to_median <- compared_to_median
    return(ret)
}

## ---------------------------
## 1.4.4 analyze_columns(df, j)
## ---------------------------
analyze_columns <- function(df,j) {
  colname1 <- names(df)[j[1]]
  colname2 <- names(df)[j[2]]
  content1 <- c(mean=mean(df[,j[1]]),median = median(df[,j[1]]), sd = sd(df[,j[1]]))
  content2 <- c(mean=mean(df[,j[2]]),median = median(df[,j[2]]), sd = sd(df[,j[2]]))
  content3 <- cor(df[j], df[j]) 
  ret <- list(content1, content2, content3)
  names(ret) <- c(colname1,colname2,"correlation_matrix")
  return(ret)
}


## ---------------------------
## BEGIN TO TEST
## ---------------------------
mark_my_assignment()

