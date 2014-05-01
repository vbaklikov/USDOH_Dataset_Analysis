rankall <- function(outcome, num="best"){
  library(plyr)
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_state <- unique(outcome_data$State)
  if(!is.element(state,valid_state)){
    stop("invalid state")
  }
  
  valid_outcome <- c("heart attack","heart failure","pneumonia")
  if(!is.element(outcome, valid_outcome)){
    stop("invalid outcome")
  }
  
  column=0
  if(outcome=="heart attack"){
    column = 11
  }
  else if(outcome=="heart failure"){
    column = 17
  }
  else{
    column = 23
  }
  
  splitted <- split(outcome_data,outcome_data$State)
  
  result <- lapply(splitted,function(x) arrange(x,as.numeric(x[,column]),x$Hospital.Name))
  
  if(num=="best"){
    #result <- lapply(splitted,function(x) x[which.min(x[,column]),])
    result <- lapply(result, function(x) x[1,c(2,7)])
  }
  else if(num=="worst"){
    result <- lapply(splitted,function(x) x[which.max(x[,column]),])
    result <- lapply(result,function(x) x[c(2,7)])
    #result <- lapply(result, function(x) x[length(x[[1]]),c(2,7)])
  }
  else{
    #result <- lapply(splitted,function(x) x[num,column])
    #result <- lapply(splitted,function(x) arrange(x,as.numeric(x[,column]),x$Hospital.Name))
    #result <- lapply(result, function(x) x[num,c(2,7)])
    result<- lapply(result, function(x) {if (num>length(x[[1]])){c(Hospital.Name="NA", State=x[1,c(7)])} else{x[num,c(2,7)]}})
  }
  
  #result <- lapply(result,function(x) x[c(2,7)])
  
  compact <- data.frame(matrix(unlist(result),nrow=length(result),byrow=TRUE))
  colnames(compact) <- c("hospital","state")
  
  compact
}