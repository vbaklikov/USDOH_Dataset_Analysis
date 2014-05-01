rankhospital <- function(state, outcome, num="best"){
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
  
  state_subset <- outcome_data[outcome_data$State == state,]
  
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
  
  sorted <- arrange(state_subset,as.numeric(state_subset[,column]),state_subset$Hospital.Name)
  sorted_shrink <- sorted[complete.cases(as.numeric(sorted[,column])),]
  
  if(num=="best") {
    sorted_shrink[1,"Hospital.Name"]
  }
  else if(num=="worst"){
    sorted_shrink[nrow(sorted_shrink),"Hospital.Name"]
  }
  else{
    sorted_shrink[num,"Hospital.Name"]
  }
}