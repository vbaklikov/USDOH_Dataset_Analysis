best <- function(state, outcome){
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
  
  best_record <- head(arrange(state_subset,as.numeric(state_subset[,column])),n=1)
  
  best_record$Hospital.Name
  
}