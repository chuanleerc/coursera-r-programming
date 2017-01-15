# Chuan Lee
# R Programming course by Johns Hopkins University
# Coursera https://www.coursera.org/learn/r-programming/
# January 2017 Session
# Programming assignment 3 : Hospital Quality
# Week 4 - Finding the best hospital in a state

best <- function(state, outcome) {
    
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes<-c("heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if (sum((data[["State"]])==state)==0){
    stop("invalid state")
  }
  if (sum(outcomes==outcome)==0){
    stop("invalid outcome")
  }
    
  ## Subset the right columns depending on the outcome
  if (outcome=="heart attack"){
    i<-11
  } else if (outcome=="heart failure"){
    i<-17
  } else if (outcome=="pneumonia") {
    i<-23
  }
  
  ## df within the selected state with the chosen outcome rates
  data_state<-data[data$State==state,c(2,i)]

  ## Return hospital name in that state with lowest 30-day death rate
  data_state[,2]<-as.numeric(data_state[,2])
  min<-min(data_state[,2], na.rm=TRUE)
  best<-data_state[data_state[,2]==min,c(1)]

  sort(best)[1]
}
