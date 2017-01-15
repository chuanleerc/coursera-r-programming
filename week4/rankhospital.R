# Chuan Lee
# R Programming course by Johns Hopkins University
# Coursera https://www.coursera.org/learn/r-programming/
# January 2017 Session
# Programming assignment 3 : Hospital Quality
# Week 4 - Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, rank="best") {
  
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

## df within the selected state with the three death rates
data_state<-data[data$State==state,c(2,i)]
data_state<-data_state[complete.cases(data_state),]
data_state[,2]<-as.numeric(data_state[,2])
data_state<-data_state[order(data_state[,2], data_state[,1]),]

## Return hospital name in that state with the given rank outcome death rate

if (rank=="best"){
  rank<-1
} else if (rank=="worst"){
  rank<-nrow(data_state)
} 

data_state[rank,]$name

}
