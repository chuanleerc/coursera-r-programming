# Chuan Lee
# R Programming course by Johns Hopkins University
# Coursera https://www.coursera.org/learn/r-programming/
# January 2017 Session
# Programming assignment 3 : Hospital Quality
# Week 4 - Ranking hospitals in all states


rankall<-function(outcome, num="best"){

## Read outcome data
data<-read.csv("outcome-of-care-measures.csv",colClasses="character", na.strings="Not Available", stringsAsFactors=FALSE)
data<-data[order(data[,7],data[,2]),]
data<-data[,c(2,7,11,17,23)]
outcomes<-c("heart attack","heart failure","pneumonia")

## Check that outcome is valid
if (sum(outcomes==outcome)==0){
  stop("invalid outcome")
}

## Subset the right columns depending on the outcome
if (outcome=="heart attack"){
  i<-3
} else if (outcome=="heart failure"){
  i<-4
} else if (outcome=="pneumonia") {
  i<-5
}
df<-data[,c(1,2,i)]
df[,3]<-as.numeric(df[,3])

## For each state, find the hospital with the given rank

result<-data.frame()
  
for (x in unique(df$State)){
  sub_df<-df[df$State==x,]
  sub_df<-sub_df[complete.cases(sub_df),]
  sub_df<-sub_df[order(sub_df[,3],sub_df[,1]),]
  
  if (num=="best"){
    rank<-1
  } else if (num=="worst"){
    rank<-nrow(sub_df)
  } else {
    rank<-num
  }

  result<-rbind(result,cbind(sub_df[rank,1],x))
}

names(result)<-c("Hospital","State")
result

}
