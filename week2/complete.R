# Chuan Lee
# R Programming course by Johns Hopkins University
# Coursera https://www.coursera.org/learn/r-programming/
# January 2017 Session
# Programming assignment 1: Air Pollution
# Week 2 - Part 2

# Write a function that reads a directory full of files and reports the number
# of completely observed cases in each data file. The function should return a
# data frame where the first column is the name of the file and the second
# column is the number of complete cases. A prototype of this function follows:

# 'directory' is a character vector of length 1 indicating
# the location of the CSV files
# 'id' is an integer vector indicating the monitor ID numbers
# to be used
# Return a data frame of the form:
# id nobs
# 1  117
# 2  1041
# ...
# where 'id' is the monitor ID number and 'nobs' is the
# number of complete cases

complete<-function(directory,id=1:332){
  liste<-paste0(directory,"/",list.files(directory))
  df<-data.frame()
  
  for (i in id){
    cc<-sum(complete.cases(data.frame(read.csv(liste[i]))))
    df<-rbind(df,c(i,cc))
  }
  names(df)<-c("id","nobs")
  df
 }
 
##TEST
# source("complete.R")
# complete("specdata", 1)
##   id nobs
## 1  1  117
# complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
# complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
# complete("specdata", 3)
##   id nobs
## 1  3  243
