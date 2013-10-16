# Coursera Course Computing for Data Analysis
# Programming assignment 3
# Author Shan Lu


rankall <- function(outcomeName, num = "best") {
  #outcomeName <- "pneumonia"
  #num <- "worst"
  ## Read outcome data
  #setwd("/Users/oh_baizhima/Desktop/coursera/Coursera-Computing-for-Data-Analysis/week3/ProgAssignment3-data")
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- outcome$State
  diseases <- c("heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  
  if (! outcomeName %in% diseases){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  col_id <- 0
  if (outcomeName == diseases[1]){
    col_id <- 11
  } else if (outcomeName == diseases[2]) {
    col_id <- 17
  } else if (outcomeName == diseases[3]) {
    col_id <- 23
  }
  ## For each state, find the hospital of the given rank
  
  state <- levels(factor(outcome$State))
  hospital <- c(seq(length(state)))
  state <- c(state)
  df <- data.frame(cbind(hospital,state),row.names = state,stringsAsFactors=FALSE)
  
  for (i in 1:length(state)){
    outcome.sub <- outcome[outcome[,7] == state[i], ]
    outcome.sub2 <- outcome.sub[,c(2,7,col_id)]
    outcome.sub3 <- outcome.sub2[outcome.sub2[,3]!="Not Available",]
    outcome.sub3[,3] <- as.numeric(outcome.sub3[,3])
    outcome2 <- outcome.sub3
    outcome3 <- outcome2[order(outcome2[,3],outcome2[,1],outcome2[,2]),]
    if(num == "best") {
      num <- 1
    }  
    if (num == "worst"){
      num <- nrow(outcome3)
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    df[i,1] <- c(outcome3[num,1])
    if (num == nrow(outcome3)){
      num <- "worst"
    }
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df
}