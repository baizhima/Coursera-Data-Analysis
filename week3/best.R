# Coursera Course Computing for Data Analysis
# Programming assignment 3
# Author Shan Lu

# part 5
# Finding the best hospital in a state

best <- function(state, outcomeName) {
  
  ## Read outcome data
  #state <- 'CA'
  #outcomeName <- "heart attack"
  #setwd("/Users/oh_baizhima/Desktop/coursera/Coursera-Computing-for-Data-Analysis/week3/ProgAssignment3-data")
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- outcome$State
  diseases <- c("heart attack","heart failure","pneumonia")
  ## Check that state and outcome are valid
  if (! state %in% states ){
    stop("invalid state")
  }
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
  outcome.sub <- outcome[outcome[,7] == state, ]
  outcome.sub2 <- outcome.sub[,c(2,7,col_id)]
  outcome.sub3 <- outcome.sub2[outcome.sub2[,3]!="Not Available",]
  outcome.sub3[,3] <- as.numeric(outcome.sub3[,3])
  outcome2 <- outcome.sub3
  bestrow <- outcome2[outcome2[,3] == min(outcome2[,3]),]
  as.character(bestrow[1])
}