# Coursera Course Computing for Data Analysis
# Programming assignment 3
# Author Shan Lu


rm(list=ls())
setwd("/Users/oh_baizhima/Desktop/coursera/Coursera-Computing-for-Data-Analysis/week3/ProgAssignment3-data")

# Part 1 
# Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[, 11])
# head(outcome)
hist(outcome[,11],main="Heart Attack 30-day Death Rate",xlab="30-day Death Rate")

# Part 2
# Plot the 30-day mortality rates for heart attack, heart failure, and pneumonia
plot.new()
outcome[,17] <- as.numeric(outcome[,17])
outcome[,23] <- as.numeric(outcome[,23])
par(mfrow = c(3, 1))
rr <- range(c(outcome[,11],outcome[,17],outcome[,23]),na.rm=TRUE)
mean1 <- mean(outcome[,11],na.rm=TRUE)
hist(outcome[, 11], xlab="30-day Dearth Rate", main="Heart Attack")
abline(v = median(outcome[,11],na.rm = TRUE), col = 4)
hist(outcome[, 17],main="Heart Failure",xlab="30−day Death Rate",ylab="Frequency",xlim=rr,ylim = NULL)
abline(v = median(outcome[,17],na.rm = TRUE), col = 4)
hist(outcome[, 23],
     main="Pneumonia",
     xlab="30−day Death Rate",ylab="Frequency",
     xlim=rr,ylim = NULL)
abline(v = median(outcome[,23],na.rm = TRUE),col = 4)

# Part 3
# Plot 30-day death rates by state
# several lines are inspired by Xiandan, UIUC
plot.new()
t1 <- table(outcome$State)
t2 <- t1[t1<20]
exclusion <- c("AK","DC","DE","GU","HI","RI","VI","VT")
outcome2 <- outcome[!outcome$State %in% exclusion, ]
#table(outcome2$State)
death <- outcome2[, 11]
state <- outcome2$State
par(mfrow = c(1, 1),las=2)
by.m<-reorder(state, death, median, na.rm=T)
boxplot(death ~ by.m, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State", las=2, cex.axix=0.7, xaxt="n")
axis(1, by.m, paste0(by.m,"(",table(outcome2$State)[outcome2$State],")"), las=2, cex.axis=0.7)

# Part 4
# Plot 30-day death rates and numbers of patients
hospital <- read.csv("hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)
library(lattice)
# Use the xyplot function in the lattice package to make 
# a plot of the relationship between 30-day
# death rate for heart attack versus the number of patients seen. 
# The number of patients should be on
# the x-axis. Make sure you run library(lattice) before calling xyplot.
xyplot(death ~ npatient | owner, 
       xlab="Number of Patients Seen", ylab="30-day Death Rate", 
       main="Heart Attack 30-day Death Rate by Ownership",
       panel = function(x, y, ...){
            panel.xyplot(x, y, ...)
            panel.lmline(x, y, lwd = 2)
      }
       )