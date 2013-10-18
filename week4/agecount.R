# Week 4 programming assignment
# Author Shan Lu

# setwd("/Users/oh_baizhima/Desktop/coursera/Coursera-Data-Analysis/week4")

# part 2
# Ages of homicide victims
agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if(is.null(age)){
    stop("error!")
  }
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt") 
  ## Extract ages of victims; ignore records where no age is
  ## given
  z <-regexec("male, (.*?) years old",homicides,ignore.case = TRUE)
  m <- regmatches(homicides, z)
  ages <- sapply(m, function(x) x[2])
  n1 <- length(which(ages==age))
  
  z2 <-regexec("Age: (.*?) years old",homicides,ignore.case = TRUE)
  m2 <- regmatches(homicides, z2)
  ages2 <- sapply(m2, function(x) x[2])
  n2 <- length(which(ages2==age))
  return(n1+n2)
  
  ## Return integer containing count of homicides for that age
}