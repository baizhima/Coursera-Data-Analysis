# Week 4 programming assignment
# Author Shan Lu

# setwd("/Users/oh_baizhima/Desktop/coursera/Coursera-Data-Analysis/week4")

# part 1
# How many of each cause of homicide?
count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)){
    stop("error!")
  }
  ## Check that specific "cause" is allowed; else throw error
  options = c("asphyxiation","blunt force","other","shooting",
"stabbing", "unknown")
  if(!(cause %in% options)) {
    stop("cause is not allowed!")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")   
  ## Extract causes of death
  z <-grep(paste("Cause:",cause),homicides,ignore.case = TRUE)
  ## Return integer containing count of homicides for that cause
  return(length(z))
}