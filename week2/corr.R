corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  x <- vector(mode="numeric", length=0)
  id <- 1:332
  csvfiles <- sprintf("/Users/oh_baizhima/Desktop/coursera/dataAnalysis/%s/%03d.csv", directory, id)
  countComplete <- function(f){
    data <- read.csv(f)
    completeData <-complete.cases(data)
    length(completeData[completeData == TRUE])        
  }
  nrows <- sapply(csvfiles,countComplete,simplify = TRUE)
  for (i in id) {
    if (nrows[i] >= threshold) {
      data <- read.csv(csvfiles[i])
      rawmatrix <- matrix(c(data[,'sulfate'],data[,'nitrate']),ncol=2)
      newdata <- na.omit(rawmatrix)
      corr_nitrate_N_sulfate <- cor(newdata)[1,2]
      x <- c(x,corr_nitrate_N_sulfate)
    }
  }
  y <- x
}