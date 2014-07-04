# read quasi cliques
# input: fqs.txt file path
# output: a list of character vectors indicating the quasi cliques
readFqsFile <- function(fqsFile) {
  
  fqs <- readLines(fqsFile, encoding = "UTF-8")  
  qs <- NULL
  
  for(i in 1:length(fqs)) {
    tmp <- unlist(strsplit(fqs[i], "--"))
    qs <- append(qs, list(tmp))
  }
  
  return(qs)
}


# choose a few samples from a clique w.r.t a specific target value
# input: a quasi clique, a target value
# output: a sensible amount of samples for training
trainingSamples <- function(qs, targetval) {
  
  samples <<- qs[sample(nrow(qs), replace = FALSE, size = 0.15 * nrow(qs))]  
  
  ## install.packages("caTools")
  library(caTools)
  split <<- sample.split(samples[, targetval], SplitRatio = 0.6)
  training <- subset(samples, split == TRUE)
  return(training)
}

# output: a sensible amount of samples for testing
testingSamples <- function() {
  
  return(subset(samples, split == FALSE))
}


# replace invalid symbols in column names with "_"
# input: a quasi clique
# output: a quasi clique with all valid column names
reviseColnames <- function(qs) {
  
  for(i in 1:ncol(qs)) {
    colnames(qs)[i] <- gsub(" ", "_", colnames(qs)[i], fixed = TRUE)
    colnames(qs)[i] <- gsub("-", "_", colnames(qs)[i], fixed = TRUE)
  }
  
  return(qs)
}


# categorize age into group
makeAgeInterval <- function(qs) {
  
  qs$age.group <- findInterval(qs$age, seq(0, 90, 10))
  qs$age <- NULL
  return(qs)
}
