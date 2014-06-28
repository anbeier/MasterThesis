# input: file path of fqs.txt
# output: a character vector of all the final quasi cliques

readCensusFqs <- function(fqsFile) {
  
  census <- read.csv("census.csv", sep = ";")
  
  colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8")
  
  fqs <- readLines(fqsFile, encoding = "UTF-8")
  
  qs <- NULL
  
  for(i in 1:length(fqs)) {
    tmp <- unlist(strsplit(fqs[i], "--"))
    qs <- c(qs, tmp)
  }
  
  return(qs)
}