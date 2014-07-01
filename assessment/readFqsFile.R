# input: file path of fqs.txt
# output: a list of character vectors indicating the final quasi cliques

readFqsFile <- function(fqsFile) {
  
  #  census <- read.csv("census.csv", sep = ";")  
  #  colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8") 
  fqs <- readLines(fqsFile, encoding = "UTF-8")  
  qs <- NULL
  
  for(i in 1:length(fqs)) {
    tmp <- unlist(strsplit(fqs[i], "--"))
    qs <- append(qs, list(tmp))
  }
  
  return(qs)
}


# not fully successful
replaceSpacesInColnames <- function(df, cols) {
  
  for(i in 1:length(cols)) {
    cols[i] <- gsub(" ", ".", cols[i])
  }
  
  colnames(census) <- cols
}