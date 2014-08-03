readingActData <- function(csvFile) {
  data <- read.csv(csvFile, sep = ";", header = FALSE, colClasses = 'numeric')
  return(data)
}

readingQuasiCliques <- function(fqsFile) {
  cliques <- readLines(fqsFile, encoding = "UTF-8") 
  qs <- NULL  
  for(aClique in cliques) {
    temp <- unlist(strsplit(aClique, "--"))  ## get a vector of single points (column indices starting at 0)
    qs <- append(qs, list(temp))
  }
  qs <- lapply(qs, function(x) giveColumnName(x))  
  return(qs)
}

giveColumnName <- function(aClique) {
  res <- unlist(lapply(as.list(aClique), 
                      function(x) {
                        paste('V', x, sep = '')
                      }))
  return(res)
}