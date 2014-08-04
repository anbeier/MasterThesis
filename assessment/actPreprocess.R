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
                        paste('V', as.numeric(x) + 1, sep = '')  ## indices in fqs file starting at 0
                      }))
  return(res)
}

getOneClique <- function(dataset, cliqueGroups, cliqueIndex) {
  res <- subset(dataset, select = cliqueGroups[[cliqueIndex]])
  return(res)
}

splitData <- function(data, seed = 100) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  sampleSize <- floor(0.65 * nrow(data))  ## 65% of the dataset size
  trainIndex <- sample(seq_len(nrow(data)), size = sampleSize)
  trainset <- data[trainIndex, ]
  testset <- data[-trainIndex, ]
  list(trainset=trainset,testset=testset)
}