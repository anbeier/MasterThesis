#source('log.R')

getCensusData <- function(csvFilePath, colnameFilePath) {
  # Read in dataset, modify column names, eliminate rows with NAs
  data <- readingData(csvFilePath, colnameFilePath)
  # Coverting some of integer columns directly to categorical columns
  data <- convertSpecialIntegerColumnToFactor(data)
  # Mapping numeric values to a specific level of factor w.r.t. a specific range of values
  data <- convertNumericColumnToFactor(data)
  return(data)
}

readingData <- function(csvFile, colnameFile) { 
  df <- read.csv(csvFile, sep = ";", header = FALSE)  
  colnames(df) <- readLines(colnameFile, encoding = "UTF-8")
  df <- modifyColnames(df)
  # remove rows containing NAs
  df <- na.omit(df)    
  return(df)
}

# Indices of columns start with 1. 
# Convert integer columns 3, 4, 36, 38, 40 to factor columns.
convertSpecialIntegerColumnToFactor <- function(df, specialColumns = c(3, 4, 36, 38, 40)) {
  for(i in specialColumns) {
    df[, i] <- as.factor(df[, i])
  }
  return(df)
}

convertNumericColumnToFactor <- function(df) {
  dd <- df
  for(i in 1:ncol(dd)) {
    if(class(dd[, i]) == 'numeric' | class(dd[, i]) == 'integer') {      
      colname = colnames(dd)[i]
      df <- categorizeNumericValuesIntoIntervals(df, colname)
    }
  }
  return(df)
}

categorizeNumericValuesIntoIntervals <- function(df, colname) {
  df[, colname] <- as.numeric(df[, colname])
  columnVector <- df[, colname]
  ## Get the .2th and .8th quantile of values in this column
  q <- quantile(columnVector, probs = c(0.2, 0.8))  
  ## Divide the set of values into max. 10 subsets (user defined)
  range <- (q['80%'] - q['20%']) / 10
  df[, colname] <- unlist(lapply(df[, colname], 
                                 function(x) {
                                   i <- (x - q['20%']) %/% range
                                   label <- paste(colname, i, sep = '')
                                   return(label)
                                 }))
  df[, colname] <- as.factor(df[, colname])
  return(df)
}

# not used
# Bucketize values of numeric columns to bins and map them onto levels of a factor.
# Convert numeric columns to factor columns.
bucketizeNumericColumns <- function(df, alpha, specialColumns = c(3, 4, 36, 38, 40)) {
  
  x <- calculateBins(df, alpha)  
  bin <- x$bin
  binsize <- x$binsize 
  
  # special treatment for column[3, 4, 36, 38, 40]
  # numericColumnsAsCategory <- c(3, 4, 36, 38, 40)
  dd <- df
  for(i in 1:ncol(dd)) {
    if(class(dd[, i]) == 'numeric' | class(dd[, i]) == 'integer') {      
      if(!(i %in% specialColumns)) {
        colname = colnames(dd)[i]
        df <- mapBinsOntoColumn(df, colname, bin, binsize)
      }
    }
  }
  return(df)
}


calculateBins <- function(df, alpha) {
  
  n <- nrow(df)
  b <- n^alpha
  if(b > 128) {
    b <- 128
  }  
  if(b > n) {
    b <- n
  }
  bs <- floor(n / b)
  
  bin <- function() {b}
  binsize <- function() {bs}
  list(bin = b, binsize = bs)
}


mapBinsOntoColumn <- function(df, colname, bin, binsize){
  
  df <- df[order(df[, colname]), ]
  
  i <- 1
  vec <- NULL
  while(i <= bin) {
    tmp <- rep(paste(colname, 'bin', i, sep = '_'), binsize)
    vec <- c(vec, tmp)
    i  = i + 1
  }
  
  if(length(vec) < nrow(df)) {
    tmp <- rep(paste(colname, 'bin', i, sep = '_'), nrow(df) - length(vec))
    vec <- c(vec, tmp) 
  }
  
  if(length(vec) == nrow(df)) {
    df[, colname] <- vec
  }
  df[, colname] <- as.factor(df[, colname])
  return(df)
} 

# elinimate invalid symbols in column names
modifyColnames <- function(qs) {  
  colnames(qs) <- gsub(" ", "", colnames(qs), fixed = TRUE)
  colnames(qs) <- gsub("-", "", colnames(qs), fixed = TRUE)
  colnames(qs) <- gsub("'", "", colnames(qs), fixed = TRUE)
  return(qs)
}

modifySingleString <- function(str) {  
  str <- gsub(" ", "", str, fixed = TRUE)
  str <- gsub("-", "", str, fixed = TRUE)
  str <- gsub("'", "", str, fixed = TRUE)
  return(str)
}

readingQuasiCliques <- function(fqsFile) {
  cliques <- readLines(fqsFile, encoding = "UTF-8") 
  qs <- NULL  
  for(aClique in cliques) {
    temp <- unlist(strsplit(aClique, "--"))
    qs <- append(qs, list(temp))
  }
  qs <- lapply(qs, function(x) modifySingleString(x))  
  return(qs)
}

# returns a quasi clique a according to the given index without duplicated rows
getOneClique <- function(dataset, cliqueList, cliqueIndex) { 
  qs <- subset(dataset, select = cliqueList[[cliqueIndex]])   
  qs <- unique(qs)
  return(qs)
}

# Extract a few samples from a quasi clique w.r.t a specific target value
takeSamples <- function(qs, targetcol) {
  # samples <- qs[sample(nrow(qs), replace = FALSE, size = 0.01 * nrow(qs)), ]  
  samples <- qs
  ratio <- 0.6
  #alpha <- 0.5
  #trainingSize <- nrow(samples)^alpha + 1000
  #if (nrow(samples) > trainingSize) {
  #  ratio <- trainingSize / nrow(samples)
  #}
  #log(paste("choosing ratio", ratio, "sample training size", trainingSize))
  split <- sample.split(samples[, targetcol], SplitRatio = ratio)
  training <- subset(samples, split == TRUE)
  testing <- subset(samples, split == FALSE) 
  list(training = training, testing = testing)
}

# not used
# dummy code the factors except for the target value
# input: a quasi clique, the target value
# output: a data frame in which all the factors except for the target value are replaced with dummy code
dummycodeFactors <- function(qs, target) {
  
  tmp <- qs[, target]
  qs[, target] <- NULL
  df <- qs
  cols <- colnames(df)
  
  for(colname in colnames(qs)) {   
    if(class(qs[, colname]) == 'character' | class(qs[, colname]) == 'factor') {
      df <- cbind(df, model.matrix(~ df[, colname] - 1, df))
      df[, colname] <- NULL
    }
  }
  
  df$newcol <- tmp
  colnames(df)[ncol(df)] <- target
  return(df)
}

# not used
# convert factors to integers using mapLevels()
convertCategories <- function(qs) {
  
  ## install.packages("gdata")
  library(gdata)
  
  df <- qs
  cols <- colnames(qs)
  
  for(i in 1:ncol(qs)) {
    
    if(class(qs[, cols[i]]) == "factor") {
      
      col <- cols[i]
      fac <- factor(qs[, col])
      map <- mapLevels(x = fac)    
      newcol <- paste(col, "cat", sep = ".")
      
      for(j in 1:length(map)) {
        df$newcol[df[, col] == names(map)[j]] <- map[[j]]    ## add a new column
      }
      
      colnames(df)[ncol(df)] <- newcol    ## change the new column name
      df[, col] <- NULL
    }
  }
  
  return(df)
}

# Will return a sharing part of all file names
makeFileIndicator <- function(delta, alpha, i) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '-')
  string <- paste(prefix, paste('qs', i, sep = ''), sep = '-')
  string <- paste(string, 'rdata', sep = '.')
  return(string)
}