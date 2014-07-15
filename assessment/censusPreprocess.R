getDataset <- function(csvfp, colnameFile, alpha) {
  # Reading in dataset
  census <- readingdataset(csvfp, colnameFile)
  # Coverting some of integer columns to categorical columns
  census <- convertSpecialIntegerColumnToFactor(census)
  # Bucketizing values of a numeric column to bins and convert it to a factor column
  census <- bucketizeNumericColumns(census, alpha)
  return(census)
}


# read in data, modify column names, eliminate NAs.
readingdataset <- function(csvFile, colnameFile) {
  
  df <- read.csv(csvFile, sep = ";")  
  colnames(df) <- readLines(colnameFile, encoding = "UTF-8")
  df <- modifyColnames(df)
  df <- na.omit(df)    ## remove rows containing NAs
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


# read in quasi clique file
readingQuasiCliques <- function(dataset, fp) {
  
  fqs <- readLines(fp, encoding = "UTF-8") 
  qs <- NULL  
  for(i in 1:length(fqs)) {
    tmp <- unlist(strsplit(fqs[i], "--"))
    qs <- append(qs, list(tmp))
  }
  
  qs <- lapply(qs, function(x) modifySingleString(x))  
  return(qs)
}


# returns a quasi clique regarding to the given index, without duplicated entries
getOneClique <- function(dataset, fqs, index) { 
  qs <- subset(dataset, select = fqs[[index]])   
  qs <- unique(qs)
  return(qs)
}

# choose a few samples from a clique w.r.t a specific target value
# input: a quasi clique, a target value
# output: a list of sensible size of samples for training and testing, respectively
takeSamples <- function(qs, targetcol) {
  
  ## samples <- qs[sample(nrow(qs), replace = FALSE, size = 0.04 * nrow(qs)), ]  
  samples <- qs
  split <- sample.split(samples[, targetcol], SplitRatio = 0.5)
  training <- subset(samples, split == TRUE)
  testing <- subset(samples, split == FALSE)
  
  list(training = training, testing = testing)
}


# replace invalid symbols in column names with "_"
# input: a quasi clique
# output: a quasi clique with all valid column names
modifyColnames <- function(qs) {
  
  colnames(qs) <- gsub(" ", "_", colnames(qs), fixed = TRUE)
  colnames(qs) <- gsub("-", "_", colnames(qs), fixed = TRUE)
  return(qs)
}


modifySingleString <- function(str) {
  
  str <- gsub(" ", "_", str, fixed = TRUE)
  str <- gsub("-", "_", str, fixed = TRUE)
  return(str)
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
# categorize age into group and make it factor
makeAgeIntervalFactor <- function(df) {
  
  for(i in 0:9) {
    df$age[df$age >= i * 10 & df$age < i * 10 + 10] <- paste("under", i * 10 + 10, sep = " ")
  }
  df$age <- as.factor(df$age)
  return(df)
}

# not used
# categorize worked weeks into group and make it factor
makeWeekIntervalFactor <- function(df) {
  
  i <- 0
  
  while(i <= 50) {
    
    if(i == 0) {
      df[, "weeks worked in year"][df[, "weeks worked in year"] == 0] <- "no work"
      i <- i + 1
    } else if(i == 50) {
      df[, "weeks worked in year"][df[, "weeks worked in year"] >= 50] <- "over 50 weeks"
      i <- i + 1
    } else if(i == 1) {
      df[, "weeks worked in year"][df[, "weeks worked in year"] >= i & df[, "weeks worked in year"] < i + 4] <- 
        paste("under", i + 4, "weeks", sep = " ")
      i <- i + 4
    } else {
      df[, "weeks worked in year"][df[, "weeks worked in year"] >= i & df[, "weeks worked in year"] < i + 5] <- 
        paste("under", i + 5, "weeks", sep = " ")  
      i <- i + 5
    }  
  }
  
  df[, "weeks worked in year"] <- as.factor(df[, "weeks worked in year"])
  return(df)
}

# not used
# categorize working weeks into group
makeWeekInterval <- function(qs) {
  
  qs$newcol <- findInterval(qs[, "weeks_worked_in_year"], seq(0, 50, 10))
  colnames(qs)[ncol(qs)] <- "weeks_worked_in_year.cat"
  qs[, "weeks_worked_in_year"] <- NULL
  return(qs)
}

# not used
# convert education factor to integers by hand
makeEducationCat <- function(qs) {
  
  lsedu = list("Children" = 1, "Less than 1st grade" = 2,"1st 2nd 3rd or 4th grade" = 3, "5th or 6th grade" = 4, 
                     "7th and 8th grade" = 5, "9th grade" = 6, "10th grade" = 7, "11th grade" = 8, 
                     "12th grade no diploma" = 9, "High school graduate" = 10, "Some college but no degree" = 11, 
                     "Associates degree-occup /vocational" = 12, "Associates degree-academic program" = 13, 
                     "Bachelors degree(BA AB BS)" = 14, "Masters degree(MA MS MEng MEd MSW MBA)" = 15, 
                     "Prof school degree (MD DDS DVM LLB JD)" = 16, "Doctorate degree(PhD EdD)" = 17)
  
  for(i in 1:17) {
    qs$education.cat[qs$education == names(lsedu)[i]] <- lsedu[[i]]
  }
  qs$education <- NULL
  return(qs)
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


makeSamplesForEachTarget <- function(qs, cliqueColnames) {
  ls <- NULL
  for(target in cliqueColnames) {
    df <- takeSamples(qs, target)
    ls <- append(ls, list(df))
  }
  return(ls)
}

writeCSVForEachSamples <- function(ls) {
  for(i in 1:length(ls)) {
    fp <- paste(paste('data', i, sep = '_'), 'csv', sep = '.')
    df <- ls[[i]]
    write.csv(df, file = fp, row.names = FALSE)
  }
}
