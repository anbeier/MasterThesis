# get a quasi clique
# input: file paths
# output: the quasi clique according to the given index
getQuasiClique <- function(csvFile, colnameFile, fqsFile, index) {
  
  census <- read.csv(csvFile, sep = ";")  
  colnames(census) <- readLines(colnameFile, encoding = "UTF-8")
  census <- na.omit(census)    ## remove rows containing NAs
  fqs <- readFqsFile(fqsFile)
  qs <- subset(census, select = fqs[[index]])
  return(qs)
}

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
# output: a list of sensible size of samples for training and testing, respectively
takeSamples <- function(qs, targetval) {
  
  samples <- qs[sample(nrow(qs), replace = FALSE, size = 0.15 * nrow(qs)), ]  
  
  ## install.packages("caTools")
  library(caTools)
  split <- sample.split(samples[, targetval], SplitRatio = 0.65)
  training <- subset(samples, split == TRUE)
  testing <- subset(samples, split == FALSE)
  
  ls <- list(training = training, testing = testing)
  return(ls)
}


# replace invalid symbols in column names with "_"
# input: a quasi clique
# output: a quasi clique with all valid column names
reviseColnames <- function(qs) {
  
  colnames(qs) <- gsub(" ", "_", colnames(qs), fixed = TRUE)
  colnames(qs) <- gsub("-", "_", colnames(qs), fixed = TRUE)
  return(qs)
}


# dummy code the factors except for the target value
# input: a quasi clique, the target value
# output: a data frame in which all the factors except for the target value are replaced with dummy code
dummycodeFactors <- function(qs, target) {
  
  targetval <- qs[, target]
  qs[, target] <- NULL
  df <- qs
  cols <- colnames(df)
  
  for(i in 1:ncol(qs)) {
    
    col <- cols[i]
    
    if(class(df[, col]) == "factor") {
      df <- cbind(df, model.matrix(~ df[, col] - 1, df))
      df[, col] <- NULL
    }
  }
  
  df$newcol <- targetval
  colnames(df)[ncol(df)] <- target
  return(df)
}


# categorize age into group and make it factor
makeAgeIntervalFactor <- function(qs) {
  
  for(i in 0:9) {
    qs$age[qs$age >= i * 10 & qs$age < i * 10 + 10] <- paste("under", i * 10 + 10, sep = " ")
  }
  qs$age <- as.factor(qs$age)
  return(qs)
}


# categorize working weeks into group
makeWeekInterval <- function(qs) {
  
  qs$newcol <- findInterval(qs[, "weeks_worked_in_year"], seq(0, 50, 10))
  colnames(qs)[ncol(qs)] <- "weeks_worked_in_year.cat"
  qs[, "weeks_worked_in_year"] <- NULL
  return(qs)
}


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
