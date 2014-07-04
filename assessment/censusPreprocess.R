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
