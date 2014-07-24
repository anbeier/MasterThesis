colname = 'hispanic_origin'

oldLabels <- levels(df[, colname])
listOfNewLabels <- lapply(levels(df[, colname]), function(x) modifySingleString(x))
newLabels <- rle(unlist(listOfNewLevels))$values  ## Merge all items in the list to a vector
levels(df[, colname]) <- c(levels(df[, colname]), newLabels)  ## Add new levels to the factor

# Replace values with new levels
for(i in 1:length(newLabels)) {
  df[, colname][df[, colname] == oldLabels[i]] <- newLabels[i]
}

df[, colname] <- factor(df[, colname])

rules <- apriori(df, parameter = list(minlen = 2, supp = 0.01, conf = 0.6), 
                 appearance = list(rhs = newLabels, default = 'lhs'), 
                 control = list(verbose = FALSE))
# Example
rules <- apriori(titanic.raw,
                 + parameter = list(minlen=2, supp=0.005, conf=0.8),
                 + appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                     + default="lhs"),
                 + control = list(verbose=F))