# For-loop in a quasi clique; for each column do calculate association rules
loopAssociationRules <- function(qs, qsIndex) {
  result <- NULL
  for(colname in names(qs)) {
    rules <- getRules(qs, colname)
    df <- transformRulesToDataframe(rules, qsIndex, colname)
    result <- rbind(result, df)
  }
  return(result)
}

getRules <- function(df, colname) {
  rhs.vector <- getAllLevelsOfTargetColumn(df, colname)
  trans <- as(df, 'transactions')  ## Convert data frame to transactions
  rules <- apriori(trans, parameter = list(minlen = 2, support = 0.01, confidence = 0.6), 
                   appearance = list(rhs = rhs.vector, default = 'lhs'), 
                   control = list(verbose = FALSE))
  if(length(rules) > 0) {
    rules.pruned <- getRulesPruned(rules)
    return(rules.pruned)
  } else if(length(rules) == 0) {
    return(rules)
  } 
}

getAllLevelsOfTargetColumn <- function(df, colname) {
  levels <- levels(df[, colname])
  res <- NULL
  for(lv in levels) {
    temp <- paste(colname, lv, sep = '=')
    res <- c(res, temp)
  }
  return(res)
}

getRulesPruned <- function(rules) {
  subset.matrix <- is.subset(rules, rules)
  subset.matrix[lower.tri(subset.matrix, diag=TRUE)] <- NA
  redundant <- colSums(subset.matrix, na.rm=TRUE) >= 1
  rules.pruned <- rules[!redundant]
  rules.sorted <- sort(rules.pruned, by = 'lift')  ## Sort rules by lift
  return(rules.sorted)
}

transformRulesToDataframe <- function(rules, cliqueIndex, targetCol) {
  if(length(rules) > 0) {
    df <- as(rules, 'data.frame')
    df <- cbind(clique_index = cliqueIndex, target_column = targetCol, df)
  } else if(length(rules) == 0) {
    df <- data.frame(clique_index = cliqueIndex, target_column = targetCol, 
                     rules = NA, support = NA, confidence = NA, lift = NA)
  }
  return(df)
}