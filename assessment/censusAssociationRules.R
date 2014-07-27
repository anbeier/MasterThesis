# For-loop in a quasi clique; for each column do calculate association rules
loopAssociationRules <- function(qs, qsIndex, delta, alpha) {
  fileName <- makeFileNameForResultsFromRules(qsIndex, delta, alpha)
  result <- NULL
  for(colname in names(qs)) {
    rules <- getRules(qs, colname)
    df <- transformRulesToDataframe(rules, qsIndex)
    result <- rbind(result, df)
  }
  result.rules <- getRulesItems(result)
  save(result.rules, file = fileName)
}

getRules <- function(df, colname) {
  rhs.vector <- getAllLevelsOfTargetColumn(df, colname)
  trans <- as(df, 'transactions')  ## Convert data frame to transactions
  rules <- apriori(trans, parameter = list(minlen = 2, support = 0.01, confidence = 0.5), 
                   appearance = list(rhs = rhs.vector, default = 'lhs'), 
                   control = list(verbose = FALSE))
  if(length(rules) > 0) {
    rules.pruned <- getRulesPruned(rules)
    return(rules.pruned)
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

transformRulesToDataframe <- function(rules, cliqueIndex) {
  if(length(rules) > 0) {
    df <- as(rules, 'data.frame')
    df <- cbind(clique_index = cliqueIndex, df)
  } else if(length(rules) == 0) {
    df <- data.frame(clique_index = cliqueIndex,
                     rules = NA, support = NA, confidence = NA, lift = NA)
  }
  return(df)
}

getRulesItems <- function(df) {
  df <- na.omit(df)
  outcome <- NULL
  outcomeValue <- NULL
  df$outcome_column <- unlist(lapply(df[, 'rules'], 
                                     function(x) {
                                       rules.items <- gsub("^[{](.*)[}]$", '\\1', unlist(strsplit(x, ' => ')))
                                       rhs <- rules.items[length(rules.items)]
                                       outcome <- c(outcome, unlist(strsplit(rhs, '='))[1])
                                       return(outcome)
                                     }))
  df$outcome_value <- unlist(lapply(df[, 'rules'], 
                                    function(x) {
                                      rules.items <- gsub("^[{](.*)[}]$", '\\1', unlist(strsplit(x, ' => ')))
                                      rhs <- rules.items[length(rules.items)]
                                      outcome <- c(outcome, unlist(strsplit(rhs, '='))[2])
                                      return(outcome)
                                    }))
  return(df)
}

makeFileNameForResultsFromRules <- function(i, delta, alpha) {
  sharingPart <- makeFileName(i, delta, alpha)
  fn <- paste('rules', sharingPart, sep = '-')
  return(fn)
}