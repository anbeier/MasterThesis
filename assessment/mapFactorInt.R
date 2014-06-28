
mapCategoryInt <- function(fac, lsCategory, map) {
  
  unitdf <- lsCategory[[fac]]
  vals <- rep(map[[fac]], dim(unitdf)[1])
  unitdf <- cbind(unitdf, "factor" = vals)
  return(unitdf)
}

converToDataframe <- function(lp) {
  
  df <- NULL
  
  for(i in 1:length(lp)) {
    df <- rbind(df, lp[[i]])
  }
  return(df)
}

mapFactorInt <- function(df, col) {
  
  fac <- factor(df[, col])
  map <- mapLevels(x = fac)
  lsCategory <- split(df, df[, col])
  lp <- lapply(names(lsCategory), function(x) mapCategoryInt(x, lsCategory, map))
  resdf <- convertToDataframe(lp)
  return(resdf)
}