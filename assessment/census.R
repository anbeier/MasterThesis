csvFile = "census.csv"
colnameFile = "census_colnames.txt"
fqsFile = "census_fqs_delta0.7_alpha0.5.txt"
alpha = 0.5

main <- function(csvFile, colnameFile, alpha, fqsFile) {
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # here is a loop modelling for each clique and for each column in a clique
  # loop 1: take the 1st clique for example
  index <- 1
  qs <- getOneClique(census, cliques, index)
  
  # here is the 2nd loop which is for each column in the clique
  # loop 2: take one column for example
  target = "migration_code_change_in_msa"
  df <- preprocessingMatrix(qs, target)
  
  # train a svm model and return its results
  res <- getTrainingResultsOfOneClique(df)
   
}





# preprocess: categorize age into group and make it factor
df <- makeAgeIntervalFactor(df)    ## not testet yet

# preprocess: categorize worked weeks into group
df <- makeWeekIntervalFactor(df)    ## not testet yet

# preprocess: convert education factor to integers by hand
qs <- makeEducationCat(qs)

# proprocess: convert factors to integers using mapLevels()
qs <- convertCategories(qs)