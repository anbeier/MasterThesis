# specialCols = c(3, 4, 36, 38, 40)

# returns a quasi clique by giving the its index
main <- function(index, alpha = 0.5, csvFile = "census.csv", 
                 colnameFile = "census_colnames.txt", fqsFile = "census_fqs_delta0.7_alpha0.5.txt") {
  
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # here is a loop modelling for each clique and for each column in a clique
  # loop 1: take the 1st clique for example
  qs <- getOneClique(census, cliques, index)
  
  # here is the 2nd loop which is for each column in the clique
  # loop 2: take one column for example
  # target = "migration_code_change_in_msa"
  # df <- takeSamples(qs, target)
  
  # here is to train a SVM model regarding to a specific target value in a clique
  # res <- getTrainingResultsOfOneClique(qs)
  
  # get a list of data frames regarding to a specific target value, respectively
  ls <- makeSamplesForEachTarget(qs, cliques[[index]])
  writeCSVForEachSamples(ls)
}