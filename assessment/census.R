# returns a quasi clique by giving its index

main <- function(index, alpha = 0.5, csvFile = "census.csv", 
                 colnameFile = "census_colnames.txt", fqsFile = "census_fqs_delta0.7_alpha0.5.txt") {
  
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # A loop where models for each clique, i.e. for each column in this clique are built.
  clique <- getOneClique(census, cliques, index)  
  res <- getResultsOfOneClique(clique)
}