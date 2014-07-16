library(parallel)
library(e1071)
library(caTools)
source('censusPreprocess.R')
source('censusSVM.R')

csvfp = 'census.csv'
colnamefp = 'census_colnames.txt'
fqsfp = 'census_fqs_delta0.7_alpha0.5.txt'

main <- function(index, delta = 0.7, alpha = 0.5, 
                 csvFile = csvpf, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # A loop where models for each clique, i.e. for each column in this clique are built.
  clique <- getOneClique(census, cliques, index)  
  # save result of each clique in a RData file
  loopTrainingTesting(clique, index, delta, alpha)
}

worker <- function(input) {
  print(paste("processing", input$index))
  source('censusPreprocess.R')
  source('censusSVM.R')
  ret <- tryCatch({
    result <- loopTrainingTesting(input$clique)
    list(index=input$index, result=result)
  }, error=function(e) {
    print(paste("error processing", input$index, e))
  })
  ret
}

parallelMain <- function(indexStart, indexEnd, alpha = 0.5, csvFile = csvpf, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # A loop where models for each clique, i.e. for each column in this clique are built.
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    print(paste("prepare input for", i))
    list(index=i, clique=getOneClique(census, cliques, i))
  })

  cores <- detectCores() - 2 # be friendly and dont use all cores
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}
