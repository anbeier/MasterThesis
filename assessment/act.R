source('actPreprocess.R')
source('actLinearRegression.R')

csvFile = 'act.csv'
fqsFile = 'act_tutor.txt'

main <- function(csvfp = csvFile, fqsfp = fqsFile, delta, alpha) {
  data <- readingActData(csvFile)
  cliqueGroups <- readingQuasiCliques(fqsFile)
  for(i in 1:length(cliqueGroups)) {
    clique <- getOneClique(data, cliqueGroups, i)
    loopTrainingTestingLM(clique, i, delta, alpha)
  }
  rm(list = ls())  ## remove all user-defined objects
}