source('actPreprocess.R')

csvFile = 'act.csv'
fqsFile = 'act_tutor.txt'

main <- function(csvFile, fqsFile) {
  data <- readingActData(csvFile)
  cliqueGroups <- readingQuasiCliques(fqsFile)
  clique <- getOneClique(data, cliqueGroups, cliqueIndex)
  res <- loopTrainingTestingLM(clique, cliqueIndex)
}