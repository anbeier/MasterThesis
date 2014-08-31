library(monmlp)
library(caret)

source('actPreprocess.R')
source('analyticalTechniques.R')

csvfp = 'act.csv'
fqsfp = 'act_tutor.txt'
d = 0.7
a = 0.5

tryDifferentTechniques <- function(csvFile = csvfp, fqsFile = fqsfp, delta = d, alpha = a) {
  act <- readActData(csvFile)
  cliqueGroup <- readQuasiCliques(fqsFile)
  sampleClique <- getOneClique(act, cliqueGroup, 1) 
  fileIndicator <- makeFileIndicator(delta, alpha, 1)
  
  result.lm <- loopTrainLinearRegressionForOneClique(sampleClique, 1, fileIndicator)
  result.percp <- loopTrainMultilayerPerceptron(sampleClique, 1, fileIndicator)
  result.cart <- loopTrainClassificationRegressionTree(sampleClique, 1, fileIndicator)
  
  rmsd.lm <- returnRMSDTable(result.lm$result)
  rmsd.percp <- returnRMSDTable(result.percp$result)
  rmsd.cart <- returnRMSDTable(result.cart$result)
  
  data.frame(target = rmsd.lm$target,
             RMSD_lm = rmsd.lm$RMSD,
             RMSD_perceptron = rmsd.percp$RMSD,
             RMSD_cart = rmsd.cart$RMSD)
}


main <- function(cliqueIndex, delta = d, alpha = a, csvFile = csvfp, fqsFile = fqsfp) {
  data <- readActData(csvFile)
  cliqueGroups <- readQuasiCliques(fqsFile)
  for(i in 1:length(cliqueGroups)) {
    clique <- getOneClique(data, cliqueGroups, i)
    loopTrainLinearRegressionForOneClique(clique, i, delta, alpha)
  }
  rm(list = ls())  ## clear working space
}