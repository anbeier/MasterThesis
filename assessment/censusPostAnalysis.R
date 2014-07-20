getDirectory <- function(delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '-')
  foldername <- paste('census', prefix, sep = '_')
  return(foldername)
}

makeFilePath <- function(index, delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '-')
  foldername <- paste('census', prefix, sep = '_')
  filename <- paste(prefix, paste('qs', index, sep = ''), sep = '-')
  filename <- paste(filename, 'RData', sep = '.')
  filepath <- paste(foldername, filename, sep = '/')
  return(filepath)
}

readExperimentResults <- function(delta, alpha) {
  dir <- getDirectory(delta, alpha)
  
  fileList <- list.files(path = file)
  # how many files in this file path?
}