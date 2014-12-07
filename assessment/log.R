log <- function(text) {
  print(text)
  write(text, file='progress.log', append=TRUE)
}
