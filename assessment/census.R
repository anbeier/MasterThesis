census <- read.csv("census.csv", sep = ";")
colnames <- readLines("census_colnames.txt", encoding = "UTF-8")
fqs <- readLines("census_fqs_delta0.7_alpha_0.5.txt", encoding = "UTF-8")

