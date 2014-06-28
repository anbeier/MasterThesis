census <- read.csv("census.csv", sep = ";")  
colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8") 

fqsFile <- "census_fqs_delta0.7_alpha0.5.txt"
qs <- readFqsFile(fqsFile)
df <- subset(census, select = qs[[1]])

install.packages("gdata")
library(gdata)
df <- mapFactorInt(df, "education")
