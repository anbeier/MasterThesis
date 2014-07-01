census <- read.csv("census.csv", sep = ";")  
# colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8") 
cols <- readLines("census_colnames.txt", encoding = "UTF-8") 
replaceSpacesInColnames(census, cols)  ## not fully success!
fqsFile <- "census_fqs_delta0.7_alpha0.5.txt"

qs <- readFqsFile(fqsFile)
df <- subset(census, select = qs[[1]])

install.packages("gdata")
library(gdata)
df <- mapFactorInt(df, "education")
df <- mapFactorInt(df, "tax filer stat")
df <- mapFactorInt(df, "family members under 18")
df <- mapFactorInt(df, "migration code-move within reg")