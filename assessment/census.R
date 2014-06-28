# read dataset
census <- read.csv("census.csv", sep = ";")  
colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8") 

# get a list of character vectors indicating the final quasi cliques
fqsFile <- "census_fqs_delta0.7_alpha0.5.txt"
qs <- readFqsFile(fqsFile)

# take the first quasi clique for instance
# take the "education" column in the qs for example
# following is try to map an integer value to a category in one column
# this preprocessing step should be done after having read the dataset
df <- subset(census, select = qs[[1]])

# hist(df[, 1])
# hist(table(df[, 2]))          ## hist() cannot be called directly on a factor variable
# barplot(prop.table(table(df[, 1], df[, 2])), beside = FALSE)

install.packages("gdata")
library(gdata)

# get a data frame in which the factor column "col" obtained an extra column containing integers which are mapped on to the factor
df <- mapFactorInt(df, col)