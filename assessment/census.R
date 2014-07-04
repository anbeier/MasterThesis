# read dataset
census <- read.csv("census.csv", sep = ";")  
colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8")
census <- unique(census)    ## remove duplicates

# read quasi cliques in a list of char vectors
fqsFile <- "census_fqs_delta0.7_alpha0.5.txt"
fqs <- readFqsFile(fqsFile)

# choose one of the quasi cliques
qs <- subset(census, select = fqs[[1]])    ## take the first one

# choose a few samples with respect to a specific target value
target = ""
qs <- trainingSamples(qs, target)
## qstest <- testingSamples()

# replace invalid symbols in column names with "_"
qs <- reviseColnames(qs)
## qstest <- reviseColnames(qstest)

# preprocess: categorize age into group
qs <- makeAgeInterval(qs)

# preprocess: convert education factor to integers by hand
qs <- makeEducationCat(qs)

# proprocess: convert factors to integers using mapLevels()
qs <- convertCategories(qs)