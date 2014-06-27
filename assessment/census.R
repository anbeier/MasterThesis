census <- read.csv("census.csv", sep = ";")
colnames <- readLines("census_colnames.txt", encoding = "UTF-8")
colnames(census) <- colnames

exprRes <- "census_fqs_delta0.7_alpha_0.5.txt"
fqs <- readLines(exprRes, encoding = "UTF-8")

# separate the first string line in the fqs into column names
qs1 <- unlist(strsplit(fqs[1], "--"))
df <- subset(census, select = qs1)

hist(df[, 1])
hist(table(df[, 2]))          ## hist() cannot be called directly on a factor variable
barplot(prop.table(table(df[, 1],df[, 2])),beside=T)