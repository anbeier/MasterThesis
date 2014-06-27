census <- read.csv("census.csv", sep = ";")
colnames <- readLines("census_colnames.txt", encoding = "UTF-8")
colnames(census) <- colnames

exprRes <- "census_fqs_delta0.7_alpha_0.5.txt"
fqs <- readLines(exprRes, encoding = "UTF-8")

# separate the first string line in the fqs into column names
qs1 <- unlist(strsplit(fqs[1], "--"))
df1 <- subset(census, select = qs1)
