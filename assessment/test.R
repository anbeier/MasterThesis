census <- read.csv("census.csv", sep = ";")  
colnames(census) <- readLines("census_colnames.txt", encoding = "UTF-8") 

fqsFile <- "census_fqs_delta0.7_alpha0.5.txt"
qs <- readFqsFile(fqsFile)

# split data randomly, outcome value: migration_code_change_in_msa
install.packages("caTools")
library(caTools)
split <- sample.split(census[, "migration code-change in msa"], SplitRatio = 0.65)
training <- subset(census, split == TRUE)
testing <- subset(census, split == FALSE)
# take a small amount of samples
split2 <- sample.split(training[, "migration code-change in msa"], SplitRatio = 0.65)
training2 <- subset(training, split2 == TRUE)    ## take this training set as instance

# a quasi clique
df <- subset(training2, select = qs[[1]])

# replace invalid symbols in column names with "_"
for(i in 1:dim(training2)[2]) {
  colnames(training2)[i] <- gsub(" ", "_", colnames(training2)[i], fixed = TRUE)
  colnames(training2)[i] <- gsub("-", "_", colnames(training2)[i], fixed = TRUE)
}
for(i in 1:dim(df)[2]) {
  colnames(df)[i] <- gsub(" ", "_", colnames(df)[i], fixed = TRUE)
  colnames(df)[i] <- gsub("-", "_", colnames(df)[i], fixed = TRUE)
}

install.packages("gdata")
library(gdata)
dfInt <- mapFactorInt(df)

# plot two histograms together
# the following two varibales are integers
hist(df$age, col = "pink")
hist(df$weeks_worked_in_year, add = T, col = "green")

# or using ggplot2 package
age = data.frame("length" = df$age)
workweeks = data.frame("length" = df$weeks_worked_in_year)
age$veg = "age"
workweeks$veg = "work weeks"
vegLengths = rbind(age, workweeks)
ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)
## ggplot(vegLengths, aes(length, fill = veg)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')


# svm
install.packages("kernlab")
library(kernlab)
migrationTraining2Model <- ksvm(migration_code_change_in_msa ~., data = training2, type = "C-svc", kernel = "rbfdot", 
                                kpar = list(sigma = 0.5), C = 1, prob.model = TRUE)

migrationCliqueModel <- ksvm(migration_code_change_in_msa ~., data = df, type = "C-svc", 
                   kernel = "rbfdot", kpar = list(sigma = 0.1), C = 10, prob.model = TRUE)

migrationIntCliqueModel <- ksvm(migration_code_change_in_msa.factor ~., data = dfInt, type = "C-svc", 
                             kernel = "rbfdot", kpar = list(sigma = 0.1), C = 10, prob.model = TRUE)
# plot(trainModel, training, age ~ education, slice = list(age = 3, education = 4))
predict(trainModel, x, type = "probabilities")

x <- subset(df, age >= 16 & age <= 65)
plot(x$age, x$weeks_worked_in_year, xlab = "Age", ylab = "Weeks Worked In Year")
points(x$age[x[, 3] == "All other" & x[, 4] == "Nonmover"], 
       x$weeks_worked_in_year[x[, 3] == "All other" & x[, 4] == "Nonmover"], col = "blue")
points(x$age[x[, 2] == "Masters degree(MA MS MEng MEd MSW MBA)" | x[, 2] == "Bachelors degree(BA AB BS)"], 
       x$weeks_worked_in_year[x[, 2] == "Masters degree(MA MS MEng MEd MSW MBA)" | x[, 2] == "Bachelors degree(BA AB BS)"], col = "pink")


