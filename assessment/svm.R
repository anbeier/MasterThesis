# svm
install.packages("kernlab")
library(kernlab)
migrationTraining2Model <- ksvm(migration_code_change_in_msa ~., data = training2, type = "C-svc", kernel = "rbfdot", 
                                kpar = list(sigma = 0.1), C = 10, prob.model = TRUE)

migrationCliqueModel <- ksvm(migration_code_change_in_msa ~., data = df, type = "C-svc", 
                             kernel = "rbfdot", kpar = list(sigma = 0.1), C = 10, prob.model = TRUE)

migrationIntCliqueModel <- ksvm(migration_code_change_in_msa.factor ~., data = dfInt, type = "C-svc", 
                                kernel = "rbfdot", kpar = list(sigma = 0.1), C = 10, prob.model = TRUE)
# plot(trainModel, training, age ~ education, slice = list(age = 3, education = 4))
predict(trainModel, x, type = "probabilities")
