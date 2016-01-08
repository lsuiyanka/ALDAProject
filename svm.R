library(data.table)
library(e1071)

setwd('/Users/PranavKulkarni/Documents/Notes Books and Assignments/ALDA/Project/grockit_data/')
data2 <- fread("sampled_training.csv", header=T, sep=",")
data3 <- fread("sampled_test.csv", header=T, sep=",")

users <- unique(data2[,user_id])
data2[, c("question_type", "group_name", "question_id") := NULL]
predictedValues <- numeric(length = length(users))

###SVM sigmoid
for(i in (1:length(users))) {
  cat(sprintf("********count=\"%d\n", i))
  cat(sprintf("********userid=\"%d\n", users[i]))
  userDataset <- data2[user_id == users[i]]
  userDataset[, c("user_id") := NULL]
  #View(userDataset)
  trainX <- as.matrix(userDataset)
  trainY = trainX[,1]
  trainX <- trainX[, -1] #removes first column
  svmModel <- svm(trainX, trainY, type = 'C-classification', kernel = 'sigmoid', gamma = 1/3)
  userTestDataset <- data3[user_id == users[i]]
  userTestDataset[, c("question_type", "group_name", "question_id", "user_id", "correct") := NULL]
  #View(userTestDataset)
  testX <- as.matrix(userTestDataset)
  #testX <- testX[, -1]
  svmModelY = as.matrix(predict(svmModel, (testX)))
  predictedValues[i] = as.numeric(svmModelY[1][1])
}

write.csv(predictedValues, file="prediction_svm_sigmoid.csv", row.names=FALSE)


###SVM polynomial
for(i in (1:length(users))) {
  cat(sprintf("********count=\"%d\n", i))
  cat(sprintf("********userid=\"%d\n", users[i]))
  userDataset <- data2[user_id == users[i]]
  userDataset[, c("user_id") := NULL]
  #View(userDataset)
  trainX <- as.matrix(userDataset)
  trainY = trainX[,1]
  trainX <- trainX[, -1] #removes first column
  svmModel <- svm(trainX, trainY, type = 'C-classification', kernel = 'polynomial', gamma = 1/7)
  userTestDataset <- data3[user_id == users[i]]
  userTestDataset[, c("question_type", "group_name", "question_id", "user_id", "correct") := NULL]
  #View(userTestDataset)
  testX <- as.matrix(userTestDataset)
  #testX <- testX[, -1]
  svmModelY = as.matrix(predict(svmModel, (testX)))
  predictedValues[i] = as.numeric(svmModelY[1][1])
}

write.csv(predictedValues, file="prediction_svm_polynomial.csv", row.names=FALSE)


###SVM polynomial quadratic
for(i in (1:length(users))) {
  cat(sprintf("********count=\"%d\n", i))
  cat(sprintf("********userid=\"%d\n", users[i]))
  userDataset <- data2[user_id == users[i]]
  userDataset[, c("user_id") := NULL]
  #View(userDataset)
  trainX <- as.matrix(userDataset)
  trainY = trainX[,1]
  trainX <- trainX[, -1] #removes first column
  svmModel <- svm(trainX, trainY, type = 'C-classification', kernel = 'polynomial', gamma = 1/7, degree = 2)
  userTestDataset <- data3[user_id == users[i]]
  userTestDataset[, c("question_type", "group_name", "question_id", "user_id", "correct") := NULL]
  #View(userTestDataset)
  testX <- as.matrix(userTestDataset)
  #testX <- testX[, -1]
  svmModelY = as.matrix(predict(svmModel, (testX)))
  predictedValues[i] = as.numeric(svmModelY[1][1])
}

write.csv(predictedValues, file="prediction_svm_polynomial_quadratic.csv", row.names=FALSE)


###SVM radial
for(i in (1:length(users))) {
  cat(sprintf("********count=\"%d\n", i))
  cat(sprintf("********userid=\"%d\n", users[i]))
  userDataset <- data2[user_id == users[i]]
  userDataset[, c("user_id") := NULL]
  #View(userDataset)
  trainX <- as.matrix(userDataset)
  trainY = trainX[,1]
  trainX <- trainX[, -1] #removes first column
  svmModel <- svm(trainX, trainY, type = 'C-classification', kernel = 'radial', gamma = 1/3)
  userTestDataset <- data3[user_id == users[i]]
  userTestDataset[, c("question_type", "group_name", "question_id", "user_id", "correct") := NULL]
  #View(userTestDataset)
  testX <- as.matrix(userTestDataset)
  #testX <- testX[, -1]
  svmModelY = as.matrix(predict(svmModel, (testX)))
  predictedValues[i] = as.numeric(svmModelY[1][1])
}

write.csv(predictedValues, file="prediction_svm_radial.csv", row.names=FALSE)
