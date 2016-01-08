rm(list = ls(all = T))
setwd('/Users/PranavKulkarni/Documents/Notes Books and Assignments/ALDA/Project/grockit_data/')

library(data.table)
library(cluster)
library(fpc)

data1 <- fread("sampled_training.csv", header=T, sep=",")

data2 <- data1[group_name == 1]
#Step 1: Assign Difficuty levels to each question
questionsVec <- unique(data2$question_id)
questionDifficultyDT = data.frame(question_id = questionsVec)
zeroValues <- rep(0, length(questionsVec))
questionDifficultyDT[,"difficulty_level"] <- zeroValues

ser1 =1
for(q in questionsVec) {
  cat(sprintf("ser_id=\"%d\" ques_id\"%d\ \n", ser1 , q))
  ser1 = ser1 +1
  noOfques <- nrow(data2[question_id == q])
  noOfCorrectques <- nrow(data2[question_id == q & correct == 1])
  quesDiff <- noOfCorrectques/noOfques
  qSubSet <- data2[question_id==q]
  questionDifficultyDT$difficulty_level[questionDifficultyDT$question_id == q ] <- quesDiff
}

km <- kmeans(questionDifficultyDT[,c("difficulty_level")] ,centers=3)
plot(questionDifficultyDT[,c("difficulty_level")], type='p',col=km$cluster, xlab = "Question id", ylab = "Difficulty level")
text(questionDifficultyDT, labels= questionDifficultyDT$question_id, col=km$cluster)


