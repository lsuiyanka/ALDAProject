#clear workspace
rm(list = ls(all = T))
library(data.table)

setwd('/Users/PranavKulkarni/Documents/Notes Books and Assignments/ALDA/Project/grockit_data/')
data2 <- fread("training.csv", header=T, sep=",")
data3 <- fread("test.csv", header=T, sep=",")

#consider data which have only correct or incorrect as outcome
data2 <- data2[outcome %in% c(1,2)]

userWise <- data2[,.N,by=user_id]
morethan500 <- userWise[N >= 500]

newTraining <- data2[user_id %in% morethan500$user_id]
newTest <- data3[user_id %in% morethan500$user_id]

#create time taken to answer in training
x = as.POSIXct(newTraining$deactivated_at, format = "%Y-%m-%d %H:%M:%S")
y = as.POSIXct(newTraining$round_started_at, format = "%Y-%m-%d %H:%M:%S")
answer_time <- difftime(x,y,units = "secs")
newTraining <- cbind(newTraining, answer_time)

#remove unwanted features from training
newTraining <- newTraining[, c("outcome", "game_type", "num_players", "tag_string", "answer_id", "question_set_id", "date_of_test", "deactivated_at", "round_started_at", "answered_at") := list(NULL)]

#create time taken to answer in test data
x = as.POSIXct(newTest$deactivated_at, format = "%Y-%m-%d %H:%M:%S")
y = as.POSIXct(newTest$round_started_at, format = "%Y-%m-%d %H:%M:%S")
answer_time <- difftime(x,y,units = "secs")
newTest <- cbind(newTest, answer_time)

#remove unwanted features from test data
newTest <- newTest[, c("game_type", "num_players", "tag_string", "question_set_id", "date_of_test", "deactivated_at", "round_started_at", "answered_at") := list(NULL)]

#View(newTraining)
#View(newTest)

write.csv(newTraining, file = "sampled_training.csv", row.names = FALSE)
write.csv(newTest, file = "sampled_test.csv", row.names = FALSE)

#end of sampling and basic preprocessing


