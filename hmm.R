#install.packages("HMM")
#install.packages("depmixS4")

library(HMM)
library(data.table)

setwd('/Users/PranavKulkarni/Documents/Notes Books and Assignments/ALDA/Project/grockit_data/')
data2 <- fread("sampled_training.csv", header=T, sep=",")
data3 <- fread("sampled_test.csv", header=T, sep=",")

users <- unique(data2[,user_id])
Abilities = data.table(user_id = users) # create a temp table of only users and their abilities
Abilities[, c("ability") := list(0)]
 
classLabels <- c("1", "0")
transProbs=matrix(c(0.80, 0.2, 0.2, 0.8),2)
emissionProbs = matrix(c(0.7, 0.3, 0.3, 0.7),2)

for(i in (1:length(users))) {
  
  cat(sprintf("********userid=\"%d\n", users[i]))
  x = data2[user_id == users[i]]$correct
  
  observation = character(length = length(x))
  for(j in (1:length(x))) {
    if(x[j] == 0)
    {
      observation[j] = "0"
    }
    else
    {  
      observation[j] = "1" 
    }
    
  }
  
  hmm <- initHMM(c("L", "UL"), classLabels, transProbs = transProbs, emissionProbs = emissionProbs)
  post <- t(posterior(hmm, observation))
  remove(observation)
  
  UL_ability = post[length(post)]
  if(UL_ability > 0.5) {
    Abilities[i]$ability = 0
  }
  else {
    Abilities[i]$ability = 1
  }
  
}

predictions = rep(0.5, nrow(data3))
for (row_id in (1:nrow(data3))) {
  #print(sprintf("Predicting for row %0.0f.",row_id))
  usr = data3[row_id]$user_id
  cat(sprintf("********userid=\"%d\n", usr))
  predictions[row_id] = Abilities[user_id == usr]$ability
}


write.csv(predictions, file="prediction_hmm.csv", row.names=FALSE)

