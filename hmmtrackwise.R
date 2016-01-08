#install.packages("HMM")
#install.packages("depmixS4")

library(HMM)
library(data.table)

setwd('/Users/PranavKulkarni/Documents/Notes Books and Assignments/ALDA/Project/grockit_data/')
data2 <- fread("sampled_training.csv", header=T, sep=",")
data3 <- fread("sampled_test.csv", header=T, sep=",")

users <- unique(data2[,user_id])
abilitiesTrackWise = data.table(user_id = users) # create a temp table of only users and their abilities
abilitiesTrackWise[, c("0","1","2","3","4","5","6","7","8") := list(0)]

classLabels <- c("1", "0")
transProbs=matrix(c(0.80, 0.2, 0.2, 0.8),2)
emissionProbs = matrix(c(0.7, 0.3, 0.3, 0.7),2)

for(i in (1:length(users))) {
  
  z = data2[user_id == users[i]]
  uniqueTracks = unique(z$track_name)
  
  for(j in (1:length(uniqueTracks))) {
    
    #get data for this user for this particular track only
    trackWiseData <- z[track_name == uniqueTracks[j]]
    
    cat(sprintf("********userid=\"%d\n", users[i]))
    cat(sprintf("********trackId=\"%d\n", uniqueTracks[j]))
    x = trackWiseData$correct
    
    observation = character(length = length(x))
    for(k in (1:length(x))) {
      if(x[k] == 0)
      {
        observation[k] = "0"
      }
      else
      {  
        observation[k] = "1" 
      }
      
    }
    
    hmm <- initHMM(c("L", "UL"), classLabels, transProbs = transProbs, emissionProbs = emissionProbs)
    
    if(length(observation) > 1) { #posterior probability requires number of observations to be more than 1
      post <- t(posterior(hmm, observation))
      remove(observation)
      UL_ability = post[length(post)]
    }
    else {
      UL_ability = as.numeric(observation[1])
      remove(observation)
    }
    
    if(UL_ability > 0.5) {
      temp = toString(0,width=1)
      abilitiesTrackWise[[temp]][i] = 0 # this one is quite opposite - select the jth column first and update its ith row
    }
    else {
      temp = toString(uniqueTracks[j],width=1)
      abilitiesTrackWise[[temp]][i] = 1 # this one is quite opposite - select the jth column first and update its ith row
    }
    
  }
}

#View(abilitiesTrackWise)

predictions = rep(0.5, nrow(data3))

for (row_id in (1:nrow(data3))) {
  print(sprintf("Predicting for row %0.0f.",row_id))
  testRow = data3[row_id]$user_id
  trackIde = data3[row_id]$track_name
  temp = toString(trackIde,width=1)
  abilitiesOfThisUser <- abilitiesTrackWise[user_id == testRow] # this will give only one row containing all track abilities
  predictions[row_id] = abilitiesOfThisUser[[temp]][1]
}

#View(predictions)
hmm_trackWise = data.frame(user_id = data3$user_id, outcome=predictions)
#View(hmm_trackWise)
write.csv(hmm_trackWise, file="prediction_hmm_trackwise.csv", row.names=FALSE)


