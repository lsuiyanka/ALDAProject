#clear workspace
rm(list = ls(all = T))
library(data.table)
library('lme4')

setwd('/Users/PranavKulkarni/Documents/Notes Books and Assignments/ALDA/Project/grockit_data/')

# read in training data
training = fread("sampled_training.csv", header=T, sep=",")
# read in test data
test <- fread("sampled_test.csv", header=T, sep=",")

# logit function for computing probabilities
logit <- function(x) {
  (1 + exp(-x))^-1
}

## fit models

# fit a basic model for each track separately
track_models = list()
for (track in unique(training$track_name)) {
  
  print(sprintf("Starting model for track %s.",track))
  rasch = lmer(correct ~ 1 + (1|user_id) + (1|question_id) + (1|answer_time), data=training[training$track_name==track,c("correct","user_id","question_id","answer_time")], family=binomial, REML=FALSE)
  print(rasch)
  
  # get the constant term
  constant = fixef(rasch)["(Intercept)"]
  
  # get the estimated per-question 'random' effects into a named vector
  question_est = as.vector(t(ranef(rasch)[['question_id']]))
  names(question_est) = rownames(ranef(rasch)[['question_id']])
  
  # get the estimated per-user 'random' effects into a named vector
  user_est = as.vector(t(ranef(rasch)[['user_id']]))
  names(user_est) = rownames(ranef(rasch)[['user_id']])
  
  # get the estimated per-user 'random' effects into a named vector
  at_est = as.vector(t(ranef(rasch)[['answer_time']]))
  names(at_est) = rownames(ranef(rasch)[['answer_time']])
  
  # store these coefficients for later use
  track_models[[as.character(track)]] = list(constant=constant, question_est=question_est, user_est=user_est,at_est=at_est)
  print(sprintf("Finished with model for %s.",track))
}

## make predictions

# use those model parameters to predict the probability of each test example, using the appropriate track model
predictions = rep(0.5, nrow(test))
for (row_id in (1:nrow(test))) {
  #print(sprintf("Predicting for row %0.0f.",row_id))
  user_id = test[row_id,"user_id"]
  question_id = test[row_id,"question_id"]
  answer_time = test[row_id,"answer_time"]
  track = test[row_id, "track_name"]
  #print(sprintf("using track %s",track))
  model_info = track_models[[as.character(track)]]
  # get the logit of the sum -- if any of the parameters have no estimates/are NA, we effectively use 0 as their estimate
  predictions[row_id] = logit(sum(c(model_info[["constant"]], model_info[["question_est"]][as.character(question_id)], model_info[["user_est"]][as.character(user_id)], model_info[["at_est"]][as.character(answer_time)]), na.rm=TRUE))
}

## output

# output the predictions as a csv
prediction_df = data.frame(user_id = test$user_id, outcome=predictions)
write.csv(prediction_df, file="prediction_lmer.csv", row.names=FALSE)
