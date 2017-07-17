# Project - Survival Analysis
# Author: Emilie Yao
# setwd("/Users/hsinyuyao/Desktop/95-791 DM/project")

# =====================================
# setting up
# =====================================

# import package
library(randomForestSRC) # if not installed yet: install.packages("randomForestSRC")
library(ggRandomForests) # if not installed yet: install.packages("ggRandomForests")

# load utility functions
import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE, strip.white=TRUE))}
write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)}

trainset<- read.csv("aggregate_train.csv")
testset<- read.csv("aggregate_test.csv")

# READ THE DOCUMENTATION PLEASE
?rfsrc
?predict.rfsrc


# =====================================
# Analysis
# =====================================

# grow survival forest 
grow <- rfsrc(Surv(accum_subs, churn) ~ ., trainset, ntree = 200, tree.err=TRUE, nsplit=2, importance="random.ensemble", var.used="all.trees")
print(grow)
sort(grow$importance, decreasing = T) # view the importance of variables
print(grow$var.used[order(-grow$var.used)]) # view the number of times that a variable is used in the forest

# make predictions on the grow forest
pred <- predict(grow, testset, outcome = "train")
print(pred)

# view the predictions (of first 3 test instances)
t(pred$chf[1:3,]) # cumulative hazard function
t(pred$survival[1:3,]) # survival rate



# =====================================
# Plotting
# =====================================
library(ggplot2)
library(reshape2)

# error rate over all trees
plot(gg_error(grow)) 

# variable importance plot
plot(gg_vimp(grow)) 

# survival curve for the first 3 test instances
Time <- pred$time.interest # unique event times
surv <- data.frame(time=Time, customer=t(pred$survival[1:3,]))
long <- melt(surv, id="time")  # convert to long format
ggplot(data=long, aes(x=time, y=value, colour=variable)) + geom_line() + 
  labs(x="Time",y="Survival Rate") + ggtitle("Survival Curve for 3 Test Data") 

# using plot.survival (also generates hazard plots)
plot.survival(pred, subset = 1:3, haz.model = "ggamma")


# risksetROC library provides functions to compute the equivalent to a ROC curve 
# and its associated Area Under Curve (AUC) in a time-dependent context
library(risksetROC)

# ROC curve at a specific event time 
w.ROC = risksetROC(Stime = trainset$accum_subs,  
                   status = trainset$churn, 
                   marker = grow$predicted.oob, 
                   predict.time = 20,
                   main = paste("Survival ROC Curve at month ", 20), 
                   lwd = 3, 
                   col = "red" )
w.ROC$AUC # AUC at this event time

# AUC scores over the study period (48 months)
w.ROC = risksetAUC(Stime = trainset$accum_subs,  
                   status = trainset$churn, 
                   marker = grow$predicted.oob,
                   tmax = 48)



# =====================================
# Experiments (not validated yet, ignore this part)
# =====================================

# non-standard predict call: overlays the test data on the grow forest
pred.test <- predict(grow, testset, outcome = "test")
# check forest reproducibilility by comparing "test" predicted survival curves 
# to "train" predicted survival curves for the first 3 individuals
matplot(Time, t(exp(-pred$chf)[1:3,]), ylab = "Survival", col = 1, type = "l")
matlines(Time, t(exp(-pred.test$chf)[1:3,]), col = 2)


# churn rate for each individual calibrated to the scale of the number of events (48)
pred_churn_rate <- pred$predicted / 48
pred_churn_class <- ifelse(churn_rate >= 0.5, 1, 0)
confusion_matrix <- table(pred_churn_class, testset$churn)
performance <- data.frame(accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix),
                          recall = confusion_matrix[2,2] / sum(confusion_matrix[,2]),
                          precision = confusion_matrix[2,2] / sum(confusion_matrix[2,]))
performance

# plotting hazard function
chf <- data.frame(time=Time, customer=t(exp(-pred$chf)[1:3,]))
long <- melt(surv, id="time")  # convert to long format
ggplot(data=long, aes(x=time, y=value, colour=variable)) + geom_line() + 
  labs(x="Time",y="Rate") + ggtitle("Hazard Function of 3 Test Data") 


# tune input attributes (use features from feature selection)
# tune number of trees 

