####Create plot using 1st 9000 records as train and last 1000 records as test
library(ROCR)
##Running KNN, Naive Bayes, SVM
library(class)
library(e1071)
library(randomForest)

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

agg<-import.csv('aggregate.csv')
agg <- agg[sample(nrow(agg)), ]
agg<-agg[,c(-3,-4,-5,-6)]
agg$churn<-as.factor(agg$churn)
testData<-agg[1:3999,]
train<-agg[4000:10000,]

# 
# train$churn<-as.factor(train$churn)
# testData$churn<-as.factor(testData$churn)

SVMmodel <-
  svm(train$churn ~ ., data = train, gamma=0.1,cost=1)

test<-testData
trainv2<-train

test$churn <- ifelse(test$churn == levels(test$churn)[1], 0, 1)
trainv2$churn<- ifelse(trainv2$churn == levels(trainv2$churn)[1], 0, 1)


LogitModel <-
  glm(churn~.,
      family = binomial,
      trainv2)




testv3<-testData
trainv3<-train

testv3$gender <- ifelse(testv3$gender == levels(testv3$gender)[1], 0, 1)
trainv3$gender<- ifelse(trainv3$gender == levels(trainv3$gender)[1], 0, 1)

testv3$churn <- ifelse(testv3$churn == levels(testv3$churn)[1], 0, 1)
trainv3$churn<- ifelse(trainv3$churn == levels(trainv3$churn)[1], 0, 1)

nf1<-ncol(trainv3)
###KNN Model

labels <- trainv3[, nf1]
k <-
  knn(
    subset(trainv3, select = -nf1) ,
    subset(testv3, select = -nf1),
    labels,
    k = 5 ,
    prob = TRUE
  )


#For invoking probabilities by classification
temp_df <- data.frame(pred = k, prob = attr(k, "prob"))

knnPred<-vector()
for(i in 1:nrow(temp_df))
{
if((temp_df$pred[i]==0)&(temp_df$prob[i]>0.5))
  {
 t<- 1-temp_df$prob[i]  
    knnPred<-c(knnPred,t)
}
  else if((temp_df$pred[i]==1)&(temp_df$prob[i]>0.5))
  {
    t<- temp_df$prob[i]  
    knnPred<-c(knnPred,t)
  }
}

p6<-prediction(knnPred,as.vector(testv3$churn))

nbModel <- naiveBayes(churn~ ., data = train)

randomForestModel<-randomForest(churn~.,data=train, ntree=500)

svmPred <- predict(SVMmodel, newdata = testData, type="response")
svmPred <- ifelse(svmPred == levels(svmPred)[1], 0, 1)

test$pred<-predict(LogitModel,test,type="response")

nbResult<-predict(nbModel,testData,type="raw")
nbr<-vector()
for(i in 1:nrow(nbResult))
{
  
  if(nbResult[i,1]>0.5)
  {
    nbr<-c(nbr,nbResult[i,1])
  }
  else
  {
    nbr<-c(nbr,nbResult[i,2])
  }
  
  
}


randomResult<- predict(randomForestModel,testData, type = "response")
randomResult <- ifelse(randomResult == levels(randomResult)[1], 0, 1)

###Default
trainv3<-train

nf <- ncol(trainv3)
trainv3[, nf] <- ifelse(trainv3[, nf] == levels(trainv3[, nf])[1], 0, 1)


c1 <- 0
c0 <- 0

#Checking which label is more common i.e. default
for (i in 1:nrow(trainv3))
{
  if (trainv3[i,nf] == 1)
  {
    c1 <- c1 + 1
  }
  else
  {
    c0 <- c0 + 1
  }
}
if (c1 > c0){
  
  p <- 1
} else
  
{
  p <- 0
}
defPred <- rep(p, nrow(testData))



testData$churn <- ifelse(testData$churn == levels(testData$churn)[1], 0, 1)
p1<-prediction(randomResult,testData$churn)
p2<-prediction(svmPred,testData$churn)
p3<-prediction(test$pred,test$churn)
p4<-prediction(nbr,as.vector(testData$churn))
p5<-prediction(defPred,as.vector(testData$churn))


perf1<-performance(p1,"tpr","fpr")

perf3<-performance(p3,"tpr","fpr")
perf4<-performance(p4,"tpr","fpr")
perf5<-performance(p5,"tpr","fpr")
perf6<-performance(p6,"tpr","fpr")

plot.new()

plot(perf1,col="blue",lwd=2.5)

plot(perf5,add=TRUE,col="red",lwd=2.5)
plot(perf4,add=TRUE,col="grey",lwd=2.5)
plot(perf6,add=TRUE,col="orange",lwd=2.5)
##abline(0,1,col="white",lwd=2.5,lty=2)

title("ROC curves")
legend(0.55,0.55,c("Random Forest","Naive Bayes","Default","KNN"),
       lty=c(1,1,1,1,1),
       lwd=c(1.4,1.4,1.4,1.4,1.4),col=c("blue","grey","red","orange"))
