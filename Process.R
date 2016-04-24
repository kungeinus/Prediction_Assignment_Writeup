library(ggplot2)
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)





trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (!file.exists("training.csv"))
        download.file(trainUrl, "training.csv", method="auto")
if (!file.exists("testing.csv"))
        download.file(testUrl, "testing.csv", method="auto")
training<-read.csv("training.csv")
testing<-read.csv("testing.csv")

trainingNZV <- nearZeroVar(training, saveMetrics=TRUE)
training<-training[,!trainingNZV$nzv]
nanum<-apply(is.na(training),2,sum)
training<-training[,!(nanum/dim(training)>0.8)]
training<-training[,-1]
colindex<-NULL
for (i in 1:length(colnames(training))){
        temp<-grep(colnames(training)[i],colnames(testing))
        if(length(temp)>0)
                colindex[i]<-temp
}
testing<-testing[,colindex]

set.seed(9527)
inTrain<-createDataPartition(training$classe, p = 0.6, list = FALSE)
Mytraining<-training[inTrain,]
Mytesting<-training[-inTrain,]