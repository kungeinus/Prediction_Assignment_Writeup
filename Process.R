library(ggplot2)
library(caret)

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
training$classe<-as.factor(training$classe)
colindex<-NULL
for (i in 1:length(colnames(training))){
        temp<-grep(colnames(training)[i],colnames(testing))
        if(length(temp)>0)
                colindex[i]<-temp
}
testing<-testing[,colindex]

set.seed(9527)
inTrain<-createDataPartition(training$classe, p = 0.65, list = FALSE)
Mytraining<-training[inTrain,]
Mytesting<-training[-inTrain,]


mod_rf<-train(classe ~ . , method = "rf" , data = Mytraining)
mod_gbm<-train(classe ~ . , method = "gbm" , data = Mytraining)
mod_lda<-train(classe ~ . , method = "lda" , data = Mytraining)
pred_rf<-predict(mod_rf, newdata = Mytesting)
pred_gbm<-predict(mod_gbm, newdata = Mytesting)
pred_lda<-predict(mod_lda, newdata = Mytesting)
Mycomb_pred<-data.frame(pred_rf, pred_gbm, pred_lda, classe = Mytesting$classe)
mod_comb<-train(classe ~ . , method = "gbm", data = Mycomb_pred)
pred_comb<-predict(mod_comb, newdata = Mytesting)

con_rf<-confusionMatrix(pred_rf, Mytesting$classe)
con_gbm<-confusionMatrix(pred_gbm, Mytesting$classe)
con_lda<-confusionMatrix(pred_lda, Mytesting$classe)
con_comb<-confusionMatrix(pred_comb, Mytesting$classe)

plot(mod_rf$finalModel, main = "Random Forest Model")
plot(con_rf$table,col = "white", main = paste("Random Forest Confusion Matrix: Accuracy =", round(con_rf$overall['Accuracy'], 4)))

plot(mod_gbm, main = "Boosting Model")
plot(con_gbm$table,col = "white", main = paste("Boosting Confusion Matrix: Accuracy =", round(con_gbm$overall['Accuracy'], 4)))

plot(con_lda$table,col = "white", main = paste("Linear Discriminant Analysis Confusion Matrix: Accuracy =", round(con_lda$overall['Accuracy'], 4)))

plot(mod_comb, main = "Combined Model")
plot(con_comb$table,col = "white", main = paste("Combined Confusion Matrix: Accuracy =", round(con_comb$overall['Accuracy'], 4)))

tpred_rf<-predict(mod_rf, newdata = testing)
tpred_gbm<-predict(mod_gbm, newdata = testing)
tpred_lda<-predict(mod_lda, newdata = testing)
Mycomb_tpred<-data.frame(pred_rf=tpred_rf, pred_gbm=tpred_gbm, pred_lda=tpred_lda)
tpred_comb<-predict(mod_comb, newdata = Mycomb_tpred)

problem_id=1:length(tpred_comb)
prediction_final<-data.frame(problem_id,classe=tpred_comb)
write.table(prediction_final, file = "prediction.csv", sep = ",", row.names = FALSE)