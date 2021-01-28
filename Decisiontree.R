getwd()
setwd("C:/Users/devar/Downloads/")

####decision tree with party package
library(party)
library(readxl)
library(ROCR)
dec_data <- read_excel("roomoccdataset.xlsx")
str(dec_data)
summary(dec_data)
View(dec_data)

dec_data$Occupancy <- as.factor(dec_data$Occupancy)


set.seed(1234)
indx <- sample(2,nrow(dec_data), replace=T, prob=c(0.7,0.3))
dec_training <- dec_data[indx ==1,]
dec_test <- dec_data[indx == 2,]


#postpruning default 
tree <- ctree(Occupancy~Humidity+Light+CO2+HumidityRatio, data=dec_training)

#prepruning and reducing the tree size
tree <- ctree(Occupancy~Humidity+Light+CO2+HumidityRatio, data=dec_training, controls=ctree_control(mincriterion = 0.99,minsplit=500))
plot(tree)

#misclassification error for training data
trainpred<-predict(tree,newdata=dec_training)
tab<-table(trainpred,dec_training$Occupancy)
print(tab)
1-sum(diag(tab))/sum(tab)


#misclassification error for test data
testpred<-predict(tree,newdata=dec_test)
tab2<-table(testpred,dec_test$Occupancy)
print(tab2)
1-sum(diag(tab2))/sum(tab2)
sum(diag(tab2))/sum(tab2)
#Accuracy for postproning
#0.991
#Acuuracy for preproning
#0.987

table(dec_test$Occupancy)
#roc curve
eeee_dec<- prediction(testpred,dec_test$Occupancy)
roc_dec <- performance(eeee_dec, "tpr","fpr")

plot(roc_dec,colorize=T,main="ROC CURVE",ylab="Sensitivity",xlab="1-Specificity")
#with out anymodel if everything was not occupied in test data we will get that curve model does worst the curve will be at down
#Classifier without any predicted value
abline(a=0,b=1)
