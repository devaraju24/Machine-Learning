getwd()
setwd("C:/Users/devar/Downloads/")

# Room Occupancy dataset
library(caret)# varimp
library(readxl)
library(car) #vif
library(ROCR)

#Loading data
log_data <- read_excel("roomoccdataset.xlsx")
View(log_data)
str(log_data)
summary(log_data)

pairs.panels(data)
# converting the occupancy column to factor 
log_data$Occupancy <- as.factor(log_data$Occupancy)

set.seed(1234)
indx <- sample(2,nrow(log_data), replace=T, prob=c(0.7,0.3))
log_training <- log_data[indx ==1,]
log_test <- log_data[indx == 2,]


#Fitting the model
log_model <- glm(Occupancy ~., log_training,family='binomial')
summary(log_model)

#Removing the columns which are not significantlly statistic
log_model2 <- glm(Occupancy ~.-date -Temperature, log_training,family='binomial')
summary(log_model2)

# checking the multicolinearity 
vif(log_model2)

log_model3 <- glm(Occupancy ~.-date -Temperature -HumidityRatio, log_training,family='binomial')
summary(log_model3)
vif(log_model3)
varImp(log_model3)


#Confusion Matrix
p1<- predict(log_model3,log_training, type='response')
pred1 <- ifelse(p1>0.5,1,0)
tab<- table(predicted= pred1 ,Actual=log_training$Occupancy)
tab
# Calculating Misclassification Error
1-sum(diag(tab))/sum(tab)


#Confusion Matrix
p2<- predict(log_model3,log_test, type='response')
pred2 <- ifelse(p2>0.5,1,0)
tab2<- table(predicted= pred2, Actual=log_test$Occupancy)
tab2
#mis classification error
1-sum(diag(tab2))/sum(tab2)
#0.01042101
#Accuracy
sum(diag(tab2))/sum(tab2)
#0.989579

#This pseudo R-square calculation is referred to as the Hosmer and Lemeshow R-square.
#r square 0.8731
modelChi <- log_model2$null.deviance - log_model2$deviance
pseudo.R2 <- modelChi / log_model2$null.deviance
pseudo.R2

#Sensitivity 
#0.9471639

#Specificity
#0.989457

#If we predict that all the time room is not occupied in test data we get accuracy of 0.7907
table(log_test$Occupancy)
#1897/2399
#0.7907 our model which we are creating should has more accuracy then this other wise we can not use this model
#just to check our model crossed the benchmark 
  
#Roc curve  
eeee<- prediction(pred2,log_test$Occupancy)
roc <- performance(eeee, "tpr","fpr")
plot(roc,colorize=T,main="ROC CURVE",ylab="Sensitivity",xlab="1-Specificity")
#with out anymodel if everything was not occupied in test data we will get that curve, we can't use the model if curve will be at down
#Classifier without any predicted value
abline(a=0,b=1)
