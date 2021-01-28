
#naivebayes factor conversion. binning.

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
data1   <- read.csv("Data_for_UCI_named.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(data1)
View(data1)
summary(data1)
pairs.panels(data1)


data1$tau1<-cut(data1$tau1,breaks=c(0,5.2500,max(data1$tau1)),labels=c("0-1","1+"))
data1$tau2<-cut(data1$tau2,breaks=c(0,5.2500,max(data1$tau2)),labels=c("0-1","1+"))
data1$tau3<-cut(data1$tau3,breaks=c(0,5.2500,max(data1$tau3)),labels=c("0-1","1+"))
data1$tau4<-cut(data1$tau4,breaks=c(0,5.2497,max(data1$tau4)),labels=c("0-1","1+"))

data1$p1<-cut(data1$p1,breaks=c(1.5,3.751,max(data1$p1)),labels=c("0-1","1+"))
data1$p2<-cut(data1$p2,breaks=c(-2,-1.2500,max(data1$p2)),labels=c("0-1","1+"))
data1$p3<-cut(data1$p3,breaks=c(-2,-1.2500,max(data1$p3)),labels=c("0-1","1+"))
data1$p4<-cut(data1$p4,breaks=c(-2,-1.2500,max(data1$p4)),labels=c("0-1","1+"))

data1$g1<-cut(data1$g1,breaks=c(0.05,0.52501,max(data1$g1)),labels=c("0-1","1+"))
data1$g2<-cut(data1$g2,breaks=c(0.05,0.52500,max(data1$g2)),labels=c("0-1","1+"))
data1$g3<-cut(data1$g3,breaks=c(0.05,0.52501,max(data1$g3)),labels=c("0-1","1+"))
data1$g4<-cut(data1$g4,breaks=c(0.05,0.52500,max(data1$g4)),labels=c("0-1","1+"))

data1$stab<-cut(data1$stab,breaks=c(-0.09,0.01714,max(data1$stab)),labels=c("0-1","1+"))

set.seed(1234)

ind <- sample(2,nrow(data1), replace=T, prob=c(0.7,0.3))
training <- data1[ind ==1,]
test <- data1[ind == 2,]


# by default laplace is zero 
model <- naive_bayes(stabf~., data=training,   usekernel = T)
model

#laplace function to improve model assigning laplace to 1 
model <- naive_bayes(stabf~., data=training, laplace = 1,  usekernel = T)
model
plot(model)




#predict
#p <- predict(model ,training ,type ='prob')
#head(cbind(p,training))


#confusion matrix

p1<- predict(model,training)
tab<-table(p1, training$stabf)
tab
#miscalucalation
1-sum(diag(tab))/sum(tab)

#test
p2<- predict(model,test)
tabe<-table(p2, test$stabf)
tabe
#mis caluclation
1-sum(diag(tab))/sum(tab)

sum(diag(tab))/sum(tab)

#Accuracy for the model

table(data1$stab)
hist(data1$tau1)



################################################# Without binning #########################################

installed.packages("naivebayes")

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

nv_data   <- read.csv("Data_for_UCI_named.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(nv_data)

View(nv_data)
sapply(nv_data ,function(x) sum(is.na(x)))



# when developing the naive bayes the independent variables are not highly correlated
pairs.panels(nv_data[-14])

set.seed(1234)
ind <- sample(2,nrow(nv_data), replace=T, prob=c(0.7,0.3))

training <- nv_data[ind ==1,]
test <- nv_data[ind == 2,]


#Fitting the model 
model <- naive_bayes(stabf~., data=training, usekernel = T)
model


#laplace function to improve model


#predict
#p <- predict(model ,training ,type ='prob')
#head(cbind(p,training))

#confusion matrix
p1<- predict(model,training)
tab<-table(p1, training$stabf)
tab
#miscalucalation
1-sum(diag(tab))/sum(tab)

#test
p2<- predict(model,test)
tabe<-table(p2, test$stabf)
tabe
#mis caluclation
1-sum(diag(tab))/sum(tab)
sum(diag(tab))/sum(tab)

#sensitivity
#0.9846709
#specificity
#0.9918919
