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
tab<-table(p2, test$stabf)
tab
#mis caluclation
1-sum(diag(tab))/sum(tab)
sum(diag(tab))/sum(tab)

#sensitivity
#0.9846709
#specificity
#0.9918919
