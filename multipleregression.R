
getwd()
setwd("C:/Users/devar/Downloads/")
library(readxl)
library(psych)
library(car)
library(caret)

reg_data <- read_excel("winedata.xlsx")
str(reg_data)
summary(reg_data)
View(reg_data)

par(mfrow=c(2,2))
for(i in 1:length(reg_data))
    {barplot(prop.table(table(reg_data[,i])) ,
         xlab=names(reg_data[i]), ylab= "alcohol (%)" , col=rainbow(3))}

pairs.panels(reg_data)

set.seed(1234)
indx <- sample(2,nrow(reg_data), replace=T, prob=c(0.7,0.3))
reg_training <- reg_data[indx ==1,]
reg_test <- reg_data[indx == 2,]

#Building the model

res <- lm(alcohol~.,data=reg_training)
summary(res) #r2 is 0.8123
par(mfrow=c(2,2))
plot(res)

#Cooks distance cutofff
#Eliminating the extreme values from the data

cutoff <- 4/((nrow(reg_training)-length(res$coefficients)-2))
plot(res,which=4, cook.levels = cutoff)
plot(res,which=5, cook.levels = cutoff)

reg_training <- reg_training[-which(rownames(reg_training) %in% c("1177","3615","4381")),]

# Refitting the the model(2)
res2 <- lm(alcohol~.,data=reg_training)
summary(res2) #r2 is 0.8144


cutoff <- 4/((nrow(reg_training)-length(res$coefficients)-2))
plot(res,which=4, cook.levels = cutoff) # which 4 gives the cook's distance plot
plot(res,which=5, cook.levels = cutoff) # which 5 gives the Residuals vs Levarage

reg_training <- reg_training[-which(rownames(reg_training) %in% c("1184","4417")),]


# Refitting the the model(3)
res3 <- lm(alcohol~.,data=reg_training)
summary(res3) #r2 is 0.8159


cutoff <- 4/((nrow(reg_training)-length(res$coefficients)-2))
plot(res,which=4, cook.levels = cutoff) # which 4 gives the cook's distance plot
plot(res,which=5, cook.levels = cutoff) # which 5 gives the Residuals vs Levarage

reg_training <- reg_training[-which(rownames(reg_training) %in% c("1004","2756","4415")),]

# Refitting the the model(4)
res4 <- lm(alcohol~.,data=reg_training)
summary(res4) #r2 is 0.8187



cutoff <- 4/((nrow(reg_training)-length(res$coefficients)-2))
plot(res,which=4, cook.levels = cutoff) # which 4 gives the cook's distance plot
plot(res,which=5, cook.levels = cutoff) # which 5 gives the Residuals vs Levarage

reg_training <- reg_training[-which(rownames(reg_training) %in% c("3622","3633")),]

# Refitting the the model(5)
res5 <- lm(alcohol~.,data=reg_training)
summary(res5) #r2 is 0.8195

cutoff <- 4/((nrow(reg_training)-length(res$coefficients)-2))
plot(res,which=4, cook.levels = cutoff) # which 4 gives the cook's distance plot
plot(res,which=5, cook.levels = cutoff) # which 5 gives the Residuals vs Levarage

reg_training <- reg_training[-which(rownames(reg_training) %in% c("3622","3974","4188")),]

# Refitting the the model(6)
res6 <- lm(alcohol~.,data=reg_training)
summary(res6) #r2 is 0.8206

cutoff <- 4/((nrow(reg_training)-length(res$coefficients)-2))
plot(res,which=4, cook.levels = cutoff) # which 4 gives the cook's distance plot
plot(res,which=5, cook.levels = cutoff) # which 5 gives the 


# remoivng column due to p value its not statistically significant.
resfinal <- lm(alcohol~. -`free sulfurdioxide` ,data=reg_training)
summary(resfinal)


#checking for multicolinearity with vif
vif(resfinal)
varImp(resfinal)

#predictor values for both training and test dataset.

reg_training$pr.alcohol <-predict(resfinal, newdata=reg_training)
reg_test$pr.alcohol <-predict(resfinal,newdata=reg_test)




pred1 <- predict(resfinal, newdata = reg_training)
rmse1 <- sqrt(sum((exp(pred1) - reg_training$alcohol)^2)/length(reg_training$alcohol))
c(RMSE = rmse1, R2=summary(resfinal)$r.squared)

par(mfrow=c(1,1))
plot(reg_training$alcohol, exp(pred1))

pred2 <- predict(resfinal, newdata = reg_test)
rmse2 <- sqrt(sum((exp(pred2) - reg_test$alcohol)^2)/length(reg_test$alcohol))
c(RMSE = rmse2, R2=summary(resfinal)$r.squared)

par(mfrow=c(1,1))
plot(reg_test$alcohol, exp(pred2))

