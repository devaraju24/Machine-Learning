install.packages("class")
install.packages("gmodels")

library(class)
library(gmodels)

dataknn  <- read.csv("Data_for_UCI_named.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(dataknn)
sapply(dataknn,function(x) sum(is.na(x)))
#View(dataknn)
str(dataknn)

# it will normalize the data into 0 -1 ranges
normalize <- function(x) {    return((x - min(x)) / (max(x) - min(x)))  } 
kn_data <- as.data.frame(lapply(dataknn[-14], normalize))


# to improve the knn model accuracy using the scal function
data <- as.data.frame(scale(dataknn[-14])) 


knn_train <- kn_data[1:7000, ] 
knn_test <- kn_data[7001:10000, ] 

#View(knn_train)

knn_train <- data[1:7000, ] 
knn_test <- data[7001:10000, ] 


knn_train_labels <- dataknn[1:7000, 14] 
knn_test_labels <- dataknn[7001:10000, 14] 

#View(knn_train_labels)

knn_pred <- knn(train=knn_train,test=knn_test, cl=knn_train_labels,k=83)
#0.903333
knn_pred <- knn(train=knn_train,test=knn_test, cl=knn_train_labels,k=55)
plot(knn_pred)
#Accuracy 0.9103333,,0.9386667
knn_pred <- knn(train=knn_train,test=knn_test, cl=knn_train_labels,k=35)
#0.9076667,0.9395

CrossTable(x = knn_test_labels, y = knn_pred,  prop.chisq=FALSE)



#sensitivity a/a+c  
#0.9769392
#specificity  stable/stable+unstable
#0.9206271   d/d+b







