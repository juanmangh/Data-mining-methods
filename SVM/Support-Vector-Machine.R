#Install and load the svm package
#install.packages("e1071")
library("e1071")

#Iris data set to be used
summary(iris)
dataset <- iris

#Divide the variables
x <- subset(dataset, select=-Species)
y <- Species

#Create SVM model and show summary
svm_model <- svm(Species ~ ., data=dataset)
summary(svm_model)
help("svm")

#Another option
svm_model1 <- svm(x,y)
summary(svm_model1)

pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

table(pred,y)

#Selection of SVM model with a data driven approach: various combinations need to be examined depending on the data
svm_tune <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
