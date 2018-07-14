#LDA
#Set your working directory to wherever your csv file is located and then read the file
data<-read.csv("Wine.csv")
#splitting the data
library(caTools)
set.seed(123)
split<- sample.split(data$Customer_Segment, SplitRatio = 0.8)
train<- subset(data, split==T)
test<- subset(data,split==F)
#scaling all the independent variables to deal with outliers
train[-14]<-scale(train[-14])
test[-14]<-scale(test[-14])


library(MASS)
#applying PCA to reduce the dimensions of this datasets
lda<-lda(formula= Customer_Segment~., data = train)
train<- predict(lda, train)
train<-as.data.frame(train)
train<- train[c(5,6,1)]
test<- predict(lda,test)
test<- as.data.frame(test)
test<- test[c(5,6,1)]
#building an svm classifier
library(e1071)
svm_classifier<- svm(formula= class~.,
                     data = train,
                     type="C-classification",
                     kernel="linear")
#prediction vector
y_pred<- predict(svm_classifier, newdata = test[-3])

#building the confusion matrix to check accuracy
cm<- table(test[,3],y_pred)
