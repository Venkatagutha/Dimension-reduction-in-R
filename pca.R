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

library(caret)
library(e1071)
#applying PCA to reduce the dimensions of this datasets
pca<- preProcess( x= train[-14], method = "pca", pcaComp = 2 )
train<-predict(pca, train)
train<-train[c(2,3,1)]
test<-predict(pca, test)
test<-test[c(2,3,1)]
#building an svm classifier
svm_classifier<- svm(formula= Customer_Segment~.,
                 data = train,
                 type="C-classification",
                 kernel="linear")
#prediction vector
y_pred<- predict(svm_classifier, newdata = test[-3])

#building the confusion matrix to check accuracy
cm<- table(test[,3],y_pred)
