library(Hmisc)
library(caret)
library(randomForest)
library(rattle)
library(rpart.plot)
set.seed(2015)
options(warn=-1)
# Download dataset
dataset_Train=read.csv("pml-training.csv",head=T)
dataset_Test=read.csv("pml-testing.csv",head=T)
# Cleaning Data
### First, replace all the blanks ("") and "#DIV/0" with "NA", then remove all the columns which contain NAs
Train_clean=read.csv("pml-training.csv",na.strings=c("","#DIV/0","NA"))
Test_clean=read.csv("pml-testing.csv",na.strings=c("","#DIV/0","NA"))
Train_clean=Train_clean[,colSums(is.na(Train_clean))==0]
Test_clean=Test_clean[,colSums(is.na(Test_clean))==0]
#The features user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_window
#are not related to calculations and are removed form the downloaded data. 
Train_clean=Train_clean[,-c(1:7)]
Test_clean=Test_clean[,-c(1:7)]

# Split Train_clean into training set and testng set
inTrain=createDataPartition(Train_clean$classe,p=0.75,list=FALSE)
training=Train_clean[inTrain,]
testing=Train_clean[-inTrain,]

# Building a model based on Decision Tree
modelFit1=train(classe~.,data=training,method="rpart")
print(modelFit1$finalModel)
# Showing the decision tree

fancyRpartPlot(modelFit1$finalModel,main="Classification Tree")

# Building a model based on Random Forest
modelFit2=randomForest(classe~.,data=training)

# Compare modelFit1 and modelFit2 based the accuracy
Accuracy1=confusionMatrix(testing$classe,predict(modelFit1,testing))
Accuracy2=confusionMatrix(testing$classe,predict(modelFit2,testing))
Accuracy1$overall[1]
Accuracy2$overall[1]
# Accuracy of modelFit1 is 0.4794, and accuracy of modelFit2 is 0.9969, so we would choose
# model to do prediction

# Test Data Submit
#The following submisson code are from project assignment instruction.
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

Prediction=predict(modelFit2,Test_clean)

pml_write_files(Prediction)

library(knitr)
knit2html("Project.Rmd")
