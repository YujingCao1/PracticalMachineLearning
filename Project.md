# Practical Machine Learning Prediction Assignment
### Yujing Cao      1/10/2015

# Goal
In this prediction assignment, we use the provided dataset to predict the manner in which people did the exercise. The following packages are used 

```r
library(Hmisc)
library(caret)
library(randomForest)
library(rattle)
library(rpart.plot)
set.seed(2015)
options(warn=-1)
```
# Download Datasets

```r
dataset_Train=read.csv("pml-training.csv",head=T)
dataset_Test=read.csv("pml-testing.csv",head=T)
```
# Cleaning Data
First, replace all the blanks ("") and "#DIV/0" with "NA", then remove all the columns which contain NAs

```r
Train_clean=read.csv("pml-training.csv",na.strings=c("","#DIV/0","NA"))
Test_clean=read.csv("pml-testing.csv",na.strings=c("","#DIV/0","NA"))
Train_clean=Train_clean[,colSums(is.na(Train_clean))==0]
Test_clean=Test_clean[,colSums(is.na(Test_clean))==0]
```
The features user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_windoware not related to calculations and are removed form the downloaded data. 

```r
Train_clean=Train_clean[,-c(1:7)]
Test_clean=Test_clean[,-c(1:7)]
```
# Split Train_clean into training set and testng set

```r
inTrain=createDataPartition(Train_clean$classe,p=0.75,list=FALSE)
training=Train_clean[inTrain,]
testing=Train_clean[-inTrain,]
```
# Building a model based on Decision Tree

```r
modelFit1=train(classe~.,data=training,method="rpart")
print(modelFit1$finalModel)
```

```
## n= 14718 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 14718 10533 A (0.28 0.19 0.17 0.16 0.18)  
##    2) roll_belt< 130.5 13483  9309 A (0.31 0.21 0.19 0.18 0.11)  
##      4) pitch_forearm< -34.35 1193     2 A (1 0.0017 0 0 0) *
##      5) pitch_forearm>=-34.35 12290  9307 A (0.24 0.23 0.21 0.2 0.12)  
##       10) magnet_dumbbell_y< 438.5 10386  7467 A (0.28 0.18 0.24 0.19 0.11)  
##         20) roll_forearm< 122.5 6465  3835 A (0.41 0.18 0.19 0.16 0.061) *
##         21) roll_forearm>=122.5 3921  2630 C (0.074 0.18 0.33 0.23 0.18) *
##       11) magnet_dumbbell_y>=438.5 1904   948 B (0.034 0.5 0.041 0.23 0.19) *
##    3) roll_belt>=130.5 1235    11 E (0.0089 0 0 0 0.99) *
```

```r
fancyRpartPlot(modelFit1$finalModel,main="Classification Tree")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
# Building a model based on Random Forest

```r
modelFit2=randomForest(classe~.,data=training)
```
# Compare modelFit1 and modelFit2 based the accuracy

```r
Accuracy1=confusionMatrix(testing$classe,predict(modelFit1,testing))
Accuracy2=confusionMatrix(testing$classe,predict(modelFit2,testing))
Accuracy1$overall[1]
```

```
## Accuracy 
## 0.495106
```

```r
Accuracy2$overall[1]
```

```
##  Accuracy 
## 0.9946982
```
Since accuracy of modelFit2 is much higher than modelFit1, we would choose modelFit2 to do prediction.
# Test Data Submit
The following submisson code are from project assignment instruction.

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
Prediction=predict(modelFit2,Test_clean)
Prediction
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

```r
pml_write_files(Prediction)
```
