install.packages("caret")
#install.packages("kernlab")
#install.packages("e1071")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("corrplot")
#install.packages("rattle")
library(caret); library(rpart)
library(rattle); library(rpart.plot)
library(randomForest); library(RColorBrewer)
library(corrplot)

training_data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")

testing_data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

dim(training_data);dim(testing_data)


#clean up training data set by removing missing values
clean_train <- training_data

## pulls out the "classe"
train_classe <- as.data.frame(clean_train[, max(length(clean_train))])
colnames(train_classe) <- "classe"

## function to remove unneeded columns
remove <- function (x) {
  remove <- grepl("^X|timestamp|window", names(x))
}

clean_train <- clean_train[,colSums(is.na(clean_train)) == FALSE]

## applying the function
clean_train <- clean_train[,!remove(clean_train)]

## converting columns to numeric values
clean_train <- clean_train[, sapply(clean_train, is.numeric)]

## adding the "classe" values back in
clean_train <- cbind(clean_train, train_classe)

clean_train

#clean up testing data set by removing missing values
clean_test <- testing_data

## pulls out the grades or "classe"
clean_test <- clean_test[, -max(length(clean_test))]
## filtering down to complete objects
clean_test <- clean_test[, colSums(is.na(clean_test)) == 0]
## function to remove unneeded columns
remove <- function (x) {
  remove <- grepl("^X|timestamp|window", names(x))
}
## applying the function
clean_test <- clean_test[,!remove(clean_test)]
## converting columns to numeric values
clean_test <- clean_test[, sapply(clean_test, is.numeric)]
clean_test

#check the new column names
names(clean_train);names(clean_test)

#set the seed so that it can be reproduced
set.seed(532)

#partition training and cross validation sets
inTrain <- createDataPartition(y = clean_train$classe, p = .60, list = FALSE)

#initial 60% given to training_data remaining 40% goes to cross_validate set
clean_train <- clean_train[inTrain,]
cross_validate <- clean_train[-inTrain,]
clean_train

plot(clean_train$classe, col="blue", main="Levels of Classe", xlab="classe levels", ylab="Frequency")


model <- train(classe ~ ., data = clean_train, method = "rf", 
               trControl = trainControl(method = "cv", 5), 
               ntree = 250)
model

#predictions based on cross validated data
predict_it <- predict(model,cross_validate)

confusionMatrix(cross_validate$classe,predict_it)

## checking the accuracy of the model
model.accuracy <- postResample(predict_it, cross_validate$classe)
model.accuracy

## out of sample error
samp.error <- 1 - 
  as.numeric(confusionMatrix(
    cross_validate$classe, 
    predict_it)$overall[1])
samp.error


clean_test$prediction <- predict(model, clean_test)
clean_test$prediction


## Decision Tree
tree <- rpart(classe ~ ., data = clean_train, method = "class") 
fancytree <- fancyRpartPlot(tree)
basictree <- prp(tree)

## prediction out puts
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(clean_test$prediction)

## Knitting function
library(knitr)

build.report <- function(x) {
  knit2html(x, "project.html")
}
