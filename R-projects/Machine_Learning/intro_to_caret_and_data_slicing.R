###############################################
########## PRACTICAL MACHINE LEARNING #########
###############################################

# Caret package
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, 
                               p=0.75, list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type~.,data=training,method = "glm")
modelFit
modelFit$finalModel

# predictions
predictions <- predict(modelFit, testing)

#evaluate whether model fit works or not

confusionMatrix(predictions, testing$type)


####### DATA SLICING ############
## K-fold

set.seed(32323)
folds <- createFolds(y = spam$type,k=10,
                     list=T, returnTrain = T)
sapply(folds, length)
folds[[1]][1:10]

# Resampling
folds <- createResample(y = spam$type,times=10,
                     list=T)
sapply(folds, length)
folds[[1]][1:10]

# Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20,
                          horizon = 10)
names(folds)

folds$train[[1]]
folds$test[[1]]

