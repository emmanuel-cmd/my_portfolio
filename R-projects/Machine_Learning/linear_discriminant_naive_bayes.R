
library(caret)
library(ggplot2)
library(randomForest)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species, 
                               p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Build predictions
modlda <- train(Species~.,data=training,method="lda")
modnb <- train(Species~.,data=training,method="nb")
plda <- predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)

# Comparison of results
equalPredictions <- (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)


