## DECISION TREES 

library(caret)
library(ggplot2)
data(iris)
names(iris)

table(iris$Species)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species, 
                               p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Iris petal widths/sepal width
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

# Fit decision tree model
modFit <- train(Species~.,methods="rpart", data=training)
print(modFit$finalModel)

#Plot decision tree
plot(modFit$finalModel, uniform=T,
     main = "Classification Tree")
text(modFit$finalModel, use.n=T, all=T, cex=.8)

# Prettier plots 
library(rattle)
fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata = testing)
