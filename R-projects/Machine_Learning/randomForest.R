
library(caret)
library(ggplot2)
library(randomForest)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species, 
                               p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

modFit <- train(Species~.,data=training,method="rf",prox=T)
modFit

getTree(modFit$finalModel,k=2)

# Class "centers"
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$proximity)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length,col=Species),size=5, shape=4,data=irisP)

# Predicting new values
pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species
table(pred, testing$Species)
