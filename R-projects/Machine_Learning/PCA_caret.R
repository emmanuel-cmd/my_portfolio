# Caret package
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, 
                               p=0.75, list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M>0.8, arr.ind = T) # array indices

names(spam)[c(34,32)]

# principal components
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

# number of variances explained by each PCs
prComp$rotation

# PCA on SPAM data
typeCOlor <- ((spam$type=="spam")*1+1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1], prComp$x[,2],col=typeCOlor, xlab="PC1", ylab="PC2")

#PCA with caret
preProc <- preProcess(log10(spam[,-58]+1), method = "pca",pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col=typeCOlor)

# Preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1), method = "pca",pcaComp = 2)
trainPC <- predict(preProc, log10(training[,-58]+1))
trainPC$type <- training$type
modelFit <- train(type~.,method = "glm", data = trainPC)

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

# Alternative(sets # of PCs) -- doing all in one function 
modelFit <- train(type ~ .,method = "glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

