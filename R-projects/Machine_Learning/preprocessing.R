
## Preprocessing

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75,list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve,main="", xlab = "ave.capital run length")

# Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS) #mean = 0
sd(trainCapAveS) # sd = 1

# Standardizing - test set # always use mean and sd from training set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

# Standardizing - preProcess function
preObj <- preProcess(training[,-58],method = c("center", "scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# Standardizing - Box-Cox transforms
preObj <- preProcess(training[,-58],method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,1))
hist(trainCapAveS); qqnorm(trainCapAveS)

# Standardizing - Imputing data
set.seed(13343)

#Make some values NA

training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method = "knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)

quantile((capAve - capAveTruth)[selectNA])

quantile((capAve - capAveTruth)[!selectNA])
