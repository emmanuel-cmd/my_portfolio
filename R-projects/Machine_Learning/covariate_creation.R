# Covariate creation

library(ISLR); library(caret); data(Wage)
inTrain <- createDataPartition(Wage$wage, p=0.75, list = F)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# convert factor variables to indicator variables

table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# Removing zero covariates
nsv <- nearZeroVar(training, saveMetrics = T)
nsv # see variables that have zero variance in them
    # so that you can throw them out

library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis #creates 3 new age variables - age, age^2
        #and age^3

# Fitting curves with splines
lm1 <- lm(wage~bsBasis, data = training)
plot(training$age, training$wage, pch=19,cex=0.5)
points(training$age, predict(lm1,newdata=training),col="red", pch=19,cex=0.5)

# splines on the test set
predict(bsBasis, age=testing$age)
