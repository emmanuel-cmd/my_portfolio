######## PLOTTING PREDICTORS
# Example: Wage data

library(ISLR)
library(ggplot2)
library(caret)
data(Wage) 
summary(Wage)

# Get training/test sets
inTrain <- createDataPartition(y=Wage$wage, 
                               p=0.7,list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Feature plot in caret package
featurePlot(training[,c("age", "education","jobclass")], 
            y = training$wage,
            plot="pairs")

# Qplot (ggplot2 package)
qplot(age, wage, data = training)

qplot(age,wage,colour=jobclass,data=training)

qq <- qplot(age,wage,colour=education,data=training)
qq + geom_smooth(method='lm', formula=y~x)


# cut2, making factors(Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

# Boxplots with cut2
p1 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot"))
p1

# Boxplots with points overlayed
library(gridExtra)
p2 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol=2)

# Tables
t1 <- table(cutWage, training$jobclass)
t1

# get the proportion in each groups (1=row, 2=column)
prop.table(t1,1)

# Density plots
qplot(wage, colour=education, data=training, geom="density")
