#q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(tidyverse)

set.seed(125)
intrain <- segmentationOriginal$Case == "Train"
training <- segmentationOriginal[intrain,-c(1,2)]
testing <- segmentationOriginal[-intrain,-c(1,2)]
mdl1 <- train(Class~., data = training, method = "rpart")

library(rattle)
fancyRpartPlot(mdl1$finalModel)

#1 PS 2 WS 3    PS 4

#q2 larger value is equal

#q3
library(pgmm)
data(olive)
olive = olive[,-1]
olmdl<-train(Area~., data = olive, method="rpart")
newdata = as.data.frame(t(colMeans(olive)))
predict(olmdl,newdata = newdata)


#q4 not .35 .31, .31 .27
load("SAheart.RData")
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
mdl <- train(chd~ age + alcohol + obesity + tobacco + typea + ldl, data = SAheart,
             method="glm", family="binomial")
missClass = function(values,prediction)
        {sum(((prediction > 0.5)*1) != values)/length(values)}
table(missClass(testSA,predict(mdl, newdata = testSA)),
      missClass(trainSA,predict(mdl, newdata = trainSA)))

#q6 2,1, 56
library(randomForest)
load("vowel.train.RData")
load("vowel.test.RData")
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
mdl<- randomForest(y~., data = vowel.train)
varImp(mdl)
