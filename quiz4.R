library("AppliedPredictiveModeling")
library(caret)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)

#q1
load("vowel.train.RData")
load("vowel.test.RData")
vowel.test$y<- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)

mdl11 <- train(y~., data = vowel.train, method = "rf")
mdl12 <- train(y~., data = vowel.train, method = "gbm")

pmdl11<- as.integer(predict(mdl11, vowel.test[,-1]))
pmdl12<- as.integer(predict(mdl12, vowel.test[,-1]))

accmdl11 <- mean(pmdl11==as.integer(vowel.test[,1]))
accmdl12 <- mean(pmdl12==as.integer(vowel.test[,1]))

agree <- pmdl11 == pmdl12
accagree <- mean(pmdl11[agree]==as.integer(vowel.test[,1][agree]))


#q2 not batter than all 3 .8, 0.8 less then boosting
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
mdl21 <- train (diagnosis~., data = training, method = "rf")
mdl22 <- train (diagnosis~., data = training, method = "gbm")
mdl23 <- train (diagnosis~., data = training, method = "lda")

prmdl21 <- predict(mdl21, testing[,-1])
prmdl22 <- predict(mdl22, testing[,-1])
prmdl23 <- predict(mdl23, testing[,-1])

stacked <- data.frame(prmdl21, prmdl22, prmdl23, diagnosis= testing$diagnosis)
stackmdl <- train(diagnosis~., data = stacked, method = "rf")
pstack <- predict(stackmdl, stacked)

acc <- function(mdl){
        mean (mdl==testing$diagnosis)}

acc(prmdl21)
acc(prmdl22)
acc(prmdl23)
acc(pstack)


#q3 not fine or corse, cement?
set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
mdl3 <- train(CompressiveStrength~., data = training, method = "lasso")
plot.enet(mdl3$finalModel)
mdl3$finalModel

#q4
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
mdl4 <- bats(tstrain)
pmdl4 <- forecast(mdl4, h= nrow(testing))
accuracy(pmdl4, testing[,3])
mean(testing[,3] > pmdl4$lower[,2] & testing[,3] < pmdl4$upper[,2])


#q5 6.72
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
mdl5 <- svm(CompressiveStrength~., data = training)
pmdl5 <- predict(mdl5, newdata = testing)
mean(sqrt((pmdl5-testing$CompressiveStrength)^2))
