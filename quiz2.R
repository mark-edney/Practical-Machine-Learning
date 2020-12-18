#q1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


#q2 missing
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(ggplot2)
library(Hmisc)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
training[,1:8]<-as.data.frame(apply(training[,-9], 2, cut2))
colors<-names(training[1:8])
g<-ggplot(data = training,aes(y=CompressiveStrength, x=rownames(training)))+
        geom_point() + aes(color = Age,alpha=.5)
g
 

#q3 same values
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
g<-ggplot(data=training, aes(y=Superplasticizer)) + geom_boxplot()
g
summary(training)
test<-log10(training$Superplasticizer+1)
boxplot(test)


#q4 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
Il<-grep("^IL",names(training))
pre<-preProcess(training[,Il], method ="pca",thresh = 0.8)


#q5 not 075 .71
index<-c(1,Il)
mdl1<-train(diagnosis~., data = training[,index], method = "glm")
trainpc<-predict(pre, training[,index])
testpc<-predict(pre, testing[,index])
test1<-predict(mdl1, testing[,index])
mdl2<-train(diagnosis~., data = trainpc, method = "glm")
confusionMatrix(testing$diagnosis, predict(mdl1,testing))
confusionMatrix(testing$diagnosis, predict(mdl2,testpc))
