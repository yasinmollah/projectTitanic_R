#####################################
#      Intro to ML Traning - R      #
# Please come back for updated file #
#####################################

# Script 1 - Video 5

# Set the working directory

setwd("~/Learning/projectTitanic")

#Amelia Packege Installetion
install.packages("Amelia")
library(Amelia)
# Import the training set: train
# Your working directory might vary

train <- read.csv("~/Learning/projectTitanic/train.csv")
test<- read.csv("~/Learning/projectTitanic/test.csv")
# viewing the "train" dataframe in raw format

train

# viewing the "train" dataframe in table format

View(train)
View(test)

# dataframe structure

str(train)


# making Table of table survived
table(train$Survived)
prop.table(table(train$Survived))*100

# Manipulating Test Data Set
table(test$Sex)
prop.table(table(test$Sex))*100
test$Survived <- rep(0,418)
#Submission file ready for first submission
prediction1 <- data.frame(PassengerId=test$PassengerId, Survived=test$Survived)
write.csv(prediction1,file = "1stPrediction.csv", row.names = FALSE)


#Missing map calculating

missmap(train,main="TitanicTrainingData-MissingMap",col=c("orange","black"),legend=FALSE)


#
##
###
# Making ready for second submission
summary(train$Sex)
prop.table(table(train$Sex,train$Survived),1)*100

barplot(table(train$Sex), xlab = "Passenger", ylab = "People", main = "Titanic Data Passenger")

test$Survived<-0
test$Survived[test$Sex=='female']<-1

prediction2<-data.frame(passengerID=test$PassengerId,Survived=test$Survived)
write.csv(prediction2,file = "2ndPrediction.csv",row.names = FALSE)


#Starting the section for third prediction
train$Fare2<-"30+"
train$Fare2[train$Fare<30 & train$Fare>=20]<-"20-30"
train$Fare2[train$Fare<20 & train$Fare>=10]<-"10-20"
train$Fare2[train$Fare<10]<-"<10"
View(train)
table(train$Fare2)
barplot(table(train$Fare2))

aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN = sum)

aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN = length)

aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN = function(x){sum(x)/length(x)})

test$Survived<-0
test$Survived[test$Sex=="female"]<-1
test$Survived[test$Sex == "female" & test$Pclass==3 & test$Fare>=20]<-0

#barplot(table(test$Survived))

#Submission File ready for 3rd prediction
prediction3<-data.frame(test$PassengerId,test$Survived)
names(prediction3)<-c("passengerID","Survived")
rownames(prediction3)<-NULL
write.csv(prediction3,file = "prediction3.csv",row.names = FALSE)

#
#Getting ready for 4th prediction
#
#Installing Packeges
install.packages("rattle", repos="https://rattle.togaware.com", type="source")
install.packages("rpart.plot")
install.packages("RColorBrewer")

#Loading Libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Constructing Decision tree
mytree1<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = train, method = "class")

fourthPrediction<-predict(mytree1,test,type = "class")
prediction4<-data.frame(PassengerId=test$PassengerId,Survived=fourthPrediction)
write.csv(prediction4,file = "Prediction4.csv",row.names = FALSE)

#Starting Scripts for 5th prediction
View(test)
