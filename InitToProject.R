#####################################
#      Intro to ML Traning - R      #
# Please come back for updated file #
#####################################

# Script 1 - Video 5

# Set the working directory

setwd("~/Learning/projectTitanic")

# Import the training set: train
# Your working directory might vary

train <- read.csv("~/Learning/projectTitanic/train.csv")

# viewing the "train" dataframe in raw format

train

# viewing the "train" dataframe in table format

View(train)

# dataframe structure

str(train)


# making Table of table survived
table(train$Survived)
prop.table(table(train$Survived))

