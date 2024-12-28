#RÜMEYSA DURDAĞ 

getwd()
setwd("C:/Users/rmysa/OneDrive/Masaüstü/412")
library(dplyr)
library(naniar)
library(stringr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(caret)
library(epitools)
library(missForest)
library(neuralnet)
library(devtools)
library(gridExtra)
library(rpart)
library(rpart.plot)

poke_test <- read.csv("test.csv", stringsAsFactors = T)

poke_train <- read.csv("train.csv", stringsAsFactors = T)


str(poke_train)
is.na(poke_train)


poke_test$Name <- recode(poke_test$Name,  "NA" = "Other")


unique(as.character(poke_test$Type.2))

poke_test$Type.2 <- if_else(poke_test$Type.2 == "", "Other", poke_test$Type.2)
unique(as.character(poke_test$Type.2))

unique(as.character(poke_test$Type.1))

unique(as.character(poke_train$Name))
poke_train$Name <- if_else(is.na(poke_train$Name), "Other", poke_train$Name)


unique(as.character(poke_train$Type.2))
poke_train$Type.2 <- if_else(poke_train$Type.2 == "", "Other", poke_train$Type.2)

poke_train$Type.2 <- if_else(is.na(poke_train$Type.2), "Other", poke_train$Type.2)
unique(as.character(poke_train$Type.2))



str(poke_train)
str(poke_test)

poke_train$X <- as.factor(poke_train$X)
poke_train$Generation <- as.factor(poke_train$Generation)

poke_test$X <- as.factor(poke_test$X)
poke_test$Generation <- as.factor(poke_test$Generation)

str(poke_train)
str(poke_test)
M_train <-  missForest(poke_train[,c(6:13)])
M_test <-  missForest(poke_test[,c(6:13)])

M_train <-  M_train$ximp
M_test= M_test$ximp

M_train=data.frame(M_train,poke_train[,c(4:5,14)])
M_test=data.frame(M_test,poke_test[,4:5])

train_new=na.omit(M_train)
training.samples <- train_new$Legendary %>% createDataPartition(p = 0.8, list = FALSE)


train.data  <- train_new[training.samples, ]
test.data <- train_new[-training.samples, ]

fit= rpart(Legendary~.,data = train.data, method = 'class')

rpart.plot(fit, type = 1,fallen.leaves=FALSE)



predictions <-predict(fit, test.data, type = 'class')
confusionMatrix(as.factor(predictions),as.factor(test.data$Legendary))

predictions <-predict(fit, M_test, type = 'class')
predictions

data=cbind(predictions)
data[data==1]= "FALSE"
data[data==2]= "TRUE"
data <- data.frame(data)
data <- tibble::rowid_to_column(data, 'id')
head(data)
colnames(data) <- c('id','y')
write.csv(data,"competition-412.csv")

