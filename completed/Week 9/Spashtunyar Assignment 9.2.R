# Assignment: ASSIGNMENT 9.2
# Name: Pashtunyar,Shaquiel
# Date: 2022-08-07

library(ggplot2)
library(plyr)
library(dplyr)

#I went ahead and created a CSV with the information from the file. Then I pulled the column names so that the data set could make sense
setwd("C:/Users/spashtunyar/Documents/school/DSC0520/")

Surgery <- read.csv("data/Theoratic Surgery.csv")

head(Surgery)

colnames(Surgery) <- c("diagnosis","FVC","FEV1","Perf Stat", "Pain", "Haemoptysis","Dyspnoea","Cough","Weakness","T Size","T2 Diab","MI","PAD","Smoking","Asthma","Age","Risk1Yr")

head(Surgery)
str(Surgery)

#Survival chance using the 1 year risk column

glm.fit <- glm(Risk1Yr ~ ., data =Surgery, family = binomial)
summary(glm.fit)

#when looking at the summary we see a p values we can see what had the least chance of survival. This includes the size of the tumor, Diabetes, Dyspepsia and smoking.
#All of these were marked as * values in the summary indicating their p value. 


library(caTools)
Surgery2 <- read.csv("data/Theoratic Surgery.csv")
colnames(Surgery2) <- c("diagnosis","FVC","FEV1","Perf Stat", "Pain", "Haemoptysis","Dyspnoea","Cough","Weakness","T Size","T2 Diab","MI","PAD","Smoking","Asthma","Age","Risk1Yr")

split <- sample.split(Surgery2, SplitRatio = 0.8)

subset1<- subset(Surgery2,split = "TRUE")
subset2<- subset(Surgery2,split = "FALSE")

tr_model <- glm(Risk1Yr ~ ., data = subset1,
                family = binomial)

res1 <- predict(tr_model, data = subset1, type = "response")
res1

res2 <- predict(tr_model, data = subset2, type = "response")
res2

confmatrix <- table(Actual_value = subset1$Risk1Yr, Predicted_value = res1 > 0.5)
confmatrix


(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

#0.8422 or 84% accuracy using our model, so I would say accurate

#repeat same actions for binary class
binary_class <- read.csv("data/binary-classifier-data.csv")
Binarysplit <- sample.split(binary_class, SplitRatio = 0.8)

train<- subset(binary_class, split = "TRUE")
test<- subset(binary_class, split = "FALSE")

binary_class$label <- as.factor(binary_class$label)
binary_class$x <- as.factor(binary_class$x)
binary_class$y <- as.factor(binary_class$y)

binary_class_model<- glm(label ~ x + y, data = train, family = binomial)
summary(binary_class_model)

res <- predict(binary_class_model, data = train, type = "response")

res <- predict(binary_class_model, data = test, type = "response")

confmatrix <- table(Actual_value = train$label, Predicted_value = res > 0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

#0.5834 or 58.34%
#the 58% seems not very accurate in pridicting the label value, so I wouldn't trust this model. 