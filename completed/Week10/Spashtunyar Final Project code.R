#Final Project code

setwd("C:/Users/spashtunyar/Documents/school/DSC0520/Final Project")
dir()
IndiaMD <- read.csv("India_Menu.csv")
MD_Menu <- read.csv("Mcdonalds_Menu.csv")

#Had to parse out the delimiter of a ; to a , to make is CSV formatable 
MDBK_Menu <- read.csv("BKMD.csv")

#MD_Menu is an overlap of the MD/BK Menu, Ignoring.

#Dropping columns that don't match
drop <- c("Calories.from.fat","Total.sugar..g.","c000016","c000017","Dietary.Fiber..g.")
df1 = MDBK_Menu[,!(names(MDBK_Menu) %in% drop)]

head(IndiaMD)
drop2 <- c("Added.Sugars..g.")
df2 = IndiaMD[,!(names(IndiaMD) %in% drop2)]


#so I am changing the column titles of the India dataset to match the MDBK one for comparison, also renaming MDBK for comparison
names(df2) <- c('Type', 'Item', 'Serving.Size.g', 'Calories', 'Protein.g', 'Total.Fat.g', 
                       'Saturated.Fat.g', 'Trans.Fat.g', 'Chol.mg', 'Total.Carb.g', 'Total.Sugar.g',
                        'Sodium.mg')

names(df1) <- c('Chain', 'Item', 'Type', 'Serving.Size.g', 'Calories', 'Total.Fat.g', 
                'Saturated.Fat.g', 'Trans.Fat.g', 'Chol.mg', 'Sodium.mg','Total.Carb.g', 'Total.Sugar.g',
                'Protein.g')

#adding chain column to India MD
Chain <- c('IndiaMD')           
df2$Chain <- Chain

#Merge the tables so we have 1 complete data table to use
FinalDFMDBK <- rbind(df1,df2)

BurgerCompare<- FinalDFMDBK[c(2,5,7,180,181, 183), ]
FriesCompare<-FinalDFMDBK[c(210,50,54), ]
library(ggplot2)
ggplot(BurgerCompare, aes(x=Chain,y=Calories,color=Item)) + geom_point() +ggtitle("Calorie Comparison of Iconic Burgers")
ggplot(BurgerCompare, aes(x=Chain,y=Total.Fat.g,color=Item)) + geom_point() +ggtitle("Fat Comparison of Iconic Burgers")
ggplot(BurgerCompare, aes(x=Chain,y=Serving.Size.g,color=Item)) + geom_point() +ggtitle("Serving Size Comparison of Iconic Burgers")


ggplot(FriesCompare, aes(x=Chain,y=Calories,color=Item)) + geom_point() +ggtitle("Calorie Comparison of Iconic Menu Itmes")
ggplot(FriesCompare, aes(x=Chain,y=Serving.Size.g,color=Item)) + geom_point() +ggtitle("Serving Size Comparison of Iconic Menu Itmes")

IndiaMDCompare<-FinalDFMDBK[c(180,181, 183, 501, 510, 512), ]
ggplot(IndiaMDCompare, aes(x=Chain,y=Calories,color=Item)) + geom_point() +ggtitle("Calorie Comparison of Indian vs American Burgers")
ggplot(IndiaMDCompare, aes(x=Chain,y=Total.Fat.g,color=Item)) + geom_point() +ggtitle("Fat Comparison of Indian vs American Burgers")
ggplot(IndiaMDCompare, aes(x=Chain,y=Serving.Size.g,color=Item)) + geom_point() +ggtitle("Serving Size Comparison of Indian vs American Burgers")

IndiaFriesCompare<-FinalDFMDBK[c(210,527,530), ]
ggplot(IndiaFriesCompare, aes(x=Chain,y=Calories,color=Item)) + geom_point() +ggtitle("Calorie Comparison of Iconic Menu Itmes")
ggplot(IndiaFriesCompare, aes(x=Chain,y=Serving.Size.g,color=Item)) + geom_point() +ggtitle("Serving Size Comparison of Iconic Menu Itmes")
ggplot(IndiaFriesCompare, aes(x=Chain,y=Sodium.mg,color=Item)) + geom_point() +ggtitle("Serving Size Comparison of Iconic Menu Itmes")

MassBurgerCompare<-FinalDFMDBK[c(1:29,175:196,500:518), ]
ggplot(MassBurgerCompare, aes(x=Chain,y=Calories, color=Type)) + geom_point() +ggtitle("Calorie Comparison of All Sandwiches")

ggplot(MassBurgerCompare, aes(Calories, color=Chain)) + geom_histogram(bins = 22) +ggtitle("Calorie Comparison of All Sandwiches")

ggplot(MassBurgerCompare, aes(x=Total.Fat.g,y=Calories, color=Chain)) +geom_point() +ggtitle("Calorie Comparison of All Sandwiches")

ggplot(MassBurgerCompare, aes(x=Total.Fat.g,y=Calories, color=Chain)) +geom_point() + geom_smooth()+ggtitle("Calorie Comparison of All Sandwiches")

cor(MassBurgerCompare$Total.Fat.g, MassBurgerCompare$Calories)

cor(MassBurgerCompare$Protein.g, MassBurgerCompare$Calories)

cor(MassBurgerCompare$Protein.g, MassBurgerCompare$Calories)
cor(MassBurgerCompare$Sodium.mg, MassBurgerCompare$Calories)
cor(MassBurgerCompare$Total.Carb.g, MassBurgerCompare$Calories)

