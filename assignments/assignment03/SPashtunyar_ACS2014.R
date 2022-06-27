# Assignment: ASSIGNMENT ACS_2014
# Name: Pashtunyar Shaquiel
# Date: 2022-06-26

install.packages ("pastecs")
library("ggplot2")
library("psych")
library("pastecs")
theme_set(theme_minimal())

setwd("C:/Users/spashtunyar/Documents/school/data")
dir()

Census_2014 <- read.csv("acs-14-1yr-s0201.csv")
#Question 1
typeof(Census_2014)

#Question 2
str(Census_2014)
nrow(Census_2014)
ncol(Census_2014)

#Question 3
ggplot(Census_2014, aes(HSDegree)) + geom_histogram(bins=25) +ggtitle("Census 2014 Regions HS Degree %") +xlab("Percentage with HS Degree") +ylab("Counties")

#Question 4
#The chart is unimodal with a peak at 90
#It is not symmetrical it is leaning to the right at the 90% percentile
#I would say its normal, but there is one outlier at 8% ish that seems out of place
#It is not skewed in any way just has an outlier
ggplot(Census_2014, aes(HSDegree)) + geom_histogram(aes(y=..density..)) + stat_function(fun = dnorm, args = list(mean = mean(Census_2014$HSDegree), sd = sd(Census_2014$HSDegree)), colour = "black", size = 1)+ ggtitle("Census 2014 Regions HS Degree %") +xlab("Percentage with HS Degree") +ylab("Counties")
#the normal curve is not entirely accurate, it is skewed to the left a little because there are outliers on the left hand side 

#Question 5
ggplot(Census_2014, aes(HSDegree)) + geom_density(aes(x=HSDegree))

#Question 6 
#The distribution is skewed to the 88ish precentil as it has a 10% chance, so the denisty is higher in that region

#Question 7
ggplot(Census_2014, aes(HSDegree)) + geom_histogram(aes(y=..density..)) + stat_function(fun = dnorm, args = list(mean = mean(Census_2014$HSDegree), sd = sd(Census_2014$HSDegree)), colour = "black", size = 1)+ ggtitle("Census 2014 Regions HS Degree %") +xlab("Percentage with HS Degree") +ylab("Counties")
describe(Census_2014$HSDegree)
stat.desc(Census_2014$HSDegree, basic=TRUE, norm=FALSE)

#question 8
#the skew score tells us how skewed the data is, it is saying that it is 1.67 std deviation skewed to the right
#the kurtosis score shows that it is very heavy tailed since it is 4.35
# the z-score shows that the data is not like the mean, and is swayed slightly, you can see that from the curve line to the histogram values
