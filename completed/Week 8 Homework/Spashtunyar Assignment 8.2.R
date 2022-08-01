# Assignment: ASSIGNMENT 8.2
# Name: Pashtunyar,Shaquiel
# Date: 2022-07-31

library(ggplot2)
library(plyr)
library(dplyr)
library(purrr)
setwd("C:/Users/spashtunyar/Documents/school/DSC0520/")

## Load the `data/housing_df` to
housing_df <- read_excel("data/week-7-housing.xlsx")

#second DF for manipulations
housed_df = data.frame(housing_df)

## 8.2.1 
# Some thing I try to do to clean up the data is change the housing prices to thousands, so my charts don't get weird axis by having to high a value
house_lm<-lm(Sale.Price ~ sq_ft_lot, data=housed_df)

summary(house_lm)

new_house_df<- data.frame(sale_price = predict(house_lm, housed_df), square_ft = housed_df$sq_ft_lot)

ggplot(data = housed_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  geom_line(color='red', data = new_house_df, aes(y = sale_price, x=square_ft)) 

## 8.2.2  Create two variables and more variables
## Square foot lot (sq_ft_lot), bedroom, & square foot total livingroom
house_df = data.frame(housing_df)

#dfdata %>%
#mutate(Sale.Price = Sale.Price/1000)
housed_df = data.frame(housing_df)

# mutate housing sale price in thousands
house_df <-housed_df %>%
  mutate(Sale.Price = Sale.Price/1000)


multi_house_df<- data.frame(sale_price = predict(house_lm, house_df), sq_ft_lot = house_df$sq_ft_lot, 
                            bd_room=house_df$bedrooms, bath_full=house_df$bath_full_count, 
                            sq_ft_LR = house_df$square_feet_total_living)

multihouse_lm<-lm(sale_price ~ sq_ft_lot + bd_room + bath_full + sq_ft_LR, data=multi_house_df)

ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  geom_line(color='red', data = multi_house_df, aes(y = sale_price, x = sq_ft_lot)) 

## 8.2.3 The R2 and Adjusted R2
# Both the R2 and Adjusted R2 generally explain the goodness of a fit in a regression model.
house_lm_std<-lm(scale(Sale.Price)~scale(sq_ft_lot), data=house_df)
summary(house_lm_std)

new_house1_df<- data.frame(sale_price = predict(house_lm_std, house_df), square_ft = house_df$sq_ft_lot)

ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  #geom_line(color='red', data = new_house1_df, aes(y = sale_price, x=square_ft)) 
  geom_smooth(method="lm", color='red')

multihouse_lm_std<-lm(scale(Sale.Price) ~ scale(sq_ft_lot) + scale(bedrooms) + scale(bath_full_count) + scale(square_feet_total_living), data=housed_df)

## 8.2.4 standardized betas for each parameter of the multiple regression model
summary(multihouse_lm_std)

multi_house_df2<- data.frame(sale_price = predict(house_lm, house_df), square_ft = house_df$sq_ft_lot,
                            bd_room=house_df$bedrooms, bath_full=house_df$bath_full_count, 
                            sq_LR_total = house_df$square_feet_total_living)

ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  geom_line(color='red', data = multi_house_df, aes(y = sale_price, x=square_ft))

## 8.2.5 Confidence Intervals
multihouse_lm<-lm(Sale.Price ~ sq_ft_lot + bedrooms + bath_full_count + square_feet_total_living, data=house_df)
multihouse_lm

multi_house_df3<- data.frame(sale_price = predict(multihouse_lm, house_df), sq_ft_lot = house_df$sq_ft_lot, 
                            bd_room=house_df$bedrooms, bath_full=house_df$bath_full_count, 
                            sq_ft_LR = house_df$square_feet_total_living)

predict(multihouse_lm, house_df, interval="confidence")


## 8.2.6 When I changed the x-axis with a variable square foot total living room, the regression
#       model improved a lot while the fitness of this multi regression model looked much better.

ggplot(data = multi_house_df, aes(y = sale_price, x = sq_LR_total)) +
  geom_point(color='blue') +
  #geom_line(color='red', data = multi_house_df, aes(y = sale_price, x=sq_LR_total))
  geom_smooth(method="lm", color='red')


##8.2.7  Identify and removing Outliers
Q<- quantile(multi_house_df$sale_price, probs=c(.25, .75), na.rm = FALSE)
Q1<- quantile(multi_house_df$square_ft, probs=c(.25, .75), na.rm = FALSE)
Q1

iqr<- IQR(multi_house_df$sale_price)
iqr

up<- Q[2] + 1.5*iqr # Upper Range
low<- Q[1] - 1.5*iqr # Lower Range

up
low

multi_house_elm_df <- subset(multi_house_df, multi_house_df$sale_price > (Q[1] - 1.5*iqr) & multi_house_df$sale_price < (Q[2]+1.5*iqr))
multi_house_elm_df1<- subset(multi_house_elm_df, multi_house_elm_df$square_ft > (Q[1] - 1.5*iqr) & multi_house_elm_df$square_ft < (Q[2]+1.5*iqr))

##8.2.8 Calculate the standardized residuals
standard_res <- rstandard(multihouse_lm_std)

final_data <- cbind(multi_house_df, standard_res)
final_data[order(-standard_res),]

filtered_final_data_less_2<-filter(final_data, standard_res <= -2)

filtered_final_data_more_2<-filter(final_data, standard_res >= 2)

##8.2.9  Sum up some of the large residuals
#with(filtered_final_data_more_2, sum(sale_price[standard_res > 5]))

#with(filtered_final_data_less_2, sum(sale_price[standard_res <= -2]))
sum(as.numeric(filtered_final_data_less_2$standard_res), na.rm = TRUE)

sum(as.numeric(filtered_final_data_more_2$standard_res), na.rm = TRUE)

## 8.2.10 Variables wiht large residuals

summary(multihouse_lm)
resid(multihouse_lm_std)

#sq_ft_lot and sq_livingroom_total variables have large residuals.

## 8.2.11  Calculate the leverage, cooks distance, and covariance

# calculate leverage for each observation
leverage_house <- as.data.frame(hatvalues(multihouse_lm))
leverage_house  

# calculate cooks distance
cooks.distance(multihouse_lm)

# calculate the covariance ratios between sale price and sq livingroom total
cov(multi_house_df$sale_price, multi_house_df$sq_LR_total)

## 8.2.12.
summary(multihouse_lm)

table(multi_house_df$sale_price, multi_house_df$sq_ft_lot)

# chi test of independence

c_test <- chisq.test(table(multi_house_df$sale_price, multi_house_df$sq_ft_lot))
c_test

# X^2 test statistic and the p-value
c_test$statistic

c_test$p.value

# running the diagnostic plots
install.packages('ggfortify')
library(ggfortify)

autoplot(multihouse_lm)

## 8.2.13  Calculate to assess the assumption of no multicollinearity.
library(caret)
library(car)

vif_test<-vif(multihouse_lm)

car::vif(multihouse_lm)

# the variance inflation factor (or VIF)  that exceeds 5 or 10 indicates a 
# problematic amount of collinearity. No multicollinearity seems to exist.

## 8.2.14.
plot(sale_price ~ sq_ft_lot, data = multi_house_df)
#The relation between the two appear to be non linear  

multi_house_df
plot(sale_price ~ sq_ft_LR, data = multi_house_df)

hist(multi_house_df$sale_price)
# What we see from the histogram, is that multi house sales have a higher frequency on the lower end, and not to often on the higher end

## 8.2.15 
summary(multihouse_lm)
# What we get from the data is that the regression model is unbiased. R squared values adjusted or not are around 0.2 and are low. The plots show the data is not scattered on the fitted line. 

