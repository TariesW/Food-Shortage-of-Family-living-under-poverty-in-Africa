library(readxl)

data <- read_excel("Documents/985:211 Statisitcs/11-500/data.xlsx")
View(data)

library(dplyr)

set.seed(3394)
household <- as.data.frame(sample_n(data, 1000))

length(household) #check numbers of variable of this dataframe
count(household)  #check numbers of observations of this dataframe
View(household)


#家庭贫困水平有哪些指标?
number_of_people <- household$`Number of people in household`
size_of_land <- household$`Size of Land (Hectares)`
crop_production <- household$`Crop production (tonnes per year)`
number_of_month_foodshortage <- household$`Number of months in past year experienced food shortages`
number_of_workingageaadults <- household$`Number of working-age adults in household`
number_of_children <- household$`Number of children in household`
number_of_people_over60 <- household$`Number of people aged over 60 in household`


library(ggplot2)
library(psych)

#这项研究中家庭的特征是什么?
describe(number_of_people)
describe(number_of_children)
describe(number_of_people_over60)
describe(number_of_workingageaadults)

qplot(number_of_people, geom = "histogram", data = household, main = "Histogram of number of people in household", xlab = "number of people", ylab = "count") +
  stat_bin(bins = 15)

qplot(number_of_children, geom = "histogram", data = household, main = "Histogram of number of children in household", xlab = "number of people", ylab = "count") +
  stat_bin(bins = 9)

qplot(number_of_people_over60, geom = "histogram", data = household, main = "Histogram of number of people over 60 in household", xlab = "number of people", ylab = "count") +
  stat_bin(bins = 5)

qplot(number_of_workingageaadults, geom = "histogram", data = household, main = "Histogram of number of wokring age adults in household", xlab = "number of people", ylab = "count") +
  stat_bin(bins = 8)

#这些家庭赖以生存的农场的特点是什么? 它们产生什么?他们有多少土地?
describe(crop_production)
describe(size_of_land)
df_land <- data.frame(crop_production, size_of_land, crop_production/size_of_land)


#家庭是否正在面临食物短缺? 多久一次?
table(household$`Experienced food shortages in the past year`)
table(household$`Number of months in past year experienced food shortages`)
ftable(table(household$`Experienced food shortages in the past year`, household$`Number of months in past year experienced food shortages`))


#种植更多农作物的家庭难免出现粮食短缺的情况?
model_0 <- lm(number_of_month_foodshortage ~ crop_production, data = household)
summary(model_0)


#较富裕的家庭(拥有更多土地，更好的住房)是否更不容易受到粮食短缺的影响?
model_1 <- lm(number_of_month_foodshortage ~ size_of_land, data = household)
summary(model_1)

qplot(size_of_land, geom = "density", data = household, main = "Density plot of size of land", ylab = "density", xlab = "size of land (Hectares)") + 
  geom_vline(aes(xintercept = mean(size_of_land)), color = "red", linetype="dashed", size=1)


#家庭组成对粮食短缺经验的影响是什么? 有更多的孩子意味着更多的短缺吗? 或更 年长的人? 还是工作年龄人口?
model_2 <- lm(number_of_month_foodshortage ~ number_of_children + number_of_people_over60 + number_of_workingageaadults, data = household)
summary(model_2)


#create a data frame for multiple linear regression model
df <- data.frame(number_of_month_foodshortage, number_of_people, number_of_workingageaadults, number_of_children, size_of_land, crop_production)
head(df)

#multiple linear regression for model 1, y = number of months experienced food shortage
model_3 <- lm(number_of_month_foodshortage ~., data = df)
summary(model_3)





