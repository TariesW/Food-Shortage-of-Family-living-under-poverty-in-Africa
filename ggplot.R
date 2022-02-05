library(ggplot2)

ggplot(model_0, aes(y=number_of_month_foodshortage, x=crop_production)) + geom_point(size = 0.5) + geom_smooth(method = lm) + ggtitle("ggplot for crop production and number of month of food shortage")

ggplot(model_1, aes(x=size_of_land, y=number_of_month_foodshortage)) + geom_point(size = 0.5) + ggtitle("ggplot between number of month of food shortage and size of land")

ggplot(model_1, aes(x=number_of_people, y=number_of_month_foodshortage)) + geom_point(size = 1) + ggtitle("ggplot between number of month of food shortage and number of people")

ggplot(model_1, aes(x=number_of_children, y=number_of_month_foodshortage)) + geom_point(size = 1) + ggtitle("ggplot between number of moth of food shortage and number of children")

ggplot(model_1, aes(x=number_of_people_over60, y=number_of_month_foodshortage)) + geom_point(size = 1) + ggtitle("ggplot between number of moth of food shortage and number of people who are over 60")
