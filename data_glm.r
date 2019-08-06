##FAST FOOD##

fastfood<-
read.csv("C:\\Users\\j\\Desktop\\fastfoodmaps_locations_2007.csv\\fastfoodmaps_location
s_2007.csv")
length(fastfood$state)
fastfoodedited <- read.csv("fastfoodedited.csv")
#trying to get number per state
FastFoodRestaurants <- fastfoodedited$Adjusted

##OBESITY##
obesity <- read.csv("obesity.csv")
obesechildren <- obesity$Overweight.or.obese..85th.percentile.or.above...

#healthy
healthyweightchildren <- obesity$Healthy.weight..5th.to.84th.percentile...
plot (FastFoodRestaurants, healthyweightchildren)
healthyweightchildren.lm <- lm(healthyweightchildren~FastFoodRestaurants)
abline(healthyweightchildren.lm, col="blue")
summary(healthyweightchildren.lm)

#underweight
underweightchildren <- obesity$Underweight..less.than.5th.percentile...
plot (FoodALL, underweightchildren)
underweightchildren.lm <- lm(underweightchildren~FastFoodRestaurants)
abline(underweightchildren.lm, col="blue")
summary(underweightchildren.lm)

#obese
plot(FastFoodRestaurants, obesechildren)
regression.lm <- lm(obesechildren~FastFoodRestaurants)
abline(regression.lm, col="red")
summary(regression.lm)
##Other Possibilities?#
#HealthSpendingPerState
HealthSpending <- read.csv("healthcare.csv")
HealthSpendingPerCapita <- HealthSpending$Health.Spending.per.Capita
plot(HealthSpendingPerCapita, obesechildren)
healthspending.lm <- lm(obesechildren~HealthSpendingPerCapita)
abline(healthspending.lm, col="red")
summary(healthspending.lm)
11
#roadmiles
Roads <- read.csv("roadmiles.csv")
RoadMiles <- Roads$Adjusted
#education
education <- read.csv("education.csv")
bachelors <- education$Bachelors
highschool <- education$High.School
advanceddegree <- education$Advanced
full.lm <- lm(obesechildren ~ bachelors + highschool + advanceddegree +
HealthSpendingPerCapita + FastFoodRestaurants + RoadMiles)
summary(full.lm)
plot(highschool, FastFoodRestaurants)
highschool.lm <- lm(FastFoodRestaurants ~ highschool)
abline(highschool.lm, col="red")
