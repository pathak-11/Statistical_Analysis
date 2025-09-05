###### Lab 13----------

plantsco2 <- read.csv("plantCO2.csv")
plantsco2

colnames(plantsco2)                      

library(ggplot2)

ggplot(plantsco2, aes( y = plant.size, x = seedWeight,
                       color = treatment))+ geom_point()+
  geom_smooth(method = "lm", se = FALSE)
# note : geom_smooth shouldnt use for final graphs
# its ok for the quick scan for trends

## 1.1.1 Checking for an interaction

plant.lm1 <- lm(plant.size ~ treatment +seedWeight, data = plantsco2)
plant.lm2 <- lm(plant.size ~ treatment* seedWeight, data = plantsco2)

# compare the two models
anova(plant.lm1, plant.lm2)

# since p-value is greater than 0.05, there is no
# improvement when we have interaction
# thus we can remove the interaction in the model
# do the test plant.lm1

# closer look at our chosen model
anova(plant.lm1)
# both treatment (F1,17 = 20.78, p -value = 0.0003) and
# seed weight (F1,17 = 9.42, p-value = 0.007)

summary(plant.lm1)
# need to also report the adjusted R2 value
# R2 = 0.60
# adjusted R as we have multiple variables


# check assumptions
shapiro.test(residuals(plant.lm1))
# normal

# linearity check
plot(residuals(plant.lm1) ~ fitted(plant.lm1))
# not okay, has fan shaped pattern

# check variance variable with each of the variables
# equal variance

# boxplot since treatment is a factor
boxplot(residuals(plant.lm1)~ plantsco2$treatment)
# not equal

plot(residuals(plant.lm1) ~ plantsco2$seedWeight)
# seems equal variance

# for this lab okay to state conclusion about assumptions


# 1.2 Linear models with multiple numeric variables

library(hflights)
data("hflights")

library(dplyr)
subsetflights <- hflights%>%  # work with hflight 
  filter(Month == 1 & DayofMonth ==1)%>%  # filters to keep month ==1
  select(Month, DayofMonth, ArrDelay, DepDelay, Distance) # selects columns of interest

# remove the NA
subsetflights <- na.omit(subsetflights)


# we will fit a linear model, that predicts arrival
# delay and departure delay and flight distance
# we are not interested in interaction

# make the linear model
delay.lm <- lm(ArrDelay ~ DepDelay + Distance, data = subsetflights)

# check assumptions
shapiro.test(residuals(delay.lm))
# not- normal
hist(residuals(delay.lm))
# CLT - large sample size can assume normality


# linearity check
plot(residuals(delay.lm) ~ fitted(delay.lm))
# linear

# equal variance
plot(residuals(delay.lm) ~ subsetflights$DepDelay)

plot(residuals(delay.lm) ~ subsetflights$Distance)
# fan shaped


# checking if the x variables are linear with y independently
plot(ArrDelay ~ DepDelay, data = subsetflights)
plot(ArrDelay ~ Distance, data = subsetflights)


# with hypothesis all passed we do summary to find 
# significant effect departure delay and flight distance
 # on arrival

summary(delay.lm)
# conclude that there is a significant positive effect
# on arrival delay ( t= 42.4, df = 544, P < 0.001)
# flight distance had no effect ( t = -1.5, df = 544, p = 0.14)
# our adjusted R-squared value is 0.77; our model
# explain 77% of the total variation 
# we report adjusted R-squared value because we have
# more than 1 explanatory variable


# 1.3 Random effects

library(nlme)

#The command for a linear mixed effects model is lme(), and you need to specify both the fixed and random
# part. If you try to use this command without a random effect, it will throw an error.



zooDiv <- read.csv (url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter18/chap18e2ZooplanktonDepredation.csv"))
zooModel <- lme(diversity ~ treatment, random = ~1 | block, data = zooDiv)


anova(zooModel)

# conclude that treatment has a significant effect
# on diversity (F 2,12 = 16.37, P= 0.0015)
# the 12 came from sample # number of groups
# 15 - 3

# one way to check number of groups
unique(zooDiv$treatment)


# check assumptions
shapiro.test(residuals(zooModel))
# normal

hist(residuals(zooModel))
# normal

plot(residuals(zooModel)~ fitted(zooModel))

# check equal variance
boxplot(residuals(zooModel) ~ zooDiv$treatment)
# looks okay

# we can check our residual plots looking for eqal variance
plot(zooModel)


# last class exercise

beetles <- read.csv("beetles.csv")
beetles

# code to make sure Block is a factor
beetles$Block <- as.factor(beetles$Block)

ggplot(data = beetles, aes( x = genotype, y = weight, 
                            shape = Block))+ geom_point()
  

# 

beetle.lm <- lme(weight ~ genotype, random =  ~1 | Block, data = beetles)

anova(beetle.lm)
# there is a significant difference between genotypes
# F2,9 = 6.97, P= 0.03
# 9 comes from number of samples - groups
# 12-3 = 9

# checking assumptions
shapiro.test(residuals(beetle.lm))
# normal
 hist(residuals(beetle.lm))
# not normal
 
plot(beetle.lm)
# looks good

boxplot(residuals(beetle.lm) ~ beetles$genotype)
# looks equal variance


