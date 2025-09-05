# Lab 9

# 1.1 Simple linear regression - another example

data("mtcars")
head(mtcars)

?mtcars
# Y = miles per gallon
# X = rear axle ratio

# graphing ggplot
library(ggplot2)
ggplot(mtcars, aes(x = drat, y = mpg))+
  geom_point() + xlab("Rear Axle Ratio")+
  ylab("Miles per Gallon") +
  geom_smooth(method = "lm", se = FALSE) # plot linear model line

car.lm <- lm(mpg ~ drat, data = mtcars)

summary(car.lm)

# since the p-value of the slope is < 0.001 we can conclude that 
# there is a significant positive relationship between mpg and drat
# slope on line is 7.7, t = 5.096, p-value <0.001

# this conclusion is invalid if our assumptions doesn't met

# 1.2 testing assumptions

# check if the residuals are normal
shapiro.test(residuals(car.lm))
# p-value > 0.05, suggests that it is normal

hist(residuals(car.lm))
# has bell curve shape, can say it is normal

# test for equal variance
plot(residuals(car.lm) ~ mtcars$drat)
abline(h = 0)
abline(h = c(4,-4), col ="red", lty = 2)
# we can conclude it has equal variance across all values of X

# test of linearity
plot(residuals(car.lm) ~ fitted(car.lm))
abline(h = 0)
# we could conclude the relationship is linear


# 1.3 Extracting information from the model object
# once we have a linear model saved as an object, we can extract information from it.
# to extract the coefficients of the model (i.e., the intercept and slope):

coef(car.lm)

# to extract the fitted values of the model (i.e. the predictions, or y ^)
fitted(car.lm)

# to extract the residuals of the model
residuals(car.lm)

# another way to extract information, using $ symbol
car.lm$coefficients

car.lm$fitted.values
car.lm$residuals

summary(car.lm)[[4]]
# if we do [[4]], it just gives certain part of the summary

summary(car.lm)[[4]][, 4]
# just gives the intercept and drat
# 4th block of the summary data

# chapter 17, questio n10
# in class exercise 1

pollen <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17q10HybridPollenSterility.csv"))
pollen
head(pollen)

lmpollen <- lm(proportionSterile ~ geneticDistance, data = pollen)
lmpollen
fitted(lmpollen)
residuals(lmpollen)

# plotting the raw data
ggplot(data = pollen, aes(x = geneticDistance, y = proportionSterile))+
  geom_point()+ xlab("Genetic Distance")+
  ylab("Proportion Sterile")+
  geom_smooth(method = "lm", se = FALSE)

# response variable is Y, here for this one Y = proportion

# multiple ways to plot the data
plot(pollen$geneticDistance, pollen$proportionSterile)
plot(proportionSterile ~ geneticDistance, data = pollen)
abline(lmpollen)

# checking the assumptions

# checking the normality
shapiro.test(residuals(lmpollen)) # p-value 0.45, >0.05, so it is normal
hist(residuals(lmpollen)) # passes normality

# checking the assumption of equal variance
plot(residuals(lmpollen) ~ pollen$geneticDistance)
abline( h = 0)

# checking the assumption of linearity
plot(residuals(lmpollen) ~ fitted(lmpollen))
abline( h =0)
# it passes the assumptions


# transforming the proportional data
pollen$trans.prop <- asin(sqrt(pollen$proportionSterile))

lmpollen2 <- lm(trans.prop ~ geneticDistance, data = pollen)
lmpollen2

# check assumptions
# normal check
shapiro.test(residuals(lmpollen2))
hist(residuals(lmpollen2))

# variance check
plot(residuals(lmpollen2) ~ pollen$geneticDistance)
abline(h = 0)

# linear check
plot(residuals(lmpollen2) ~ fitted(lmpollen2))
abline(h = 0)
# looks a lot better than without transforming

# plotting the data
ggplot(pollen, aes(x = geneticDistance, y = trans.prop))+
  geom_point()+ xlab("Genetic Distance")+
  ylab("Proportion sterile (transformed")+
  geom_smooth(method = "lm", se =FALSE)


# 1.4 another example

library(car)
data("Prestige")

?Prestige

# Y = prestige (because that is what we are trying to predict)
# X = income 
head(Prestige)
lm1 <- lm(prestige ~ income, data = Prestige)
lm1

# checking the assumptions for linear model
# normality check
shapiro.test(residuals(lm1)) # p-value very low, doesn't pass the normality test
hist(residuals(lm1))
# looks normal for histogram
# central limit theorem: large sample means we can assume normality

# variance check
plot(residuals(lm1) ~ Prestige$income)
abline(h=0)
# does not pass the assumptions

# linearity check
plot(residuals(lm1) ~ fitted(lm1))
abline(h = 0)
# does not pass the assumptions of linearity check

# since variance and linearity check assumptions is not met,
# we can't conclude anything. Linear model not a good fit.

colnames(Prestige)
ggplot(Prestige, aes(x = income, y = prestige))+
  geom_point()

# try log transformation
# transforming Y
Prestige$logpres <- log(Prestige$prestige)

# graphing the plot
ggplot(Prestige, aes(x = income, y = logpres))+
  geom_point()
# doesn't look better

# lets do variance check
# variance check
Prestige.lm2 <- lm(logpres ~ income, data = Prestige)
plot(residuals(Prestige.lm2) ~ Prestige$income)
abline(h=0)

# a log transformation of Y variable did not make it any better

# summary of regression
summary(Prestige.lm2)
# for the slope of the line we report t-value
# since the p-value of the "income" is <0.05, we can say that
# the slope is significantly different from 0.
