# 1.1 Correlations

bird <- read.csv("Sparrows.csv")
bird
head(bird)

# calculate a correlation between two columns
cor(bird$Tarsus, bird$Head)
# a positive value tells us that these two measurements are positively correlated

# plotting
plot(bird$Tarsus, bird$Head, ylab ="Head length", 
xlab ="Tarsus length")

# exercise 1
cor(bird[, c(3,4,5,6,7)]) 
# one way to do it, to see which one has the strong correlation

# another way
cor(bird[, c(3:7)])

# pair with the highest correlation = head and culmen at 0.72
# pair with the lowest correlation = culmen and wingcrd at 0.44

# 1.1.1 Assumptions
# pearson's correlation coefficeint is based on the assumption of bi-variate normality
#  the relationship between x and y is linear.
# x and y together have a normal distribution.
# the cloud of points in a scatter plot of x and y, has a circular or elliptical shape.
# assumptions for correlation coefficient in page 4 lab 8


library(MVN)

# example of Henze-zirkler test for multivariate normality
mvn(bird[, c(4,5)], mvnTest = "hz")


# graph shows normal but this test says not normal
# sometimes 
# does not meet assumption of bivariate normality
# Also the graph suggests not normal

# Using spearman's correlation
cor(bird$Tarsus, bird$Head)

## 1.1.4 Hypothesis testing
cor.test(bird$Tarsus, bird$Head)
# Conclusion: Reject the null hypothesis, Tarsus length and Head length are 
# significantly positively correlated (r=0.70, n= 979, P < 0.001).


## If assumptions are not met
cor.test(bird$Tarsus, bird$Head, method = "spearm")
# Conclusion: Reject the null hypothesis, Tarsus length and Head length are 
# significantly positively correlated (Spearman's r=0.53, n= 979, P < 0.001).

# In class exercise (Chapter 16, Example 5)
indianrope <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter16/chap16e5IndianRopeTrick.csv"))
head(indianrope)

# Checking the assumptions
# Plotting the data
ggplot(data= indianrope, aes(x= impressivenessScore, y= years)) +
  geom_point()+
  xlab("Years") +
  ylab("Impressiveness Score")

# Checking bivariate normality
mvn(indianrope, mvnTest = "hz") 
# does not meet assumption of bivariate normality
# Also the graph suggests not normal

# Using spearman's correlation
cor.test(indianrope$impressivenessScore, indianrope$years, method= "spearm")

# Conclusion: There is significanlty positive correlation between years and impressiveness score 
#(Spearman's r= 0.78, n = 21, P < 0.001).


# If we say it meets the normality assumption based on the graph
cor.test(indianrope$impressivenessScore, indianrope$years)


### 1.2 Simple Linear Regression
lions <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17e1LionNoses.csv")
head(lions)
# Checking assumptions of regression
plot(ageInYears~proportionBlack, data = lions, xlab = " Proportion of Black pigment on nose",
     ylab= "Age (years")

library(ggplot2)
ggplot(data = lions, aes(x= proportionBlack, y= ageInYears)) +
  geom_point() +
  xlab(" Proportion of Black pigment on nose") +
  ylab("Age (years")

# Fitting the line in the data
lm(ageInYears~proportionBlack, data = lions)
# Equation for the line
# Y = 0.88 + 10.65X

# To check the summary of the linear model
lmlion <- lm(ageInYears~proportionBlack, data = lions)
summary(lmlion)

# P for proportionBlack < 0.001 (Pr (>|t|) < 0.05), indicates that the slope is important
# P for slope > 0.05 ((Pr (>|t|) > 0.05)), means that the slope is not important and is same as 0.


# Example to plot the linear model in the plot
plot(ageInYears~proportionBlack, data = lions, xlab = " Proportion of Black pigment on nose",
     ylab= "Age (years")

abline(lmlion)

# To add linear model in the ggplot
ggplot(data = lions, aes(x= proportionBlack, y= ageInYears)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(" Proportion of Black pigment on nose") +
  ylab("Age (years")

?geom_smooth


# Checking assumptions for the simple linear regression
residuals(lmlion)
fitted(lmlion)


# Checking for normality
shapiro.test(residuals(lmlion)) # suggests it is normal

hist(residuals(lmlion))

qqnorm(residuals(lmlion))
qqline(residuals(lmlion))

# Suggest normality

## Checking for equal variance
plot(residuals(lmlion)~lions$proportionBlack)
abline(h=0)
abline(h= c(2,-2), col= "red", lty = 2)

# Checking assumption of linearity (checking for random spread of points)
plot(residuals(lmlion)~fitted(lmlion))
abline(h=0)

# To get all the plots into one
par(mfrow= c(2,2))
plot(lmlion)


# In class exercise
vegetation <- read.csv("Vegetation.csv")
head(vegetation)

#Plotting the data
plot(SpeciesRichness~FallPrec, data = vegetation)


ggplot(data = vegetation, aes(x= FallPrec, y = SpeciesRichness)) +
  geom_point() +
  xlab(" Fall Precipitation") +
  ylab("Species Richness")

# Fitting line in the data
lm(SpeciesRichness~FallPrec, data = vegetation)
lmvegetation <- lm(SpeciesRichness~FallPrec, data = vegetation)
summary(lmvegetation)

# Since the Pr(>|t|) of FallPrec is > 0.05, Fall Precipitation is not a 
#good indicator of Species Richness

ggplot(data = vegetation, aes(x= FallPrec, y= SpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(" Fall Precipitation") +
  ylab("Species Richness")

abline(vegetation)

# Equation of the line: 8.90 + 0.019X

# Checking assumptions
# Checking normality
residuals(lmvegetation)

shapiro.test(residuals(lmvegetation)) 

hist(residuals(lmvegetation))

qqnorm(residuals(lmvegetation))
qqline(residuals(lmvegetation))

# Suggests normal

# Checking for equal variance
plot(residuals(lmvegetation)~FallPrec, data = vegetation)
abline(h=0)
abline(h= c(2, -2), col ="red", lty =2) # suggests equal variance

# Checking assumption of linearity
fitted(lmvegetation)
plot(residuals(lmvegetation)~fitted(lmvegetation))
abline(h=0)


## In class exercise
heat <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/review3/rev3q10HeatLossLeanness.csv")
head(heat)
# Make the plot
ggplot(data= heat, aes(x= bodyLeanness, y= heatLossRate)) +
  geom_point() +
  xlab("Body Leanness") +
  ylab("Heat Loss Rate")

# Fitting line in the data

ggplot(data= heat, aes(x= bodyLeanness, y= heatLossRate)) +
  geom_point() +
  geom_smooth(method= "lm", se = FALSE) +
  xlab("Body Leanness (m/kg)") +
  ylab("Heat Loss Rate (degree C/ min)")

lmheat <- lm(heatLossRate~bodyLeanness, data = heat)
lmheat



# Creating four diagnostic plots
par(mfrow=c(2,2))
plot(lmheat)

# Running linear regression
summary(lmheat) 
# Pr(>|t|) for bodyLeanness < 0.001, thus, Body Leanness can predict heat Loss Rate.

# Equation of the line: Y= - 0.03 + 0.02X

# Checking assumptions
# Checking normality
residuals(lmheat)
shapiro.test(residuals(lmheat)) # suggests normal

hist(residuals(lmheat))

qqnorm(residuals(lmheat))
qqline(residuals(lmheat))

# Checking assumption of equal variance
plot(residuals(lmheat)~bodyLeanness, data = heat)
abline(h= 0)
abline(h= c(-0.01, 0.01), col= "red", lty =2)  # meets assumption of equal variance

# Checking assumption of linearity
fitted(lmheat)
plot(residuals(lmheat)~fitted(lmheat))
abline(h=0) # meets assumption of linear relationship

# Meets all assumptions of simple linear regression
# Y= -0.03 + 0.02X # Equation of the line


