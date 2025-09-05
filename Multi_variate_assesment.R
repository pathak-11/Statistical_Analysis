## Assignment 5

# chapter 16, question 24

biopsy <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter16/chap16q24AntibodyTumorScreening.csv"))
biopsy
head(biopsy)

# making an appropriate graph
library(ggplot2)
# we are doing scatterplot as both of these are numerical data
ggplot(biopsy, aes(x = tumorSize, y = MIB1Index))+
  geom_point()+
  theme_bw()+
  xlab("MIB-1 Index")+
  ylab("Tumor sixe (mm")

# assumptions
library(MVN)
# test for multivariate-normality
mvn(biopsy, mvnTest = "hz")

# MVN test shows the data is not normal

# our data failed the assumptions so doing Spearman's rank correlation
cor.test(biopsy$tumorSize, biopsy$MIB1Index, method = "spearm")

# standard error
# n= 22
SE <- sqrt((1-0.0550^2)/(22-2))
SE


# 2
# chapter 17, question 29
grassland <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17q29grasslandDisease.csv"))
head(grassland)

lmgrassland <- lm(abundance ~ percentDiseased, data = grassland)
lmgrassland
residuals(lmgrassland)
fitted(lmgrassland)

# plotting the data
ggplot(grassland, aes(x = abundance, y = percentDiseased))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  xlab(" Plant abundance (% cover) ") +
  ylab("Percent of tissue showing disease (mean %) ")


# checking the assumptions 

# Checking normality assumption

shapiro.test(residuals(lmgrassland)) 

hist(residuals(lmgrassland))

summary(lmgrassland)
# suggests not normal

# since the Pr(<|t) of percentdisease is < 0.05, rejected the null hypothesis
#Percent of tissue showing disease have relationship
# with the plant abundance.

# Checking assumption of linearity
plot(residuals(lmgrassland)~fitted(lmgrassland))
abline(h=0)
#does not meet the assumption of linearity

# Checking for equal variance
plot(residuals(lmgrassland)~ grassland$abundance)
abline(h=0)
abline(h= c(5, -5), col ="red", lty =2) 
# does not meet the assumption of equal variance

# transforming both variables
grassland$logAb <- log(grassland$abundance)
grassland$logPD <- log(grassland$percentDiseased)


lmtransform <- lm(logPD ~ logAb, data = grassland)
residuals(lmtransform)
fitted(lmtransform)

# again checking assumptions with transformed data
# testing normality
shapiro.test(residuals(lmtransform))
hist(residuals(lmtransform))
# normality fine

# testing linearity
plot(residuals(lmtransform)~ fitted(lmtransform))
abline(h=0)
# meets the assumption of linearity

# checking assumption of equal variance
plot(residuals(lmtransform) ~ grassland$logAb)
# meets the assumption of equal variance

# summary of regression
summary(lmtransform)

# rejected the null hypothesis, Pr(>|t) is less than 0.05,
# meaning that there is relationship between plant abundance and
# percent of tissue disease.
# ( t = 3.07, df = 41, p-value = 0.004)

# creating graph shpwing the transformed data with the regression line
ggplot(data = grassland, aes(x = logAb, y = logPD))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  xlab(" Plant abundace (% cover")+
  ylab("Percent of tissue showing disease (mean %)")


