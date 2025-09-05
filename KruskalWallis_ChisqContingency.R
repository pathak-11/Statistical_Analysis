#############################################################################
Practice

# Part 1
power.t.test(delta = 0.4, sd = 0.8, sig.level = 0.05, power = 0.8, type = "paired")
# 34
# its because of it has two pairs
###########################
# Part 2

mouseDat <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/review3/rev3q08OxygenConsumption.csv")
mouseDat
# this is random effect anova, individual mouse as random effect

# a - Variance components within and among mice

# in random effect, there is no fixed and random
# but it is y and x 
library(nlme)
mouseANOVA <- lme(fixed = VO2max ~1,
                  random = ~1|mouse,
                  data = mouseDat)
mouseANOVA
VarCorr(mouseANOVA)

# when you see repeatability, within among, it is
# random effect anova

# variance within group - residuals
# variance among group - intercept

#b - calculate repeatability  
# calculating repeatability
# repeatability = var.Among / (var.Among + var,within)

0.12/(0.12+0.15) # 0.44

###########################
# Part 3

# when you download the data, the last line comes in as a row of NA.
# run the second line of code - it will delete this row for you
crabs <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15q27FiddlerCrabFans.csv")
crabs <- crabs[-85,]
crabs
# a - make a graph
library(ggplot2)

ggplot(crabs, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot()+ ylab("Degrees C per log minute")+
  xlab("Crab type")

# b - mean + SE for each group
library(dplyr)
crabs%>%
  group_by(crabType)%>%
  summarise(meantype = mean(bodyTemperature),
            vartype = var(bodyTemperature),
            sdT = sd(bodyTemperature),
            n =n(),
            seT = sdT/sqrt(n)) 

# HYPOTHEIS
# BODY temp are equal in all crab types
# body temp are different for at least one crab type.


# c - test assumptions
lmcrabs <- lm(bodyTemperature ~ crabType, data =crabs)

shapiro.test(residuals(lmcrabs))
# normal
hist(residuals(lmcrabs))
# equal variance
leveneTest(bodyTemperature ~ crabType, data = crabs)
# seems equal
boxplot(residuals(lmcrabs)~ crabs$crabType)
# seems normal

# e - perform test
anova(lmcrabs)
# significant difference between our crab group # rject H0
# F 3,80 = 20.312, P= 0.001

# f - post hoc analysis

TukeyHSD(aov(bodyTemperature ~ crabType, data = crabs))
library(multcomp)
crabs$crabType <- factor(crabs$crabType)
your.aov <- aov(bodyTemperature ~ crabType, data =crabs)
your.tukey <- glht(your.aov, linfct = mcp(crabType = "Tukey"))
cld(your.tukey)

# female different from everything = a
# interact M = bc
# Male major removed intact = b
# Male minor removed intact = c



# part 4
lifespan <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter18/chap18q07flyLifeSpan.csv"))
lifespan
head(lifespan)

lifespan$treatment <- as.factor(lifespan$treatment)
lifespan$fertility <- as.factor(lifespan$fertility)

# the equation to represent the model
# Y = constant + A + A*B
# lifespandays = constant+ treatment+strain+ treatment*strain
# both A and B are constatnt this it is two-way fixed effected anova


lm1 <- lm(lifespanDays ~ treatment * fertility, data = lifespan)


interaction.plot(lifespan$treatment, lifespan$fertility, lifespan$lifespanDays,
                 type ="b", pch = c(19,1),
                 trace.label = "fertility \n ",
                 xlab = "Treatment", ylab = "Life span in days")

# data balanced or not
table(lifespan$treatment)
# unbalanced

# assumption check
shapiro.test(residuals(lm1))
# not normal
hist(residuals(lm1))
# normal

# linearity
plot(residuals(lm1) ~ fitted(lm1))
# linear

# equal variance
boxplot(residuals(lm1) ~ lifespan$treatment)
#  looks equal
boxplot(residuals(lm1) ~ lifespan$fertility)
# seems equal

#### CAN SKIP THIS JUST SAY EQUAL VARIANCE
library(car)
leveneTest(residuals(lm1) ~ lifespan$treatment) # unequal
lifespan%>%
  group_by(treatment)%>%
  summarise(var = var(lifespanDays))
101.1/36.9
# less than 10 so, equal

leveneTest(residuals(lm1) ~ lifespan$fertility) # unequal
lifespan%>%
  group_by(fertility)%>%
  summarise(var = var(lifespanDays))
87.7/72.6
# less than 10, so can say equal


# correct statistical test
Anova(lm1, type = 3)

# graphing the data
library(ggplot2)
ggplot(lifespan, aes(x = treatment, y = lifespanDays, color = fertility))+
  geom_boxplot()+
  xlab("Treatment")+
  ylab("Number of lifespan days")


# hypothesis
#Main effect of treatments
# H0: treatment doesn’t affect the life span
# Ha: treatment affects the life span.
#Main effect of strains
# H0: strain doesn’t affect the life span.
# Ha: strain affects the life span.
#Interaction:
# H0: treatment and strain doesn’t interact to affect the lifespan.
# Ha: treatment and strain interact to affect the lifespan. 

# conclusion
# two-way fixed effect anova
# no. of lifespan days was significantly different between
# treatments (F 1,838 = 67.89, P<0.001) and fertility
# (F 1,838 = 5.21, P=0.02) and no significant interaction
# of treatments and fertility of females (F1, 838 = 0.36, P= 0.55)


#### assignment 5

mosquito <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15q23MalariaFungusVenom.csv")
mosquito
head(mosquito)

# making a graph
library(ggplot2)
ggplot(data = mosquito, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot()+ xlab(" Treatment group") + ylab("log sporozoite numbers")

# mean and variance
library(dplyr)
mosquitoMV <- mosquito%>%
  group_by(treatmentGroup)%>%
  summarise(meanmosquito = mean(logSporozoiteNumbers),
            varmosquito = var(logSporozoiteNumbers))
mosquitoMV

# one way anova- when not normal
# do kruskal wallis
# if unequal variance only = one.way.test

# checking the assumptions
lmmosquito <- lm (logSporozoiteNumbers ~ treatmentGroup, data = mosquito)

# normality
shapiro.test(residuals(lmmosquito))
# failed the normality
hist(residuals(lmmosquito))
# not normal

# checking equal variance
library(car)
leveneTest(logSporozoiteNumbers ~ treatmentGroup, data = mosquito)
# p-value more than 0.05, so passes the assumption
# equal variance

# H0 - the mean log sporozoites is same across all three groups.
# Ha - there is at least one group that has a different mean log sporozoite.

# since the data is not normal, we will do Kruskal wallis test
kruskal.test(logSporozoiteNumbers ~ treatmentGroup, data = mosquito)
# we rejected the H0
# Concluding that the mean differences is not same among the groups.
# X2 = 22.87, df = 2, p-value= 0.001

# performing post-hoc test
library(dunn.test)
dunn.test(mosquito$logSporozoiteNumbers, mosquito$treatmentGroup,
          method = "Bonferroni", list = TRUE)

# assigning letter/symbol
# control - a
# Scorpine - b
# WT - a


#### chapter 15, question 22
lizard <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15q22LizardSprintSpeed.csv")
lizard
head(lizard)

# random effects anova
library(nlme)
lizardANOVA <- lme(fixed = speed ~1,
                   random = ~1|lizard,
                   data = lizard)
lizardANOVA

VarCorr(lizardANOVA)

# variance within group- use residual
# among group - intercept
# 0.14850944

# variance within groups - residual
# 0.04607057 

# calculating repeatability
# repeatability = var.Among / (var.Among + var,within)
0.14850944 / (0.04607057+0.14850944)
# 0.76 = 76%
# Repeatability measures the similarity of repeated 
# measures on the same subject. Repeatability = 0, 
# a large role of measurement error while repeatability =1, no measurement error.
# Here, our repeatability is 76%, so 76% of the total variance in speed is due to differences between individuals and 24% is due to measurement error.



##### Assignment 7---------


seedbank_all <- read.csv("gsb_db.csv")
seedbank_all

library(dplyr)
seedbank <- seedbank_all %>%
  dplyr::select(Total_species, Habitat_broad, Habitat_degraded, Temp_mean, Biome_zone)

seedbank
head(seedbank)

## is there an association between total number of 
# species in the seed bank, and mean temperature? 

library(ggplot2)
ggplot(seedbank, aes(x = Temp_mean, y = Total_species))+
  geom_point()+ xlab("Mean temperature")+ ylab("Total number of species (n)")

# H0: there is no association between total number of
# species in the seed bank and mean temp. 
# Ha: there is association between total number of
# species in the seed bank and mean temp.

# checking assumptions of bivariate normality
library(MVN)
mvn(seedbank [, c(1,4)], mvnTest = "hz")
# doesn't meet the assumption of bivariate normality

# transforming the data 
# total number of species using square root
seedbank$tspecies <- sqrt(seedbank$Total_species)

mvn(seedbank [, c(4,6)], mvnTest = "hz")
# doesn't meet the assumption of bivariate normality

# transforming mean temp. using square 
seedbank$mtemp <- (seedbank$Temp_mean)^2

mvn(seedbank [, c(4,8)], mvnTest = "hz")
# doesn't meet the assumption of bivariate normality

# data doesn't meet the assumption of bivariate normality
# now using (non-parametric test) spearman's correlation for testing association
cor.test(seedbank$Total_species, seedbank$Temp_mean, method = "spearman")
# p< 0.001


# since p is less than 0.05, we can reject the H0
# meaning that there is significant positive correlation between
# mean temp. and total number of species
# spearman's rank corr. r = 0.122, n = 2872, P<0.01



# b) is there an association between biome zone and type of habitat? [13 points]

# both biome zone and type of habitat is categorical variables
# so we do contingency analysis

# table for contingency analysis
dtable <- table(seedbank$Habitat_broad, seedbank$Biome_zone)
dtable

mosaicplot(dtable,  color = c("skyblue", "yellow", "green", "pink", "royalblue"),
           main = "Mosaic plot of Habitat type and Biome zone",
           xlab = "Habitat type (broad)",
           ylab = "Biome zone",
           cex.axis = 0.7, las = 2)



# H0: biome zone and type of habitat is independent
# Ha: biome zone and type of habitat is not independent.



# assumption check for chi squared contingency test
test <- chisq.test(dtable)
test
test$expected
# since no cell has expected frequency less than 1
# and the percentage of cells less than the expected frequency 
# of 5 is less than 20%

# it passes the assumption of X2 contingency test

# performing the tst
test<- chisq.test(dtable)
test

# p<0.001, we rejected the H0
# meaning we can conclude that habitat type and biome zone
# are not independent.
# (X2 = 455.1, df =16, P<0.001)


#mosaicplot(dtable, main = "mosaic plot of biome zone and habitat type",
#          color = TRUE, xlab = "Habitat type", ylab = "Biome zone")



## C) What predicts total number of species in the seed bank? Consider the explanatory variables habitat
## type, if the habitat is degraded, as well as the interaction between them. 

# removing habitat type "arable" from the data

seed <- seedbank%>%
  filter(Habitat_broad != "Arable")
seed

# removing NA from the data
seed <-na.omit(seed)
seed

# we have two explanatory variable # habitat broad
# habitat degraded
# and both these are categorical variables
# where total number of species is numerical

str(seed)

seed$Habitat_broad <- as.factor(seed$Habitat_broad)
seed$Habitat_degraded <- as.factor(seed$Habitat_degraded)

# plotting graph from the data
ggplot(seed, aes(x = Habitat_broad, y = Total_species, fill = Habitat_degraded))+
  geom_boxplot()+ 
  xlab("Habitat type")+
  ylab("Total number of species")

# checking interaction between variables

interaction.plot(seed$Habitat_broad, seed$Habitat_degraded, seed$Total_species,
                 type = "b", pch = c(19,1),
                 trace.label = "Habitat degradation \ n Habitat _degraded", 
                 xlab = "Habitat type", ylab = "Total number of species")

# H0: Total number of species is not significantly predicted by habitat type, habitat degradation, or the interaction of habitat type and habitat degradation.
#Ha: Total number of species is significantly predicted by habitat type, habitat degradation, or the interaction between habitat type and habitat degradation. 

# checking if we should include interaction or not 
lmseed <- lm(Total_species ~ Habitat_broad* Habitat_degraded, data = seed)

anova(lmseed)
# p-value = 0.038
# it improves the model fit, so we keep interaction in the model

# checking assumptions of two-way anova

# normality
shapiro.test(residuals(lmseed))
# non-normal
hist(residuals(lmseed))
# non-normal
# CLT- we can ignore it, sample size is large

# linearity
plot(residuals(lmseed) ~ fitted(lmseed))
# seems linear

# equal variance for each X
boxplot(residuals(lmseed) ~ seed$Habitat_broad)
# equal
boxplot(residuals(lmseed) ~ seed$Habitat_degraded)
# equal

# levene test as well
library(car)
leveneTest(residuals(lmseed) ~seed$Habitat_broad)
# not equal
leveneTest(residuals(lmseed) ~ seed$Habitat_degraded)
# equal

# checking if data is balanced or not
table(seed$Habitat_broad)
# unbalanced data, and has interaction effect
# so we use anova type 3

Anova(lmseed, type = 3)
summary(lmseed)

# conclusion
# there is significant effect of habitat type on total number of species (F3,2592 = 24.86, P< 0.001)
# there is significant effect of habitat type and habitat degradation (F3, 2592 = 2.80, P< 0.04)
# there is no significant effect of habitat degradation alone(F 1,2592 = 0.0004, P= 0.98)
