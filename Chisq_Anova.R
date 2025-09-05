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
