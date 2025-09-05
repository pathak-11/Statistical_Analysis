# 1.1 ANOVA

nectar <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15q01HoneybeeCaffeine.csv",header = TRUE)
head(nectar)

library(ggplot2)
colnames(nectar)

# convert column into a factor
nectar$ppmCaffeine <- as.factor(nectar$ppmCaffeine)

# code to plot
ggplot(nectar, aes(x = ppmCaffeine, y = consumptionDifferenceFromControl))+
  geom_boxplot()

# to change the names of the columns
names(nectar) <- c("ppmCaff", "consump.diff")


# in class exercise
library(dplyr)

Summary <- nectar %>%
  group_by(ppmCaff) %>%
  summarise(mean = mean(consump.diff),
            SD = sd(consump.diff),
            count = n())

## 1.1.1 testing assumptions for ANOVA

# first make a linear model, Y~ X
lm.nectar <- lm( consump.diff ~ ppmCaff, nectar)

# Normality check
qqnorm(residuals(lm.nectar))
qqline(residuals(lm.nectar))

# an explicit test with shapiro wilk test
shapiro.test(residuals(lm.nectar))
# looks normal

# equal variance 
boxplot(residuals(lm.nectar) ~ nectar$ppmCaff)

str(nectar)
# good way to check equal variance
library(car)
leveneTest(consump.diff ~ ppmCaff, data = nectar)

# assumptions passed run the test
# H0: the mean of the amount of nectar is not different among caffeine concentration.
# Ha: the mean of the amount of nectar is different among caffeine concentration

anova(lm.nectar)
# anova indicates that there is a 
# significant difference (F(3, 16) = 4.18, P = 0.023)

# Post-hoc comparision - which group(s) are different
# between each other

TukeyHSD(aov(consump.diff ~ ppmCaff, data =nectar))
# you just look athe p adj and compare the values for different group
# and see if they all are bigger than 0.05

# 50 ppm is not different from any group (p is > 0.05)
# for it with other group
# 100 ppm is significantly different from 150 and 200


# 1.4 graphing
# multiple ways to show the graph

# if you want to graph the mean + SE bars need to 
# calculate it
mean.nectar <- nectar %>%
  group_by(ppmCaff) %>%
  summarise( mean = mean(consump.diff),
             se = sd(consump.diff)/sqrt(n()),
             seUpper = mean+ se,
             seLower = mean - se)

# 1.4.1 ggplot

plotCaff <- ggplot()+
  geom_jitter(data = nectar, aes( x = ppmCaff, y = consump.diff),color = "grey", size = 2, height = 0, width = 0.1)+
  theme_bw()+
  xlab(" Caffeine Treatment")+
  ylab("Difference in nectar consumed from control")+
  geom_errorbar(data = mean.nectar, aes(x = ppmCaff, ymin = seLower, ymax = seUpper),
                size =1, width = 0.1)+
  geom_point(data =mean.nectar, aes(x = ppmCaff, y=mean), colour = "red", size = 4)
plotCaff


plotCaff+ geom_text(aes(x =1, y = 0.7, label ="ab"), size = 5)+
  geom_text(aes(x = 2, y = 0.5, label = "a"), size =5)+
  geom_text(aes(x = 3, y = 0.8, label  = "b"), size =5)+
  geom_text(aes(x =4, y=1.2, label = "b"), size =5)


# base graphics
stripchart(consump.diff ~ ppmCaff, data = nectar, method = "jitter", vertical = TRUE,
           pch =1, xlab ="Caffeine Treatment", ylab = "Difference in nectar consumed from control")



# 1.5 practice

data("chickwts")
head(chickwts)

# plotting the data
ggplot(data = chickwts, aes(x = feed, y = weight))+
  geom_boxplot()

# first linear model
lmchickwts <- lm(weight ~ feed, data = chickwts)

# assumptions

# normality
qqnorm(residuals(lmchickwts))
qqline(residuals(lmchickwts))

shapiro.test(residuals(lmchickwts)) # p-val> 0.05,
# so we rejected the H0 of shapiro wilk test and it passed
# looks normal for everything

# check for equal variance
leveneTest(weight ~ feed, data = chickwts)
# passed

# run ANOVA
 anova(lmchickwts)

 # make conclusion
 # we can conclude there is a significant difference in weight among different
 # F (5, 54)= 15.37, p < 0.001
 
 # post- hoc : which groups are different
 TukeyHSD(aov(weight ~ feed, data = chickwts))
 
 # there is a lot here- we can use glht function to do it for us
 
 install.packages("multcomp")
library(multcomp) 

chick.aov <- aov(weight ~ feed, data = chickwts) 
 
chick.turkey <- glht(chick.aov, linfct = mcp(feed = "Tukey"))

cld(chick.turkey) # this adds the letters for us

 
# chapter 15, question 21

cones <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15q21LodgepolePineCones.csv"))
cones
head(cones)

# making the graph, plotting
ggplot(data = cones, aes( x = habitat, y = conemass))+
geom_boxplot()+
  xlab("Habitat")+ ylab("Cone mass (g)")

# testing the assumptions

# first linear model
lmcones <- lm(conemass ~ habitat, data = cones)

# normality
qqnorm(residuals(lmcones))
qqline(residuals(lmcones))

shapiro.test(residuals(lmcones))
# normal

cones$habitat <- as.factor(cones$habitat)

# equal variance
leveneTest( conemass ~ habitat, data = cones)
# equal variance 
# passes variance assumption

# hypothesis
# H0: the mean cone mass is not different between habitats.
# Ha: the mean cone mass is different between habitats.

# run ANOVA
anova(lmcones)

# P value less than 0.05, we rejected the null hypothesis
# meaning that there is a significant difference between habitat groups
# F(2,13) = 50.09, p< 0.01
# island.absent is significantly different from island present and mainland present
# Island present and mainland present are the same.


# Tukey kramer post hoc test
# to see which one is different
TukeyHSD(aov(conemass ~ habitat, data =cones))

# assign letters to show
# island absent - B
# island present - A
# mainland present - A

