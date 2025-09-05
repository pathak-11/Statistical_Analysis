# 1.1 ANOVA

# 1.2 Welch's one-way Anova (normal, unequal variance)
seeds <- read.csv("seedWeight.csv")
head(seeds)

# plotting the data
library(ggplot2)

ggplot(data = seeds, aes(x = treatment, y = seedWeight))+
 xlab("Treatment")+ ylab("Seed weight")+
   geom_boxplot()


# testing normality
seed.lm <- lm(seedWeight ~ treatment, data = seeds)

shapiro.test(residuals(seed.lm))
# p- value 0.12, # normal

# variance test
library(car)
leveneTest(seedWeight ~ treatment, data = seeds)
# levene test suggest that it is not equal

# calculating variance of each of groups
library(dplyr)

seed.var <- seeds%>%
  group_by(treatment)%>%
  summarise(seedvar = var(seedWeight))
seed.var

# greatest one / lowest one
0.332 / 0.0173
# 19.2 fold difference between the large and small group variance
# conclude that variance is unequal
# since the variance difference is > 10 fold

# the test for normal data and unequla variance is

# Welch's oneway anova
oneway.test(seedWeight ~ treatment, data =seeds, var.equal = FALSE)

# here p-value < 0.05 conclude that there is a significant difference between the group
# F (2,19.4)= 4.18, P= 0.03


# 1.3 Kruskal wallis test (not normal)

data("airquality")
head(airquality)
? airquality

# plotting the data
library(ggplot2)

# to make month as a factor
airquality$Month <- as.factor(airquality$Month)

ggplot(data = airquality, aes(x = Month, y = Ozone))+
  geom_boxplot()+ xlab("Month") + ylab("Ozone (ppb)")

# note on NA in data
# use the na.rm when there is NA in the data
 mean(airquality$Ozone, na.rm = TRUE)

 # in class exercise
 
 # test normality
 airquality.lm <- lm(Ozone ~ Month, data = airquality)

 shapiro.test(residuals(airquality.lm))
# not normal (p value here very less) 
 hist(residuals(airquality.lm))
# not normal, based on shapiro test
# and because of skewed histogram of the residuals
# for this example we will say it is NOT NORMAL
 
# equal variance
 leveneTest(Ozone ~ Month, data = airquality)
# not equal variance
# p-value less than 0.05, so
 
 # variance for each month
 airquality.var <- airquality%>%
   group_by(Month) %>%
   summarise(varO = var(Ozone, na.rm = TRUE))

airquality.var

1575/ 332
# variance difference is 4.7 fold
# thus we can say that it has equal variance since
# the difference is < 10-fold

# data is not Normal and has equal variance
# run test: Kruskal wallis test

# H0 - ozone emissions are the same in each months
# Ha - ozone emissions are different in at least one month. 

kruskal.test(Ozone ~ Month, data = airquality)

# conclude that there is a significant difference in ozone
# for at least one month
# X2 = 29.27, df = 4, P< 0.001

# note that this test uses x2 as the test statistic, so we only use 1 df

# 1.4 Nonparametric post-hoc comparison

library(dunn.test)
dunn.test(airquality$Ozone, airquality$Month,
          method ="bonferroni", list = TRUE)

# if p-value < 0.05, there is a difference
# between the pairs

# what happens when we don't include the "bonferroni" method
dunn.test(airquality$Ozone, airquality$Month,
           list = TRUE)
# we get difference results without the correction
# with the "bonferroni" method we have 
# 4 significantly different with we have 6

# 1.5 planned comparisons of means

# will not be in exams or assignments



# 1.6 Random effects ANOVA - will be in exam
grades <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15q03ExamGrades.csv")
head(grades)

# first we need to reshape the data
library(tidyr)

grades_reshape <- grades%>%
  gather(2:3, key = Exam, value = Grade)
grades_reshape

library(nlme)
gradeANOVA <- lme(fixed = Grade ~ 1,
                  random = ~1|individual,
                  data = grades_reshape)
gradeANOVA
VarCorr(gradeANOVA)

# calculate the repeatability
# from notes
# repeatability = var.Among / (var.Among + var,within)

# var.among = 22.07
# var.within = 119.56

22.07 /(22.07+ 119.56)
# 0.15
# 15.6 % repeat ability

# last in class activity
# doing same ANOVA test but for temperature and months

# plotting the data
# as factor (to make temp as factor)
airquality$Month <- as.factor(airquality$Month)


ggplot(airquality, aes(x = Month, y = Temp))+
  geom_boxplot()

# normality
temp.lm <- lm(Temp ~ Month, data = airquality)
shapiro.test(residuals(temp.lm))
# large sample size can assume normal (central limit theorem)

hist(residuals(temp.lm))

# equal variance
leveneTest(Temp ~ Month, data = airquality)

# variance difference between the group
airqualityvar <- airquality%>%
  group_by(Month)%>%
  summarise(vartemp = var(Temp))
airqualityvar

69.8/18.6
# since the difference in variance is 3.75 fold
# we can say that variance is equal
# data is normal and have equal variance
# all assumptions passed

# correct test
anova(temp.lm)
# there is at least one difference between
# the months and temperature

# run the post hoc test
# we did ANOVA, so tukey test as post-hoc

TukeyHSD(aov(Temp ~ Month, data = airquality))
# here 9-6 and 8-7 has more than 0.05

# if the p-value from this test is greater than 0.05
# they are the same 

# 5 -C
# 6 -A
# 7 -B
# 8 -B
# 9 -A

library(multcomp)


airquality.aov <- aov(Temp ~ Month, data = airquality)
airquality.tukey <- glht(airquality.aov, linfct = mcp(Month = "Tukey"))
cld(airquality.tukey)

