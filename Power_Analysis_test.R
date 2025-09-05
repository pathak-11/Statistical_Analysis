#### power analysis and multiple explanatory variables

# 1.1 power analysis
? power.prop.test
? power.t.test
? power.anova.test

# 1.1.1 T test

power.t.test(delta = 5, sd = 3.5,
             sig.level = 0.05, power = 0.8, 
             type = "two.sample")
# n here is 9 for each group
# in this example we have 2 groups so
# 9*2 = 18 samples
# number of groups is given in the problem

# in class exercise 1
# how does SD affects sample size
power.t.test(delta =5,
             sd = 10,
             sig.level = 0.05,
             power = 0.8,
             type = "two.sample")
# SD = 10, number of samples also increases
# the higher the SD, the higher the samples "n"

# 2
# when sample size male =7, female = 7, 
# what is the power given N from previous example
power.t.test(delta =5,
             sd = 3.5,
             sig.level = 0.05,
             n = 7,
             type = "two.sample")
# power is 0.69
# sample size decrease, power also decrease
# in our first example power was 0.8, and n was 9

# 3
# what is your sample size
# planned for paired-t-test,
power.t.test(delta =2,
             sd = 2.3,
             sig.level = 0.05,
             power = 0.8,
             type = "paired")
# sample size n = 13

# 1.1.2 ANOVA

# variance = SD^2
# within groups
4^2

# among group variance
# getting the group means and using the 
# variance function
group.means <- c(5,8,3,9)
var(group.means) # 7.58

# ready to run our power analysis for ANOVA
power.anova.test(groups = 4,
                 between.var = 7.57,
                 within.var = 16,
                 sig.level = 0.05,
                 power = 0.8)
# n is 9 for each group
# 9*4 = 36

# in class exercise
# within groups 
# variance = SD^2
32^2 # 1024

# among group variance
groupmean <- c(105, 90, 80)
var(groupmean) # 158.33

power.anova.test(groups = 3,
                 between.var = 158.33,
                 within.var = 32^2,
                 sig.level = 0.05,
                 power = 0.8)
# n is 33 for each group
# we've 3 groups, so 33*3 = 99, total samples


# 1.2 ANOVA with two factors

data("ToothGrowth")
head(ToothGrowth)

str(ToothGrowth)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
# lenght is numerical = Y variable
# dose and supp as factor = X variable

# check the interaction between two X variables
interaction.plot(ToothGrowth$supp, ToothGrowth$dose, ToothGrowth$len)
interaction.plot(ToothGrowth$ dose, ToothGrowth$supp, ToothGrowth$len)

# if the lines are not parallel or touching each other
# then there is an interaction between the X variable

# graph of the data
library(ggplot2)

# way to graph when given 2 factors
colnames(ToothGrowth)
ggplot(ToothGrowth, aes(x = supp, y =len,
                        fill = dose))+
  geom_boxplot()


tooth.lm <- lm(len ~ supp +
                 dose + # this adds more X variables
                 supp: dose,  # this part is the interaction
               data = ToothGrowth)

# another way to do this 
# short cut way to do this
tooth.lm2 <- lm(len ~supp*dose, data = ToothGrowth)

tooth.lm
tooth.lm2

# now lets see the results, using ANOVA
anova(tooth.lm)

# reject the H0, and conclude that the supplement has an 
# effect
# F = 15.57, df = 1,54, P< 0.001

# Reject the null for dose, and conclude that
# dose has an effect
# F = 92, df= 2,54, P<0.001

# Reject the Null and conclude that there is
# an interaction between dose and supplement
# F = 4.11, df = 2,54, P = 0.02


# 1.2.1 Testing Assumptions

# in class exercise

# normality check
shapiro.test(residuals(tooth.lm))
# normal
hist(residuals(tooth.lm))

plot(residuals(tooth.lm) ~ fitted(tooth.lm))
# looks linear
plot(residuals(tooth.lm) ~ ToothGrowth$dose)
# looks equal variance
plot(residuals(tooth.lm) ~ ToothGrowth$supp)
# looks equal variance


# 1.3 Practice
library(boot)
data("poisons")
head(poisons)

# graphing the raw data
library(ggplot2)
ggplot(poisons, aes(x = treat,
                    y = time,
                     fill = poison))+
  geom_boxplot() + xlab("Treatment")+
  ylab("Time (in 10 Hours)")


# anova
# lm model
lmpoison <- lm(time ~ poison * treat, data = poisons)
anova(lmpoison)

# no interaction between treatment and poison
# so not significant
# but posion and treat are significant
# mean


# Test assumptions

# normality check
shapiro.test(residuals(lmpoison))
# not normal
hist(residuals(lmpoison))
# not normal

# linearity test
plot(residuals(lmpoison) ~ fitted(lmpoison))
# has a funnel shape, so not linear

# equal variance check
plot(residuals(lmpoison) ~ poisons$poison)
# not equal variance; one boxplot a lot smaller

plot(residuals(lmpoison) ~ poisons$treat)

# levene test for variance, we can use this as well
library(car)
leveneTest(residuals(lmpoison) ~ poisons$poison)
# test suggest not equal variance
leveneTest(residuals(lmpoison) ~ poisons$treat)
# failed equal variance

#

