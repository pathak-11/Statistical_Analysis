#### T-test 

pnorm()
pnorm(1)
pnorm(-2, mean =5, sd =10)


pnorm( 1.5, lower.tail= FALSE)
?pnorm
pnorm(1.5)
pnorm(1.5, lower.tail = TRUE)

#calculating -1.5 to 1.5
pnorm(1.5, lower.tail = TRUE-pnorm(-1.5, lower.tail = TRUE))
pnorm(1.5)-pnorm(-1.5)

# t- distribution
pt(5, df =5)

pt(5, df=5, lower.tail = FALSE)
?pt
# the default is lower.tail=TRUE, so this command will
#return the area under the curve >5

# exercise 1
pnorm(-2, mean =5, sd=10)
pnorm(-0.05, mean = 0.037, sd = 0.385)

pnorm(-0.05, mean = 0.037, sd = 0.385)

 # testing data for Normality

# checking if the data is normal
swim<-read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11q13SyrupSwimming.csv")
swim
head(swim)

qqnorm(swim$relativeSpeedInSyrup)
qqline(swim$relativeSpeedInSyrup, lty =2) # lty for different type of line
# looking to see if the most of the points are in the line

# always try other ways
# 1.2.2
# shapiro-wilk test for normality

shapiro.test(swim$relativeSpeedInSyrup)
# if the p-value >0.05, the tests suggests that the data is normal
# shapiro test has p-value = 0.32, thus it suggests it's had normal ditribution

# other tests for normality and caveats
# but always graph and use your best judgement

# one-sample t-test example
bodyTemp<-read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11e3Temperature.csv")
bodyTemp
head(bodyTemp)

# first thing to do, graph the data
hist(bodyTemp$temperature)
#it looks normal- has that normal bell shape

qqnorm(bodyTemp$temperature)
qqline(bodyTemp$temperature)
# most points are within qq line suggest normal

shapiro.test(bodyTemp$temperature)
# p-value =0.7, suggest that the data is normal

# assumptions for normality meet for t-test
# so we run it to answer does samples temperatures followed known average

t.test(bodyTemp$temperature, mu= 98.6,
       alternative = 'two.sided',
       conf.level = 0.95)
# test statistic= -0.56, df = 24, p-value = 0.58
# since p-value is > 0.05, we cannot reject the null hypothesis
#conclude that the body temperature is not different
# from 98.6(t= -0.56, df= 24, P= 0.58)

# two-sample t-test, paired data

death<-read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q01DeathAndTaxes.csv")
View(death)
head(death)

# adding a difference column to the data
death$diff <- death$HigherTaxDeaths - death$lowerTaxDeaths

# check for normality
hist(death$diff)

qqnorm(death$diff)
qqline(death$diff)

shapiro.test(death$diff)
# passes normality due to shapiro test and qq plot
# justify why you choose to say normal 

# run t.test - we are assuming that the data passes normality
t.test(death$diff, mu = 0)

# if we didn't calculate the difference could also do
t.test(death$HigherTaxDeaths, death$lowerTaxDeaths,
       paired = TRUE)

# 1.5 TWO-SAMPLE t-test where the data is independent

beerDat = read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q15BeerAndMosquitoes.csv")
beerDat

# plot the data

boxplot(change~ drink, data = beerDat, 
        xlab = "Treatment",
        ylab = "Change in Activation")

# checking for normality and equal variance
# normality boxplot is equal in top on bottom
# variance is equal when both boxplot look similar

# checking for normality

beer <- subset(beerDat, drink =='beer')
water <- subset(beerDat, drink =='water')

shapiro.test(water$change)
shapiro.test(beer$change)
# should know that both has p-value> 0.05, suggest that both is normal

var.test(beer$change, water$change)
# focusing on p-value
# p-value of var.test > 0.05, suggests that variance is equal for the two groups

t.test(change ~ drink, data= beerDat, var.equal = TRUE)
# that var.equal is the whole point is doing this, so make sure var.equal is true
# we set variance equal to TRUE in the t.test since
#var.test suggest it is equal

# we can reject the null hypothesis, and conclude that
# mosquitoes are more attracted to those that drink beer
#than water
# ( t =3.19, df = 41, and P= 0.03)

# some options to consider when assumptions aren't met

# test for equal variance when the assumptions for normality are not met

library(car)
# just an eg on running Levene's test
leveneTest(change ~ drink, data = beerDat)

# welch's t-test with unequal variance (but still normally distributes)
# t.test (y ~ x, var.equal = FALSE)

### in class exercise
# 1
arm.height <- c(0.91, 1.22, 0.73, 1.11, 1.15, 0.93, 1.12, 1.01, 1.05, 0.94, 1.18, 0.98, 1.04, 0.99, 1.07)
arm.height
hist(arm.height) # mostly bell shaped
qqnorm(arm.height) 
qqline(arm.height) # suggest normal
shapiro.test(arm.height) # suggests normal

t.test(arm.height, mu =1)

# in class exercise 2
library(dplyr)
sparrowss <- read.csv("Sparrows-1.csv")

SSTS <- sparrowss %>%
  filter(Species == 'SSTS')

# always plot data
library(ggplot2)
ggplot(SSTS, aes(x = Sex, y= Tarsus))+
  geom_boxplot()

Male <- SSTS %>%
  filter(Sex == 'Male')
Female <- SSTS %>%
  filter(Sex == 'Female')

hist(Male$Tarsus) # looks normal
hist(Female$Tarsus) # looks normal

qqnorm(Male$Tarsus) 
qqline(Male$Tarsus) # looks normal
qqnorm(Female$Tarsus)
qqline(Female$Tarsus) # looks normal

shapiro.test(Male$Tarsus) # it says it doesnt look normal p-value < 0.05
shapiro.test(Female$Tarsus) # it suggest it doesn't look normal as p-value < 0.05
# shapiro.test has issues with large sample size, so it's better to graph it

# can assume normality based on graph and qqplot
# Central Limit Theorem when the sample size is > 30
# we can assume normality

# variance test
# test for equal variance

var.test(Male$Tarsus, Female$Tarsus)
# we can say that variance is equal
t.test(Male$Tarsus, Female$Tarsus, var.equal = TRUE)


# EXERCISE 3
spiders<-read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter03/chap03e2SpiderAmputation.csv"))

library(tidyr)

spider.diff <- spiders %>%
  spread(key = treatment, value = speed) %>%
  mutate(diff = after - before)
head(spider.diff)
 
hist(spider.diff$diff) # not very good, probably not normal
shapiro.test(spider.diff$diff) # normal based on p-value
qqnorm(spider.diff$diff)
qqline(spider.diff$diff) # normal)
# normal due to shapiro and qqnorm

# if we didn't calculate diff column, we have to do two-sample test
# but we have that, so we do one sample test

t.test(spider.diff$diff, mu = 0)
# mu = 0, is default, you can run the test without it
library(ggplot2)
ggplot(spiders, aes(x=treatment,y=speed))+geom_boxplot()+
  ylab("Speed cm/s") + theme_bw()  


mean(spider.diff$diff)
# here comparing means to each other, here we will do n-1 # comparing averages that's why
sd(spider.diff$diff)/sqrt(15)

# reject the null hypothesis, spiders run on average 
# 1.2cm/s faster after
# amputation (SE = 0.28, t =4.42, df = 15, P< 0.001)

