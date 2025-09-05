# Assignment 2

#appropriate chart for one variable data- histogram
#importing data - read.csv
convictions <- read.csv("chap02q22CriminalConvictions.csv")
convictions

## creating a histogram from the data
library(ggplot2)

colnames(convictions)

ggplot(data = convictions,
       aes(x = numberConvictions)) + # the x variable
       geom_bar(, fill = "pink", color ="black")+
       xlab("Number of convictions")+
       ylab("Frequency")+
       theme_bw()
       

# Chapter 3 #28

#importing data = read.csv
seaurchin <- read.csv("chap03q23ZebraFishBoldness.csv")
seaurchin
library(ggplot2)

# creating boxplot using ggplot

ggplot(data = seaurchin,
       aes(x = genotype, y = secondsAggressiveActivity))+
  geom_boxplot() 

#creating stripchart
stripchart(seaurchin$secondsAggressiveActivity ~ seaurchin$genotype, #makes the strip
           pch = 3, # points to +
           ylab = "agreesive activity", # y axis
           xlab ="Genotype", # x axis
           method= "jitter", # to spread points apart
           vertical = TRUE) #to make it vertical
 

# Chapter 7 #31

library(binom)



# null hypothesis for this problem would be that cook pine tree is equally likely to lean towards the equator as well as away from the equator
#meaning 0.5
# alternate hypothesis - the proportion of trees leaning towards the equator is not equal (not equal to 0.5)

#a calculating the best estimate of the proportion of cook pine trees leaning
n<- 256
leaning_towards <- 233
pHat <- leaning_towards/n
pHat
round(pHat, 3)

#b calculating standard error of the estimate
pHat <- 0.91
 n <- 256
Standarderror <- sqrt((pHat*(1-pHat))/n)
Standarderror
round(Standarderror, 3)

#c 95% confidence interval 
binom.confint(x = 233, n = 256, methods = "agresti-coull")

#d calculating the best estimate of the proportion of cook pine trees leaning
binom.test(x = 233, n = 256, p = 0.5)

# e 
#confidence interval after reducing the sample size was reduced by a factor of 10
binom.test(x = 23, n = 26, p = 0.5)
binom.confint(x = 23, n = 26, methods = "agresti-coull")


