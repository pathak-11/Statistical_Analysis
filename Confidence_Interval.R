# question 1
#Chapter 2, Q22 
# The Cambridge Study in Delinquent Development was undertaken in north London (U.K.) to investigate the links between criminal behavior in young men and the 
# socioeconomic factors of their upbringing (Farrington 1994). A cohort of 395 boys was followed for about 20 years, starting at the age of 8 or 9. All of the boys 
# attended six schools located near the research office. The following table shows the total number of criminal convictions by the boys between the start and end of the study.
# The data are available at whitlockschluter3e.zoology.ubc.ca.
# Read the problem in the textbook. Download the data and make the most appropriate graph (you are basically doing part e in the textbook)


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

# Reproduction in sea urchins involves the release of sperm and eggs in the open ocean. Fertilization begins when a sperm bumps into an egg and the sperm protein 
# bindin attaches to recognition sites on the egg surface. Gene sequences of bindin and egg-surface proteins vary greatly between closely related urchin species, and 
# eggs can identify and discriminate between different sperm. In the burrowing sea urchin, Echinometra mathaei, the protein sequence for bindin varies even between 
# populations within the same species. Do these differences affect fertilization? To test this, Palumbi (1999) carried out trials in which a mixture of sperm from AA and BB 
# males, referring to two populations differing in bindin gene sequence, were added to dishes containing eggs from a female from either the AA or the BB population. 
# The results below indicate the fraction of fertilizations of eggs of each of the two types by AA sperm (remaining eggs were fertilized by BB sperm).
# AA females: 0.58, 0.59, 0.69, 0.72, 0.78, 0.78, 0.81, 0.85, 0.85, 0.92, 0.93, 0.95
# BB females: 0.15, 0.22, 0.30, 0.37, 0.38, 0.50, 0.95
# Read the problem in the textbook. Download the data and follow the directions here.

# a)	Make a graph to show the data 
# b)	Make a different graph to show the data 
# c) Which graph do you prefer and why? 

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

# The Cook pine tree is native to New Caledonia but has been planted as an ornamental in many countries. The trees tend to lean to one side as they grow. Johns et al. (2017)
# measured the tilt in 256 individual trees around the world. They found that 233 trees leaned toward the equator (in the Northern Hemisphere they lean south, and in the 
# Southern Hemisphere they lean north), whereas 23 leaned away from the equator. 
# a) What is the best estimate of the proportion of Cook pine trees that lean toward the equator? 
# What is the standard error of the estimate? What does this quantity measure? 
# What is the 95% confidence interval for the proportion of trees leaning toward the equator? As written. Make sure to use the appropriate method 
# Use the binomial test to test the null hypothesis, that trees are just as likely to lean towards the equator as away from the equator
# Run the binomial test 
# What if the sample size was reduced by a factor of 10? (i.e., 23 trees leaned toward the equator out of a total of 26 trees). Would your 95% confidence interval be wider
# or narrower? Would this affect your conclusion? 

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



