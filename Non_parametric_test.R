## Assignment 4

# question 1. chapter 12, question 19
electricfish <- read.csv("chap12q19ElectricFish.csv")
electricfish
head(electricfish)

# a) mean difference in the number of electric fish in the upstream and downstream
electricfish$diff <- electricfish$speciesDownstream - electricfish$speciesUpstream

meandiff <- mean(electricfish$diff)
meandiff

# 95% confidence interval
sd(electricfish$diff)

length(electricfish$tributary) 
# number of sample (samplesize)

se <- sd(electricfish$diff)/ sqrt(length(electricfish$diff))
se
CI95 <- qt(0.975, df =11)*SE
CI95
upperlimit <- meandiff + CI95
upperlimit
lowerlimit <- meandiff - CI95
lowerlimit

# checking assumptions of the test
hist(electricfish$diff)
shapiro.test(electricfish$diff)

t.test(electricfish$speciesDownstream, electricfish$speciesUpstream, paired = TRUE)


# chapter 13, question 29

dengue <- read.csv("chap13q29WolbachiaAndDengue.csv")
dengue
head(dengue)

# 1. creating the graph
library(ggplot2)
ggplot(data = dengue, aes(x = strain, y = viralTiter))+
  geom_boxplot()+
  theme_bw() + xlab("Strain")+
  ylab("Viral Titers")

# checking the normality with shapiro-wilk in the both groups
library(dplyr)
dengue %>%
  group_by(strain) %>%
  summarise(Meantiter <- mean(viralTiter),
            Variancetiter <- var(viralTiter),
            Count = n(),
            Normalitytest = shapiro.test(viralTiter)$p.value)

# histogram for each group to see the distribution

ggplot(data = dengue, aes(viralTiter))+
  geom_histogram()+
  facet_wrap(~strain)

# levene's test
library(car)
leveneTest(viralTiter~strain, data = dengue) 
# failed to reject the null hypothesis
# both of the groups have equal variance

# non-parametric test, wilcox rank-sum test
wilcox.test(viralTiter~strain, data = dengue)
