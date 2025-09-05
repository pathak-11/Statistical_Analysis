## Assignment 3

# Question 1, chapter 8/ 25

days <- read.csv("chap08q25ComputerInjuries.csv")
days

library(ggplot2)

ggplot(data = days, 
       aes( x = Day))+
  geom_bar(width = 0.5, fill = "pink")+
  theme_bw()+
  xlab("Days")+
  ylab("Number of admissions")+
  ggtitle("Number of emergency room admissions by the days of Week")

emergency <- c(98,93,91,86,104,84,94)
emergency


# B. Hypothesis

#H0 : The probability of admissions occurs equally on each day of the week.
#Ha : #H0 : The probability of admissions does not occur equally on each day of the week.

# C. testing the assumptions for statistical test

result <- chisq.test(emergency)$expected # met the assumptions
result

stattest <- chisq.test(emergency) # p-value 0.81 > 0.05, we fail to reject the null hypothesis.
stattest

# Q.2 {chapter 9, 19}

PlasmaP <- read.csv("chap09q19plasmaTransfusion.csv")
PlasmaP


# creating the table for contingency analysis, matrix
plasma_table <- table(PlasmaP$treatment, PlasmaP$response)
plasma_table

rownames(plasma_table) <- c("No Plasma", "Plasma")
colnames(plasma_table) <- c("Died", "Survived")


# creating mosaic plot for the data
library(mosaic)
mosaicplot(plasma_table, main = "Treatment of Patients and the Outcomes",
           col= c("pink", "yellow"),
           xlab ="Treatment",
           ylab = "Outcome",
           cex.axis = 1.5)


# B. statistical test to test for an association between plasma transfusion and patient outcome
#null hypothesis

# H0: Patients survival and plasma transfusion are independent.
# Ha: Patients survival and plasma transfusion are not independent.


# checking the assumptions

chisq.test(plasma_table)$expected
# assumptions are met

# correct test
test <- chisq.test(plasma_table) # p-value 0.02 < 0.05, so we rejected the null hypothesis
test


#matrix for calculating odds ratio
Deathratio <- matrix(c(53, 89, 167, 172), nrow =2, 
                     dimnames = list(Treatment = c("Transfusion", "Control"),Response = c("Survived","Died")))
Deathratio
 
# odds ratio of death                                                                
oddsRatio(Deathratio, verbose = TRUE) # 1.63 
