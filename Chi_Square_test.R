

# Question 1, chapter 8/ 25

#The National Electronic Injury Surveillance System (Consumer Product Safety Commission 2018) recorded the number of injuries 
#associated with computers and video games. A sample of these injuries (all severe enough to merit admission to an emergency room) 
#is recorded in the table below, according to the day of the week of the admission. Test whether hospital admissions occur with equal 
#probability on each day of the week. Assume that each day has the same frequency of occurrence over the time interval investigated.
# Day of week Number of emergency room admissions for computer or video game injuries
# Sunday 98
# Monday 93
# Tuesday 91
# Wednesday 86
# Thursday 104
# Friday 84
# Saturday 94



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

# a) Make a graph of the data. 
# b) Use the appropriate statistical test, to test for an association between plasma transfusion and patient outcome. 
# State your null and alternative hypotheses. 
# Check assumptions and make a conclusion. 
# Perform the correct test. 
#  Make your conclusion (in words). This final conclusion sentence should also include your test statistic, df and p-value. 
# c) Calculate the odds ratio of death in the group that did not receive the plasma transfusion, and interpret this number (i.e., were they more likely or less likely to die?).
# Make sure you define what is the “success” and what is the “treatment of interest” as this will influence how you set up your matrix. 
# Treatment of patients whose injury might cause massive blood loss has involved giving them plasma transfusions after they had been admitted to the emergency room. 

# Sperry et al. (2018) investigated whether earlier use of this treatment—by emergency first responders before patients reached the hospital—could be effective. 
# In a randomized control trial, patients with trauma either did or did not receive a plasma transfusion during air medical transport. (All patients also received standard care.)
# Of the 220 patients who received plasma, 53 died within 30 days after injury. Of the 261 patients who received only standard care, 89 died within 30 days. 


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

