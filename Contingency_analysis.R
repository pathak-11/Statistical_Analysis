# contingency analysis

#answer the question does two categorical variables have an association


# H0: Passengers survival is independent of sex
# Ha: Passesngers survival is Not independent of sex

install.packages("titanic")

library(titanic)
data("titanic_train")
head(titanic_train)

surv <- table(titanic_train$Survived, titanic_train$Sex)
surv
#here 0 = dead, and 1 = survived

# creates a proportion table
prop.table(surv)

# run the contingency test
chisq.test(surv)

# can also turn off the Yate's continuity correction with:
chisq.test(surv, correct = FALSE)
# you turn off the yate's continuity if you have really large data

# p-value <0.001, we reject the null and say that the passengers chance of surviving was not independent of sex


# another example

# does survival have an association with class?
surv_class <- table(titanic_train$Survived, titanic_train$Pclass)


# run the test

chisq.test(surv_class)

# p-value <- 0.001, reject the null and conclude that passengers surviving is not independent to their class


# Graphing data examples

# Mosaic plot

surv_class <- table(titanic_train$Pclass, titanic_train$Survived)


mosaicplot(surv_class,
           main = "Survival of passengers on the Titanic")

surv_class_matrix <- matrix(surv_class, 
                            nrow = 3,
                            dimnames = list(class = c("First","Second","Third"),
                                            Outcome = c("Died", "Survived")))
surv_class_matrix


# mosaic plot

mosaicplot(surv_class_matrix, main = "Survival of passengers on the Titanic",
           color = c("orange","red"),
           xlab = "Passenger class",
           ylab = "Relative Frequency",
           cex.axis = 1)


?mosaicplot
# Some alternative graphs in ggplot:


# a stacked bar graph another good way to show data distribution

library(ggplot2)
ggplot(titanic_train, aes(x = Pclass, fill = factor(Survived)))+
  geom_bar() +
  xlab("Class")+ ylab("Count")+
  theme_bw()+
  scale_fill_manual("Survived", values = c("black","goldenrod"))

# a Grouped bar graph

ggplot(titanic_train, aes(x = Pclass, fill = factor(Survived)))+
  geom_bar(position = "dodge")+ # dodge helps us not to stack the data presents as bar type
    xlab("Class")+ ylab("Count")+
  theme_bw()+
  scale_fill_manual("Survived", values = c("pink","yellow"))



# in class exercise

owls <- read.csv("Owls.csv")
owls

# does treatment has any association with the sex of the parent who visited?

# H0: the food treatment is independent of sex of the parent
# Ha: the food treatment is Not independent of sex of the parent

#make the table for chisq test

owlsA <- table(owls$FoodTreatment, owls$SexParent)
owlsA

#run the test 
chisq.test(owlsA)
# since p-value 0.22 is greater than 0.05, so we fail to reject the null hypothesis
# food treatment is independent of sex of the parent

library(ggplot2)
ggplot(owls, aes(x = FoodTreatment, fill = factor(SexParent)))+
  geom_bar(position = "dodge") +
  xlab("Food Treatment")+ ylab("Count")+
  theme_bw()+
  scale_fill_manual("SexParent", values = c("black","goldenrod"))


# 1.2 Calculating odds and odds-ratio

install.packages("mosaic")

library(mosaic)

# for odds ratio we use a 2x2 table
# "Success" is going to be column 1
# "Treatment is going to be the row 2

aspirinM <- matrix(c(1427, 1438, 18515, 18496), nrow =2)
aspirinM
rownames(aspirinM) <- c("Placebo", "Aspirin")
colnames(aspirinM) <- c("Cancer", "No Cancer")
aspirinM

# this is for when "Success" is cancer is yes
# "Treatment" is Aspirin
oddsRatio(aspirinM, verbose = TRUE)
# adding verbose TRUE gives us more information such as CI

# Odds ratio is 1.009

# Since CI includes 1; there is no difference in Cancer chance
# taking the treatment aspirin

chisq.test(aspirinM, correct = FALSE)
# since p-value > 0.05, we fail to reject the null and 
# conclude that there is no association between taking aspirin
# we can turn off Yates correction when our sample is large


# another example with cows

# "Success" is had been bitten - this is our first column
# "Treatment" is has estrus - this is our second column

# create the table

cowsM <- matrix(c(6,15,322,7), nrow = 2)
rownames(cowsM) <- c("Not in Estrus", "Estrus")
colnames(cowsM) <- c("Bitten", "Not Bitten")
cowsM

oddsRatio(cowsM, verbose = TRUE)
# odds ratio is 115
# CI is 34.39 - 384.5

chisq.test(cowsM)$expected
# did not meet the assumptions 
# warning and check of assumptions show that we should use FISHER.test

fisher.test(cowsM)
# when reporting oddratio do not use the value given in the fisher for THIS CLASS

# report the p-value from the FISHER test when assumption not met for the chisq. test

# in class exercise chap 9, Q 7

redspider <- read.csv("chap09q07RedbackSpiderCannibalism.csv")
redspider

# get counts and make prelim table

Table <- table(redspider$secondMaleAcceptance,
               redspider$eatenOrEscape)
Table

# make table to answer questions

# success is second male is accepted - this is our first column
# treatment is first male is eaten - second row
# change the table like first row is accepted eaten

tablee <- matrix(c(22,3,1,6), nrow = 2)
tablee

oddsRatio(tablee, verbose = TRUE)
# odds ratio is 0.023 
# success is less given the treatment, since odds ratio < 1

# another example
# success is when second male is accepted
# treatment is when first male escaped


Table <- table(redspider$secondMaleAcceptance,
               redspider$eatenOrEscape)
Table

table2 <- matrix(c(3,22,6,1), nrow = 2)
table2

oddsRatio(table2, verbose = TRUE)
#odds ratio is 44,
# that means that success is more than 1, when the treatment is given

# last question
# appropriate test 

chisq.test(Table) # assumptions did not met

# Ho : the variables are independent
# Ha : the variables are not independent

# we need to do Fisher test
fisher.test(Table)
# p-value is less than 0.05, so we reject the null hypothesis and conclude that the variables are dependent


# last question= Tree death association

# Ho: tree death is not associated with aspect
# Ha: tree death is associated with aspect

# create the table to make mosaic plot

treefate <- matrix(c(6,4,9,1,19,21,16,24), nrow = 4)
treefate
rownames(treefate) <- c("North","East","South","West")
colnames(treefate) <- c("Dead","Alive")
treefate

# create the mosaic plot

mosaicplot(treefate, main = "Tree fate and aspect",
           color = c("orange","pink"),
           xlab = "Aspect",
           ylab = "Response",
           cex.axis = 1)


# chisq test 

chisq.test(treefate)$expected
# met the assumptions

chisq.test(treefate)
# our p-value is less than 0.05, so we rejected the null hypothesis
# meaning tree death is associated/ related  with the aspect.

# conclusion
# since our p-value(0.037) is less than 0.05, we rejected the null hypothesis. 
# meaning tree death is related to the aspect. (chi-square value = 8.5, df = 3, p-value = 0.037)

