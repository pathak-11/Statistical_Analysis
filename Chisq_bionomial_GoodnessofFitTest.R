 # Lab 4 Binomial, X2, and Goodness of Fit Test

# 1.1 Probability Distribution Function in R

#creates random number for us 
# n = number of samples
#mean for the data
#sd for the data

rnorm(n =3, mean =4, sd =2)

# refer to the help? to see what the inputs are for the function
?pnorm
pnorm(2, mean = 3, sd =2)

#this gives us probability of getting success = 3, from 10 trial and size(trials)=10
#when probability of success is 0.2
dbinom(x =3, size = 10, prob = 0.2)
dbinom(x =5, size = 10, prob = 0.2)

# Class Exercise
#this is also asking how many success we get

dbinom( x = 2, size =6, prob = 0.1) # just giving the change of getting 2 successes if the percentage is 10%
# change of getting 2 lefties from a family of 6 is 0.098 (10%)


# 1.2 Binomial Test

#does 3 out of 10 differ significantly from the expectation of 50%
binom.test(x= 3, n=10, p =0.5)
# since p=value is >0.05. we can conclude that the proportion of lefties in the family is not significantly different than that the expected 50%
#(p-hat = 0.3, n=10, P=0.34)

# class exercise 2
binom.test(x = 2, n=6, p = 0.1) # testing significantly different 
# we fail to reject the null hypothesis here

# X2 distribution - when there are more than one categories

#when running these test we are always comparing to a known distribution

# H0: The proportion is expected
# Ha: The proportion is not the same as expected 

#degrees the freedom for this class is going to be 
#( number of categories) - 1


# 1.4.1

#Need to check assumptions for x2 test
# 1. None of the categories have expected frequency <1
# 2. no more than 20% of the categories should have expected frequencies

#page 5 in lab manual has the steps you need to take to do the x2 test


# example of x2 test

# step 1
# H0 : the proportion of colors follows the distribution of the company 
# Ha : the proportion of color does not follow the distribution of the company

#step 2
# determine the expected probability - is provided by the company

expected <- c(0.24, 0.13, 0.16, 0.20, 0.13, 0.14)

# check if the assumptions are violated
#observed values provide
observed<- c(481, 371, 483, 544, 372, 369)

# run this to check if expected assumptions are met
chisq.test(observed, p = expected)$expected #$ expected to pull out the expected numbers from here
# none of the x2 assumptions are violated

# step 4 = need to do if assumptions are violated

#step 5 - conduct the test

chisq.test(observed, p = expected) # magic number is p-value
# we can conclude 

#step 6 - state conclusion
# since the p-value < 0.05, we reject the H0.The color distribution of the compnay
# (x2 = 50.83, df =5, p-value <0.0001)

# class exercise
# H0 : the proportion of colors follows the distribution of the company 
# Ha : the proportion of color does not follow the distribution of the company

MM_observed <- c(12,6,6,11,14,14)
expected <- c(0.24, 0.13, 0.16, 0.20, 0.13, 0.14)

# this is to check our assumption for the x2 test - page 5 of lab manual
chisq.test(MM_observed, p=expected)$expected # gives the 

chisq.test(MM_observed, p = expected)

#since p-value is greater than 0.05 (p>0.05), we fail to reject the H0; the proportion of M&M colors matches the company
# X2 = 10.25, df =5, p-value = 0.07


# class exercise 2

#H0 : the dice results follow equal distribution (fair dice)
#Ha: the dice does not follow equal distribution 

observed <- c(8, 10,9,11,12,10)
expected <- c(1/6, 1/6,1/6,1/6,1/6,1/6 )

# if you leave p blank, R assumes equal expected probability
chisq.test(observed, p = expected)$expected

chisq.test(observed)

# since the p-value is > 0.05, we fail to reject the null hypothesis, the dice is fair
#(x2 =1, df =5, p-value = 0.96)

# class exercise 3

# H0 : the number of groundhog babies born each week during a 6- week spring period is evenly distributed
# Ha: the number of groundhog babies born each week during a 6-week spring period is not evenly distributes

# H0: the births are evenly distributed over the weeks
# Ha : the births are not evenly distributed over the weeks

observed <- c( 2, 0,3,2,1,14)
expected <- c(1/6,1/6,1/6,1/6,1/6,1/6)

chisq.test(observed, p = expected)$expected

# since the assumptions not met (freq <5 for more than 20% of categories)
# fix by combining categories

# we now have week 1 + week 2, week 3+4, week 5+6
expected <- c((2/6), (2/6),(2/6))
observed <- c(2,5,15)
chisq.test(observed, p= expected)$expected

chisq.test(observed, p=expected)

# since the p-value is <0.05 we reject the Ho. the births of groundhog is not equally distributed over two week sections
#(x2 = 12.64, df =2, p-value= 0.002)
