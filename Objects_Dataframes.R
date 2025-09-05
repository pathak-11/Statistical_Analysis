#Lab 2 - Stats objects and Data Frames

#First lets set the working directory to where we want it to be 
# Go to sessions > set working directory > select folder the 
#this code will be different for everyone
setwd("C:/Users/user-1/Documents/Stat_PBio")


#save the file name it whatever you want!
#we will turn this in at the end of lab.

#1.1 Objects
#Lab manual has general guidelines on how to name objects 

c<- 10

c

A10 <- 10
#variable name should not contain space, shouldnt start with number

'A 10' <- 10

?Reserved 
# 1.1 g
#1.2 code

x1 <- 10.5
class(x1) #class - numeric

c1 <- "Hello"
class(c1) #gave class - character

x2 <- 10
class(x2)  #is numeric

#to change x2 into an integer
x2 <- as.integer(x2)
class(x2) # the class in now integer

#works if we want to chance into other class
#if we want it to be a character
as.character()

#Class Exercise 1
x3 <- 22.7
x3
class(x3)
## to change it to an integer 
x3 <- as.integer(x3)
x3
# we get 22 when we change it to an integer

#what happens when we change it to a character
x3 <- as.character(x3)
x3  # we get "22"

#1.3 vectors
?c()
cats <- c(7.5, 10.9,16.0,
          16.1, 10.5, 8.0,10.3,12.2,
          12.6,11.6,10.8,11.9,15.1,9.8,13.0)
cats
sum(cats) #sums up the cat values
length(cats) #give us the number of entries

#to get the mean sum / N
sum(cats)/length(cats)
#can use the mean function to get the mean of the vector
mean(cats)

#page 5 of the manual has examples of predefined functions
sd(cats)
?sd
summary(cats)
?summary(0)

#1.3.1 Graphing a single numeric value

# we can make a histogram with our vector
hist(cats) #will create a histogram in the bottom right pane

#can click on the zoom magnifying glass to open a window 
?hist

# we can modify the hist() function
# help file gives us all of the options that we can use to edit the histogram
hist(cats,
     breaks = 10,
     col= "yellow",
     xlab = "weight of cats (kg)",
     ylab = "count")

# lots of extra things you can do to the hist() function refer to the ?hist() for details

# 1 .3.2 - accessing different parts of a vector

cats
#if we cant the fifth element we do
cats[5]

#exercise 2
# we want just the first 10 of cats
# created a object cats2
# used cats[] to get values of interest
# 1:10 gives us a list of numbers 1,2,3,4,....10

1:10
cats[c(1,2,3,4)]

cats2 <- cats[1:10]
cats2

# 1.3.3 Generating sequences and repeats

1:10 #this makes a list of number 1-10

10:1 #this makes a list of numbers 10-1

#seq(from, to, increment)
seq(1,5,0.5)

#rep(the sequence, number of repeats)
rep(2, 5) #made 2,2,2,2,2
rep(1:5, 2) #made 1,2,3,4,5,1,2,3,4,5
rep(c(1,2,5), 3) # made 1,2,5,1,2,5,1,2,5 # c makes sur sequence


# Exercise 3 - 

OddNumbers <- seq(1,50,2)
# sequence (From 1, to 50, increments at 2)
OddNumbers

OddNumbers <- seq(1,50,3)
# sequence (From 1, to 50, increments at 2, isn't odd number)
OddNumbers

#calculate general stats
mean(OddNumbers)
median(OddNumbers)
range(OddNumbers)
var(OddNumbers)
sd(OddNumbers)

# 1.4 Data Frames

# create some vector example
days <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
birth.boys <- c(20,25,46,34,32,29,27)
birth.girls <- c(34,25,39,28,26,41,20)

#combine the vectors to make a data frame
c.dat <- data.frame (cbind(days, birth.boys, birth.girls))
c.dat #look at it
# can also click on the dataframe  in the top right environment

# check the class of the data
class(c.dat)
str(c.dat)
#note that the data for births in a character

# If we try to get the mean with a character it doesnt work
mean(c.dat$birth.boys)

# we need to change data into numeric
c.dat$birth.boys <- as.numeric(c.dat$birth.boys)
c.dat$birth.girls <- as.numeric(c.dat$birth.girls)

mean(c.dat$birth.boys)

# 1.4.1 ways to access data frame parts

c.dat[1,1]
# if we want column data
c.dat[ , "birth.boys"]

# 1.4.3 Subsets

#ways to manipulate the data frame

# lab manual has many ways to filter data
# example what if we want to keep only births of boys > 30

boys_30 <- subset(c.dat , birth.boys > 30)
boys_30

girls_34 <- subset(c.dat, birth.girls<= 34)
girls_34
GB_34 <- subset(c.dat, birth.boys == 34, birth.girls==34)
GB_34
##Week <- subset(c.dat, days == ,3)

# Exercise - Practice at home - use logical operators to manipulate data frames

# 1.5 Importing and exporting

#to import data
# go to Import Dataset > From Text (readr) > Browse > select data we want to import

# this is the code to import data
bird <- read.csv ("bird.csv")

# write data - which will create another Data set

#write.csv (name of the Data Frame, "What we want to )
write.csv(c.dat , "TestFile.csv")

# Last part of the assignment
#Create Female Bird file and do stats

Female_birds <- subset(bird, Sex == "F")
Female_birds
 write.csv(Female_birds, "BirdFData.csv")
 
mean(Female_birds$Tarsus) #21.03
sd(Female_birds$Tarsus) #0.4
mean(Female_birds$Head) #30.56
sd(Female_birds$Head) #0.25
mean(Female_birds$Weight) #15.3
sd(Female_birds$Weight) #0.4
