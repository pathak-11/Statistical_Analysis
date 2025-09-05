# Stats Lab 1
# Comments are here to help me remember things.

#Examples of calculator use

7+5
7-5

0/0 #not defined (Not a number, Nan)

#Order of operations is the same as a calculator
10+6/2 #13

(10+6)/2 #8

#Objects where we assign a values to 

#created object x at value 12
x<- 7+5 

#created object y at value 17
y<- x+5

#we can use objects in calculations
x*y #204

x+2*y #46

#can run the object by itself to remind me what the value is 

#how to get a list of our objects made
#examples of ways to get the name of our objects

ls()
objects()

#if I want to remove x object

rm(x) #removes the x object when I run this code

# can remove objects with the broom on the right corner of the environment

#Exercise 1
?ls()

v1 <- 23
v2 <- 15
v3 <- 11

#A 
v1*v2+v3 #356

#B 
v1+v2*v3 #188

#C 
v1*(v2+v3) #598 - largest value

#D 
(v1+v2)*v3 #418

#math functions
#details in lab manual page 7

#Example 2

log(x)+exp(y)

#A  - 24.69
x<-100
y<-3

#B #can replace x and y manually in the equation 
#we dont have to use the objects themselves
#gave me 59.6
log(150)+exp(4)

#can reassign x and y values or manually input them
# c: 14.29
log(1000)+exp(2)
#d: 154.63
log(500)+exp(5)

#Exercise 3

#pi is already defined in R
#function for height is 
20*tan(35*pi/180) #14 #C is the answer

# installing and loading packages

#this code gives us all of the packages we have in R
library()
# when we run it, it opens a tab with R packages available 
# can get the same information by clicking on packages tab in bottom right box

#can install packages by going to Tools -> to install packages 
#install "car" and "dplyr" package with the dependencies

#to use a library we have to run the "library" code
library(car)
# can alternatively check the box in the packages tab in the box

#how to get help in R
#add a ? in infront of the function to get information about it
?log()

?round
#from the ? round we used round(number, number of decimals)
round(5.612,1)

#signif() is another way to round
signif(5.612, digits = 2)

testCase<- data.frame(expand.grid(c (1:4), c(1:4)))
testCase$ mean<- rowMeans(testCase)
testCase
