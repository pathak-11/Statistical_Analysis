### basic maths / mean, median. mode

# Creating object

yeast <- c(0.86, 1.02, 1.02, 1.01, 1.02, 1, 0.99, 1.01, 0.91, 0.83, 1.01)

# Calculating mean growth of sample
mean <- mean(yeast)
mean
#rounding the value
round(mean, 3)

median <- median(yeast)
median



variance <- var(yeast)
variance
round(variance, 3)

SD <- sd(yeast)
SD
round(SD, 3)

#for the total number of sample
n <- length(yeast)
n

SE <- SD/sqrt(n)
SE
round(SE, 3)

#95% CI and upper and lower limits
lowerlimit <- mean - 2*SE
lowerlimit
round(lowerlimit, 3)

upperlimit <- mean + 2*SE
upperlimit
round(upperlimit, 3)

#creating the graph of the data
hist(yeast,
          main = "Growth Rate of Yeast",
          xlab = "Growth rate (no. of cells/hour",
          col = "pink")

