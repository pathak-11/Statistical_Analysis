### Stats lab 6 

## 1.1 dplyr

install.packages("car")
library(car)
library(dplyr)
data("KosteckiDillon")
migraine <- as_tibble(KosteckiDillon)

head(migraine)
glimpse(migraine)
str(migraine)

# 1.1 filter
migraine[migraine$id==1, ]
subset(migraine, id==1)

filter(migraine, id==1)

# ways to g

# select
migraine[ , c("id", "age","headache","sex")]

# dplyr
select(migraine, id, age,headache, sex)

#multiple contigous columns, use the colon
select(migraine, id:age)

select(migraine, contains("id"))

select(migraine, starts_with("a"))

# conditions can be combined
select(migraine, starts_with("a"), medication:sex)
?select

# 1.1.3
#arrange

#base R
migraine[order(migraine$age), ]

# dplyr
arrange(migraine, age)

# sort in reverse order(i.e., descending)
arrange(migraine, desc(age))

# exercise 1
?arrange

arrange(migraine,age, airq)

# 1.1.4 mutate - create a new column into the DF

migraine$airqKP <- migraine$airq/3

migraine$temp <- 0

# dplyr way
migraine <- mutate(migraine, airqKP2 = airq/3)

migraine <- mutate(migraine, airKP3 = airq/3,
                   id.new = id + 1000)

# summarise functions

# mean age of each sex
Male <- subset(migraine, sex =="male")
Female <- subset(migraine, sex == "female")

mean(Male$age)
mean(Female$age)

# dplyr
summarise(group_by(migraine, sex), meanAge = mean(age))

summarise(group_by(migraine, sex), meanAge = mean(age), sdAge = sd(age))


# 1.2 Chaining and the pipe command %>% ( %>% its called pipe)

filter(select(migraine, id, age), age>60)

migraine_60 <- migraine %>%    # select the data frame I want to work with 
  select(id, age)%>% # then select columns of interest
  filter(age>60)   # then filter age> 60


# 1.3 Practice

# 1

#Temptable <- filter(select(migraine, id, age, sex))
#Temptable
Temptable <- migraine%>%
  select(id, age, sex)
Temptable

# 2
##filter(select(migraine, (age<20), (age>60)))
newtable <- migraine %>% filter(age<20| age> 60)
newtable

# 3
newtable1 <- migraine %>%  # Read in data frame of interest 
  group_by(headache) %>% # group by headache (subsetting the data)
  summarise(meanAQ = mean(airq)) # get the mean of airQ
newtable1

# can get the count of entries with 

migraine %>%
  group_by(id) %>%
  summarise(Numberofpobservations = n())

newtable2 <- migraine %>%  # select our data migraine
  group_by(id) %>%  # grouped by ID
  filter(headache == "yes") %>%  # filter to keep headache == "yes"
  summarise(NumberofHeadaches = n())  # get the count
newtable2


# because we need the hflights package
install.packages("hflights")

library(hflights)
data("hflights")

?hflights

##airline <- as_tibble(hflights)
##airline

# 1 
KP <- hflights %>% # selects the hflight data
  group_by(UniqueCarrier) %>% # and then groups ( or subsets) by unique
  filter(Cancelled == 1) %>% # then filter to keep cancelled ==1
  summarise(NumberofCancelled = n()) # and then get the number of calcelled flight
KP
# carrier XE had the most

# 2 
Kp2 <- hflights %>% # get the DF of interest
  group_by(DayOfWeek) %>% # then group by day of week
  summarise(NumberofFLight = n()) # then get the counts
Kp2
# Day 6 had the lowest flights

# 3
Kp3 <- hflights %>% # select our data of interest
  group_by(UniqueCarrier) %>% # then group by carrier
  summarise(meanDistance = mean(Distance), # calculate the mean
            minDistance = min(Distance), # calculate the min
            maxDistance = max(Distance)) # calculate the max
Kp3


# Bonus Graph

library(ggplot2)
?geom_errorbar

ggplot(Kp3 , aes(x = UniqueCarrier, y = meanDistance, 
                 ymin = minDistance, ymax = maxDistance))+
  geom_point() + # add data point
  geom_errorbar()+ # adds the error bars
  xlab("Unique Carrier")+
  ylab("Distance")
