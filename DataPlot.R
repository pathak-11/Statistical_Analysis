#### data plotting in R

library(ggplot2)

#see some example base R plots Enter run two times for this demo(graphics) to run
demo(graphics)

# 1.2 Creating and customizing graphs in base graphics
#load data that is already in R

#iris is loading our data from the R

#create the data frame iris
data(iris)
head(iris)

# Scatter plot example with base R
# to see the column name of data frame
colnames(iris)

# plot (Y ~ X, data = data.frame)
plot(Sepal.Length ~ Petal.Length, data = iris)
plot(y = iris$Sepal.Length, x = iris$Petal.Length)

# need to reorder the species to make sure that species matches
colours<- c("black", "red", "blue")
color.vector<-rep(colours,each=50) # makes a vector of colors


#add levels
plot(y = iris$Sepal.Length, #the y variable
     x = iris$Petal.Length, # the x variable
     col = c(color.vector),  #adds color
     pch = 19,   #pch is the type of dot # 1 is small dot, 2 is triangle, 3 is +
     xlab = "Petal Length",  #edits the x label
     ylab = "Sepal Length") #edits the y label

legend("topleft",   # this creates a legend on the top left
       pch = 19,  #set the point type
       col = c("black", "red", "blue"), #colors
       legend = unique(iris$Species), #specifies the legend entries
       title = "Iris Species") #title of the legend
     
# strip chart example
stripchart(iris$Petal.Length ~ iris$Species, # makes the strip 
           pch = 3, # turns the points into +
           ylab = "Petal length", # change our y label
           xlab = "Species", #changes our c label
           method = "jitter", # this spreads the points apart
           vertical = TRUE) # make it vertical

# example with dplyr = a way to get summary data
library(dplyr)


#on page 9 ## summarise 

statValues <- iris%>% #take the iris data and then group the data by species
  group_by(Species) %>%
  summarise(mean = mean(Petal.Length), # get the mean
            sd = sd(Petal.Length), #get the SD
            sem = sd(Petal.Length)/ sqrt(length(Petal.Length))) #get the 

# add the mean into our strip chart
points(x = statValues$Species,
       y = statValues$mean,
       col = "red",
       pch = 19,
       cex = 1) #cex changes the size of the point

#code on page 9
# Add the lines for the SE of the graph
segments(x0 = c(1,2,3),  # create the bottom EB (error bars)
         y0=statValues$mean-statValues$sem,
         x1 =c(1,2,3), 
         y1=statValues$mean+statValues$sem,
         lwd = 2, col = "green")  #lwd specifies the line type and the line width

# 1.3 ggplot
#another way to graph data

library(ggplot2)

#code to make the graph page 10

ggplot(data = iris,  # define the data #aes is for aesthetics
       aes (x = Petal.Length, 
            y = Sepal.Length,
            shape = Species,
            color = Species))+  #define our x and y
  geom_point() +  #plot with points
  xlab("Petal Length")+ #edits the x label
  ylab("Sepal Length")+ #edits the y label
  theme_bw() #changes the background to white and black


# in class exercise 
ggplot(data = iris,
       aes(x = Species, y = Petal.Length))+
  geom_boxplot() 

pdf("C:\Users\user-1\Documents\Stat_PBio/Kamana.pdf", width = 7, height = 4, units = "in")
plot(Sepal.Length~Petal.Length, data =iris)
dev.off()

# example of ggsave
 ggsave("KamanaTest2.jpeg",
        )
#graph practice
 library(readr)
#load the sparrow data
 
 Sparrows <- read_csv("Sparrows.csv")
 colnames(Sparrows)
 ggplot(Sparrows,
        aes(x = Head, y = Culmen))+
   geom_point()+
   theme_bw()

 #Creating ggplot for head size and weight
 
 colnames(Sparrows)
 ggplot(Sparrows,
        aes(x = Head, y = Wt))+
   geom_point()+
   theme_bw()
 
# creating ggplot for sex and headsize #for both numeric data try making dotplot
 #box plot - integer and character variable
 # only two numerics dotplot
 
 colnames(Sparrows)
 ggplot(Sparrows,
        aes (x = Sex, y = Head))+
   geom_point()+
   theme_bw()
 
 colnames(Sparrows)
 ggplot(Sparrows,
        aes(x = Sex, y= Head,
            colour = Sex))+
   geom_boxplot()+
   theme_bw()

#histogram 
hist(Sparrows$Wingcrd)

ggplot(Sparrows,
       aes(x = Sex, y = Culmen, fill = Species))+
  geom_boxplot()+
  theme_bw()+
  xlab("Sex")

#vegetation
Vegetation <- read_csv("Vegetation.csv")
ggplot(Vegetation,
       aes(x = SpeciesRichness, y = BARESOIL))+
  geom_boxplot()+
  theme_classic()
 

