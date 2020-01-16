#########################################################################################################################################
## Objective: Data Exploration and Visualization and Feature Engineering                                                                #
## Data source: iris, mtcars, diamonds, titanic dataset in R                                                                            #
## Please install "lattice" package: install.packages("lattice") for trellis graphics                                                   #
## Please install "ggplot2" package: install.packages("ggplot2") for Data Visualization                                                 #
## Please install "GGally" package: install.packages("GGally") for Enhanced Scatter Plot Matrices                                       #
#########################################################################################################################################

library(lattice)

data(iris)
head(iris)

# Core boxplot
boxplot(Sepal.Length ~ Species, data=iris)

# Colored Box Plots with Notches
boxplot(Sepal.Length ~ Species, data=iris, 
    main="Sepal Length for Various Species", xlab="Species",  ylab="Sepal Length", 
    notch=TRUE, col=c("blue","green","red")
)
boxplot(Petal.Length ~ Species, data=iris, 
    main="Petal Length for Various Species", xlab="Species",  ylab="Sepal Length", 
    notch=TRUE, col=c("blue","green","red")
)

# Saving Plots. Can also use the "Plot" window in R Studio
# Saves to current working directory (getwd()) by default
pdf("myplot.pdf")

boxplot(Sepal.Length ~ Species, data=iris)

dev.off() # Clears canvas and makes room for future visualiazations

# Core Pie Chart
pie(table(iris$Species))

# Core Graphics
plot(iris$Sepal.Length,
     iris$Sepal.Width, 
     xlab="Sepal Length", 
     ylab="Sepal Width"
)

# Core Graphics
plot(Sepal.Length ~ Sepal.Width, data=iris)

# Lattice Graphics
xyplot(
  Sepal.Width ~
  Sepal.Length, 
  data=iris,
  groups=Species,
  auto.key=TRUE
)

# Exercise 1:
# Make a 2-D scatter plot of Petal Width versus Petal Length  
# Petal Length vs Petal Width using core. 
plot(Petal.Length ~ Petal.Width, data=iris)

# Plot Petal Length versus Petal Width
# Add a regression line
plot(Petal.Length ~ Petal.Width, data=iris, main="Petal Width vs Length")
abline(lm(Petal.Length ~ Petal.Width, data=iris), col="red", lwd=2)

#Find correlation between variables
#Values close to 1 or -1 depict strong linear resltionship
cor(iris$Petal.Width,iris$Petal.Length)
# [1] 0.9628654

# Create the same plot but using Lattice xyplot
# Provides more information about species
xyplot(Petal.Length ~ Petal.Width, data=iris, groups=Species)

# Lattice Graphics
xyplot(
  Petal.Width ~
  Petal.Length, 
  data=iris,
  groups=Species,
  auto.key=TRUE
)

# Lattice Histogram 
histogram(iris$Petal.Length, breaks=10, type="count", main="Histogram")

# Lattice Density
densityplot(
  iris$Petal.Length,
  main="Kernel Density of Petal Length", 
  type="percent", 
)

# Lattice Density Plot
densityplot(iris$Petal.Length)
# To remove points, add plot.points=F argument
densityplot(iris$Petal.Length,plot.points=F)


# Lattice: Multiple Density Plots
densityplot(~ Petal.Width, data=iris, groups=Species, 
    xlab=list(label="Kernel Density of Petal Width", fontsize=20), ylab="", 
    main=list(label="Density of Petal Width by Species", fontsize=24), 
    auto.key=list(corner=c(0,0), x=0.4, y=0.8, cex=2), scales=list(cex=1.5)
) # cex defines a scale multiplier for text


# Scatterplot matrix
pairs(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris, 
    main="Simple Scatter Matrix"
)

# Getting settings for legend
super.sym <- trellis.par.get("superpose.symbol")

splom(iris[1:4], groups=iris$Species, panel=panel.superpose,
    key=list(title="Three Flower Types", columns=3, 
        points=list(pch=super.sym$pch[1:3],  col=super.sym$col[1:3]),
        text=list(c("Setosa","Versicolor","Verginica"))
    )
)

# Enhanced Scatter Plot Matrices
library(GGally)
ggpairs(iris, ggplot2::aes(color=Species))


# Load the mtcars dataset. If the goal is to predict the MPG column based on the
# other columns, create 2 different plots which illustrate useful relationships
# in the data.


# plot 1
densityplot(
  ~ mpg, 
  data=mtcars, 
  groups=cyl, 
  plot.points=F, 
  auto.key=list(columns=3, title="Cylinders")
)

# plot 2
plot(mpg ~ disp, data=mtcars)
abline(lm(mpg ~ disp, data=mtcars), col="red")


# Find correlation between variables 
cor(mtcars$mpg,mtcars$disp) 
# [1] -0.8475514


## ggplot2 introduction ##
library(ggplot2)

data(diamonds)

# Basic plot types
ggplot(diamonds, aes(x=carat)) + geom_histogram()

ggplot(diamonds) + geom_density(aes(x=carat), fill="gray50")

ggplot(diamonds, aes(x=carat, y=price)) + geom_point()

# ggplot object
# Store the plot for future modification
g <- ggplot(diamonds, aes(x=carat, y=price))
# Second aesthetic adds settings specific to geom_point layer
g + geom_point(aes(color=color))
g + geom_point(aes(color=clarity))

# Segment by factor
g + geom_point(aes(color=color)) + facet_wrap(~ color)
g + geom_point(aes(color=clarity)) + facet_wrap(~ color)

g + geom_point(aes(color=color)) + facet_wrap(cut ~ clarity)
g + geom_point(aes(color=clarity)) + facet_wrap(cut ~ clarity)


##===========================================================================##

## Extended Titanic Exploration ##
# Set your working directory to the bootcamp root folder using setwd()
# or this line won't work
titanic <- read.csv("C:/Users/muckam/Desktop/DataScienceBootcamp/Datasets/titanic.csv")

## first few rows of the dataset
head(titanic) 


## let's look at the features
str(titanic)

# Casting & Readability
titanic$Survived <- as.factor(titanic$Survived)
levels(titanic$Survived) <- c("Dead", "Survived") #need to see structure, order matters
levels(titanic$Embarked) <- c("Unknown", "Cherbourg", "Queenstown", "Southampton")
str(titanic[,c("Embarked","Survived")])



# Is Sex a good predictor?

## Let's load the ggplot2 library first
library(ggplot2)
## plotting the Dead versus Survived   
## Store the plot for future modification
t1 <- ggplot(titanic, aes(x=Survived, fill=Sex)) 

## Use the object and add a bar plot
## The bars for Dead and Surived are further divided by Sex
t1 + geom_bar()   


## Faceting By gender
## We use facet_wrap to stitch two plots 
## facet_Wrap by Sex stitches the individual plots for female and male 
t1 + geom_bar(aes(color= Sex)) + facet_wrap(~ Sex)


## Faceting By embarked
## facet_wrap by Embarked stitches the plots for Cherbourg, Southampton, Queenstown, and Unknown (missing value)  
t1 + geom_bar(aes(color= Sex)) + facet_wrap(~ Embarked)



## Faceting by more than 1 variable
## facet_wrap can take only a single variable
## facet_grid is used to facet by more than one variable simultaneously 
## simultaneous facet by Sex and Embarked 
plt  <- ggplot(titanic, aes(x = Survived, fill = Sex)) + geom_bar()
plt  + facet_grid(Sex ~ Embarked)


# Is Age a good predictor?
## Let's look at the Age distribution by gender 
p <- ggplot(titanic, aes(Age, fill = Sex)) + geom_histogram()
p + labs(x ="Distribution of Age", y="Frequency of Bucket", title = "Distribution of Passenger Ages on Titanic")


## Boxplot of Age
## Plotting Age versus Survived
p <- ggplot(titanic, aes(Survived, Age)) 
p + geom_boxplot() 


## We can get the mean and median age amongst Dead and Survived  
summary(titanic$Age)
summary(titanic[titanic$Survived=="Dead",]$Age)
summary(titanic[titanic$Survived=="Survived",]$Age)


## Let's explore Age further 
## Density plot of Age by Sex 
q1 <- ggplot(titanic, aes(Age, color = Sex)) 
q1 + geom_density() 


## Density plot of Age by Survived
q2 <- ggplot(titanic, aes(Age, color = Survived)) 
q2 + geom_density()


## 1. Create densityplot of "Age" on facet_grid egmented by Survived, Sex, and Embarked
## 2. Create densityplot of "Fare" on facet_grid egmented by Survived, Sex, and Embarked
## 3. Create boxplot of "Age" on facet_grid egmented by Survived, Sex, and Embarked
## 4. Create boxplot of "Fare" on facet_grid egmented by Survived, Sex, and Embarked



## Density plot of Age on facet_grid segmented by Sex and Embarked 
q3 <- ggplot(titanic, aes(x = Age, color = Survived)) + geom_density()
q3 + facet_grid(Sex ~ Embarked)

## Density plot of Fare on facet_grid segmented by Sex and Embarked 
q4 <- ggplot(titanic, aes(x = Fare, color = Survived)) + geom_density()
q4 + facet_grid(Sex ~ Embarked)


## Boxplot of Age on facet_grid segmented by Sex and Embarked 
q5 <- ggplot(titanic, aes(x = Survived, y = Age)) + geom_boxplot()
q5 + facet_grid(Embarked ~ Sex)



## Boxplot of Fare on facet_grid segmented by Sex and Embarked 
q6 <- ggplot(titanic, aes(x = Survived, y = Fare)) + geom_boxplot()
q6 + facet_grid(Embarked ~ Sex)



# Create a new column "Child", and assign each row either "Adult" or "Child"
# based on a consistent metric. Then use ggplot to create a series of box plots
# relating Fare, Child, Sex, and Survived
child <- titanic$Age
child[child < 13] <- 0
child[child >= 13] <- 1
titanic$Child <- as.factor(child)
levels(titanic$Child)
levels(titanic$Child) <- c("Child", "Adult")
g <- ggplot(data=titanic[!is.na(titanic$Child),],
            aes(x=Child, y=Fare))
g.b <- g + geom_boxplot()
g.b + facet_grid(Sex ~ Survived)



