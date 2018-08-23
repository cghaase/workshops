#######################################################
# INTRODUCTION TO R FOR ECOLOGISTS                    #
# C.G. Haase                                          #
# cghaase@gmail.com                                   #
# [some code borrowed from R. Fletcher]               #
#######################################################

###########################################
#IMPORTING AND EXPORTING DATA IN R
###########################################

#Set working directory manually
setwd("C:\\Users\\cghaase\\Desktop\\Rworkshop")  #the path will change for your work

#Set working directory with RStudio
#>Tools>Set working directory>Choose directory

#Import text files
data <-read.table("data.txt",header=TRUE)

#Lets you choose file from within working directory
#data <-read.table(file.choose(),header=TRUE)

#Import CSV files
#data.csv<-read.csv("data.csv",header=T)

#Play with data set
data
head(data)
dim(data)
names(data)
attach(data)
data$SurveyID
class(data$SurveyID)

#Create vector
surveyID <- data$SurveyID
surveyID

###########################################
#SUMMARIZING DATA
###########################################
#Some common functions summarizing data
sum(data$Presence)
?sum
??sum

median(data$Presence)

mean(data$Presence)

sum(data$Presence)/length(data$Presence)

sd(data$Presence)

max(data$Presence)

min(data$Presence)

length(data$Presence)

quantile(data$Presence, prob=0.95)

#This function calculates common summary statistics for each column 
#great for outliers or missing values (N/As)
summary(data)

#tapply: useful when you need to summarize data based on groups or treatments
#tapply(X=summary variable, INDEX=grouping variable, FUN=function)

#Calculate mean of elevation when water is present/not present
tapply(X = data$Elevation, INDEX = data$Water, FUN = mean)
tapply(data$Elevation, data$Water, mean)


###########################################
#CONDITIONAL EXECUTIONS
###########################################

##Comparison operators:
#Equal: ==
#Not equal: !=
#Greater than: > 
#Less than: <
#Greater than or equal: >= 
#Less than or equal: <=

##Logical operators:
#And: &
#Or: |
#Not: !

#When object is number, use number alone, when object is character (letters), must use ""

##If statements: if(condition = true) {return value 1} else {return value 2}
a <- 1
b <- 5
if(a==0){print(1)} else {print(2)}
if(a!=0){print(1)} else {print(2)}
if(a>=0){print(1)} else {print(2)}
if(a==0 & b==5){print(1)} else {print(2)}
if(a==0 | b==5){print(1)} else {print(2)}

#IfElse statements: ifelse(condition, condition=true, condition=false)
ifelse(a>=0, "yay", "boo")
ifelse(a<=0 & b!=4, "yay", "boo")
ifelse(a<=0 | b!=4, "yay", "boo")


###########################################
#SUBSETTING DATA
###########################################
#Accessing column 1 of data
#subscript: [Row, Column] USE BRACKETS
SurveyID<-data[,1]

#Accessing row 1 of data
data[1,]
data[1,1]

#Accessing a subset of data: observations of density > 0
#Grabs subset of data with nonzero values
data[data$Presence>0,]
data$Presence[data$Presence]

#Accessing a subset of data: a portion of one variable based on another
#Grabs all data with elevation > 1.001m
data$Presence[data$Elevation>1.001]
data$Presence[!data$Elevation<1.001]

#Grabs all data points with elevation > 1.001m and presence of water
data$Presence[data$Elevation>1.001 & data$Water==1]

#OR use "subset" function
#subset(data, condition, select=data you want to select)
subset(data, data$Elevation>1.001)
subset(data, data$Elevation>1.001, select=PointID)
subset(data, data$Elevation>1.001, select=c(SurveyID,PointID))
dummy <- subset(data, data$Elevation>1.001, select=c(SurveyID,PointID))
dummy


###########################################
#LOOPS
###########################################

##FOR loops
#for(variable in sequence){statements}

#for(random variable "in" start:end){statements}
#for(i in 1:length(data$SurveyID)){statements}
#for(i in 1:dim(data)[1]){statements}

#set dummy variable
c <- 0
for(i in 1:30){
x <- c + i
print(x)
}

#Change sex from Male and Female to M and F
for(i in 1:dim(data)[1]){
	data$Sex.2[i] <- ifelse(data$Sex[i] == "Male", 0, 1) #Make a new column with binary inputs
}

data$Sex.2 <- as.factor(data$Sex.2)

##WHILE loops
#while (condition) {statements}
z <- 0
while(z < 5) { 
    z <- z + 2
    print(z)  
}


###########################################
#CREATING YOUR OWN FUNCTIONS
###########################################

#my.function.name <- function(var1, var2){function body: return()}

#example: calculate mean
arith.mean<-function(x){sum(x)/length(x)}
z<- c(1:50)
arith.mean(z)
sum(z)/length(z)

y <- c(4:27)
arith.mean(y)

ex.funct <- function(x, y){
	z1 <- x/y
	z2 <- y^3
	z  = c(z1, z2)
	return(z)
}

#Returns the function itself
ex.funct

#Apply function
ex.funct(x=2, y=5)
ex.funct(2, 5)

#Example applying function within a FOR loop
ex.funct2 <- function(x){
		y <- x^2
		return(y) 
		}
a <- 1
for(i in 1:10){
	b <- a + i
	output <- ex.funct2(b)
	print(output)
}

#STOP and WARNING
myfct <- function(x1) {
        if (x1>=0) print(x1) else stop("This function did not finish, because x1 < 0")
        warning("Value needs to be > 0")
}
myfct(x1=2)
myfct(x1=-2)


###########################################
#BASIC PLOTTING
###########################################
?par #gives you help file of graphical parameters
window()
window=F ##For PC

quartz=T # For mac
quartz=F
##SCATTERPLOT
plot(data$Easting, data$Northing)
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)")

#Other alternative ways
plot(x = data$Easting, y = data$Northing)
plot(Northing~Easting, data = data)

#You can try different symbol shapes by varying the "pch" argument
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16)
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=23)
?plot
#Change shape colors
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",col="blue")

#Change shape colors by presence/absence 
data$Color[data$Presence==0]="blue" 
data$Color[data$Presence==1]="red" 
head(data)
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16, col=data$Color)

#Add a legend
legend("topright", c("Absence", "Presence"), pch=c(16,16), 
       col=c("blue", "red")) 
?legend
# This tells R to put the legend in the top left of the 
# graph, then in the legend make labels for Absence/Presence,
# make the symbols and colours next to these labels 
# the same as what we specified for the graph 

#Add a legend wherever you click on the figure (the number within the "locator()" command signifies the number of click)
legend(locator(1), c("Absence", "Presence"), pch=c(16,16), 
       col=c("blue", "red")) 

#Change size of symbols
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16,cex=2)
#http://www.phaget4.org/R/Rplotsymbols.png

#Change labels size, note that instead of "cex" we are using "cex.lab"
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16,cex.lab=2)

#Change size of axes values.
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16,cex.axis=2)

#Change color of axes labels
colors() #gives you list of possible colors
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16,col.lab="red")

#Change length of ticks
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16,tcl=1) #default is -0.05.  Sign means "inside"(+) or "outside"(-)

#Font type (regular=1, bold=2, itallic = 3, bold/itallic=4)
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16,font.lab=2)

#Remove axes and add box (good when creating maps)
plot(data$Easting, data$Northing,axes=FALSE, xlab="",ylab="",pch=16, col=data$Color)
box()
axis(1)
axis(2)
axis(3)
#Save figure as pdf
pdf("Figure1a.pdf")
plot(data$Easting, data$Northing,xlab="Easting (UTMs)",ylab="Northing (UTMs)",pch=16, col=data$Color)
dev.off()
#can save same way as jpeg, sub .jpeg for .pdf

##HISTOGRAMS
hist(data$Elevation,xlab="Elevation (m)",main="")

#Color can be changed
hist(data$Elevation,xlab="Elevation (m)",main="",col="black")

#The number of bins can be also changed
hist(data$Elevation,xlab="Elevation (m)",main="",breaks=3)

##BOXPLOTS
#Create 2 different objects
pres<- data$Elevation[data$Presence==1]
abs <- data$Elevation[data$Presence==0]

#Display both graphs in the same figure
graph <- boxplot(pres,abs, xlab="Unicorns", ylab="Elevation")
graph < -boxplot(pres,abs, xlab="Unicorns", ylab="Elevation", ylim=c(0,5))
axis(1, at=1:2, labels=c("Presence", "Absence"))

#Adding color from 2:4 because 1 is black
boxplot(data$Elevation,col=2)

#Remove outliers
boxplot(data$Elevation,col=2,outline=FALSE)

#Add labels
boxplot(data$Elevation,col=2,outline=FALSE,ylab="Elevation",xlab="Data Example")
axis(1,at=1,labels="example2")

##MULTIPLE FIGURES
par(mfrow=c(2,1))#Note that the notation is row,column, so in this case is 2 rows, 1 column
boxplot(data$Elevation,col=2,outline=FALSE,ylab="Elevation",xlab="Data Example")
hist(data$Elevation,xlab="Elevation (m)",main="")
#dev.off() #turn figures off

###########################################
#BASIC STATISTICS
###########################################

##Visually assess data
#Create histogram to assess distribution
hist(data$Elevation)

#Create a qqplot to assess normality
qqnorm(data$Elevation)

##Statistically test normality with a Shapiro test
shapiro.test(data$Elevation)

##Look at elevation where animals were present
hist(data$Elevation[data$Presence==1])
##Or absent
hist(data$Elevation[data$Presence==0])

#Run a t-test to see if elevation affects presence--must install the 
#"car" package first
install.packages("car")

##After you install a package you need to access its library using the
##library() function or by checking the box next to the package in your
##packages window
library(car)

#Change presence to a factor with 2 levels
Presence<-as.factor(data$Presence)

#See what format this variable is in
class(Presence)

##Run t-test
#Most statistical functions relating variables in R are Y~X
t.test(Elevation~Presence,data=data,var.equal=T)

##What if we wanted to see if elevation varied by state? 
#Because there are 3 levels to this factor, we'll need to run an ANOVA 
#First create a linear model
state.lm <- lm(Elevation~State,data=data)

#Run the ANOVA on the linear model
anova(state.lm)

##Let's look at a general linear hypothesis 
install.packages("multcomp")
library(multcomp)

state.mc <- glht(state.lm,mcp(State="Tukey"))
summary(state.mc)
