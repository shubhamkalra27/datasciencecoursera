# Topic: R Sessions - MSU
# Purpose: Introduce basic concepts of R

#                               Session 1


# Set working directory
setwd("D:/MSU/R/Working")

# Get the location of the working directory
getwd()

# Read a data file into R
# Read a csv file
trans <- read.csv("TransactionMaster.csv")
cust <- read.csv("CustomerMaster.csv")

# Read files using the read.table command
trans_1 <- read.table("TransactionMaster.csv", header=FALSE, sep=",",
                      stringsAsFactors=FALSE, na.strings = TRUE)

# Create different types of data 
# vectors/array
a <- c(1,3,4,6)
a
b <- c("LedZep","Floyd","Who?","Doors")
b

1:16
# Matrix
mat1 <- matrix(c(1:16), ncol=4, byrow=TRUE, 
               dimnames = list(c("row1", "row2","row3","row4"),
               b))
mat1

# list
list1 <- list(a,mat1)
list1

# dataframe
data1 <- data.frame(a,b)
data2 <- data.frame(BandName = b, Rank=a)

data1
data2

# Get basic statistics about the data
summary(trans)
head(trans)
str(trans)
colnames(cust)

# Accessing elements from R objects
a
a[3] 
list1[1]

# Access Columns
data2[2]
data2[,2] # What's the difference in these 2 methods?

cust["Branch_Num"]

trans$System_Period

# Access rows
data2[2,]

trans[1223,]

# Subset based on rows and columns
cust[c(1,2,3,4),c(3,4,5)]



#                             Session 2


# Basic functions in R

# Dimensions of dataframes
nrow(trans)
ncol(cust)

# Identifying unique entries
unique(trans$Branch_Number)

# Counting the number of entries
length(trans$Invoice_Date)
length(unique(trans$Branch_Number))

# 'Which' function
which(cust$City == 'ATLANTA')
which(cust$City == 'NEW YORK')

# Subset the dataset based on the 'which' function
Atlanta_cust <- cust[which(cust$City == 'ATLANTA'),]

# Subset function
Atlanta_cust_1 <- subset(cust, cust$City == 'ATLANTA')


# Text manipulation functions
# Find function
grep("CARTUM", cust$Customer_Name)
which(cust$Customer_Name == 'CARTUM') # Why we use grep when 'which' function is available

cartum_cust <- cust[grep("CARTUM", cust$Customer_Name),]

# Find and Replace
gsub("-","",cust$Phone_Number)
cust$Phone_Number <- gsub("-","",cust$Phone_Number)
# Check out regexpr function

# Concatenation function
cust$Full_Name <- paste(cust$Contact_Name_First,
                        cust$Contact_Name_Last,sep=" ")

# Sorting data
order(a)
order(-trans$Sales_Amount)
trans_sorted <- trans[order(-trans$Sales_Amount),]

# Merging data
# Inner Join
inner <- merge(trans,cust,by.x="Customer_Number", by.y="Customer_Number", all=FALSE)

# Outer Join
outer <- merge(trans,cust,by.x="Customer_Number", by.y="Customer_Number", all=TRUE)

# Left Join
left <- merge(trans,cust,by.x="Customer_Number", by.y="Customer_Number", all.x=TRUE)

# Right Join
right <- merge(trans,cust,by.x="Customer_Number", by.y="Customer_Number", all.y=TRUE)



#                           Session 3


# Reading Date formats: 
# Reference: http://www.statmethods.net/input/dates.html
inv_date <- as.Date(trans$Invoice_Date, format = c("%d-%b-%y"))
inv_date

# Test on real data
trans$Invoice_Date <- as.Date(trans$Invoice_Date, format = c("%d-%b-%y"))
trans$Service_Date <- as.Date(trans$Service_Date, format = c("%d-%b-%y"))

# Get the system time
Sys.time()
which(trans$Invoice_Date < trans$Service_Date)

# Read up on as.POSIXct and as.POSIXlt. When and why are they useful??

# Extract time related values
format(trans$Invoice_Date, "%d")
max(format(trans$Invoice_Date, "%y"))
max(format(trans$Service_Date, "%Y"))

trans$Service_Date

max(trans$Service_Date)

# Subset based on year
trans_sub <- subset(trans, as.numeric(format(trans$Invoice_Date,"%m")) %in% c(1,2,3))

# Aggregate function
trans_agg <- aggregate(trans$Sales_Amount, by=list(trans$Branch_Number), FUN=sum)

trans_agg <- aggregate(trans["Sales_Amount"], by=list(trans[,"Branch_Number"]), FUN=sum)


trans_agg_1 <- aggregate(trans$Sales_Amount ~ trans$Branch_Number, FUN=max)
trans_agg_2 <- aggregate(trans$Sales_Amount ~ trans$Branch_Number + trans$Product_Number, FUN = sum)

# Let's say i want to do 'Max - Min' when aggregating, how do i do that?

# Flow Control

x <- runif(100,min=100, max=10000)

measure <- "max"

if(measure == "median" ) {
  print(median(x))
} else if (measure == "mean") {
  print(mean(x))
} else {
  print("Wrong Input")
}


## Include apply family

# SQLDF in R

install.packages("sqldf")
library(sqldf)

df <- sqldf("select distinct Product_Number from trans")

sqldf("select distinct Customer_Number as Customers 
      from cust where State = 'FL'")

leftjoin <- sqldf("select 
  			a.*, 
				b.* 
			from cust as a 
				left join trans as b 
				on a.Customer_Number = b.Customer_Number")

innerjoin <- sqldf("select 
				a.Customer_Number,
        a.City,
        b.Sales_Amount
			from cust as a 
				inner join trans as b 
				on a.Customer_Number = b.Customer_Number")

SalesByCustomer <- sqldf("select 
				Customer_Number as Customer, 
				sum(Sales_Amount) as Total_Sales
			from innerjoin 
      group by Customer_Number")



#                         Session 4

# Custom Functions in R

oddcount <- function(x) {
  k <- 0  ## Assign the value 0 to k
  for (n in x) {  ## Start a FOR loop for every element in x
    
    if (n %% 2 == 1) k <- k + 1  ## %% is a modulo operator
    }
  return(k)
}


oddcount(c(1,2,3,5,7,9,14))


# For-Loops
for ( i in 1:5) {
  print(i)
}

# While-Loop
i <- 0
while (i < 10) {
  print(i)
  i <- i + 1
}


# Basic plots and charts in R
# Good reference site: http://www.harding.edu/fmccown/r/

# Read in world bank dataset
world <- read.csv("worldbank.csv")
summary(world)

# Subset the dataset
attach(world)

#  Plotting
# Histogram
hist(life_expectancy)

hist(infant_mortality_rate , breaks=10,
     main = "Infant Mortality rate", xlab = "Infant MR")

plot(density(infant_mortality_rate))



# Scatterplots
# Test the hypothesis - 'Higher the life expectancy, lower the infant mortality'
plot(life_expectancy, infant_mortality_rate)

plot(life_expectancy, infant_mortality_rate, main = "Hypothesis Test",
     xlab = "Life Expectancy", ylab = "Infant mortality rate", 
     col = "blue", pch = 20)

pairs(~life_expectancy + infant_mortality_rate + birth_rate )
pairs(world)
# Look at 'Pairs' function

# Sample Bar plot
# Subset the data
world_subset <- subset(world, country_name %in% c("Australia", "India", 
                                                  "Mexico", "Bulgaria",
                                                  "Finland", "Uruguay"))

detach(world)

# Bar and Pie charts
attach(world_subset)

barplot(energy_use_percapita, main = "Energy per capita", 
        xlab = "Country", ylab = "Consumption",names.arg= country_name)

barplot(energy_use_percapita, main = "Energy per capita", 
        xlab = "Country", ylab = "Consumption",
        col=rainbow(length(country_name)), legend = country_name)

# Sample Pie plot
pie(x=fertility_rate, col = rainbow(length(country_name)), 
    label = paste(country_name, fertility_rate, sep = "-"), 
    main = "Fertility rate")

detach(world_subset)



#                       Session 5
# Misc operations and Mathematical functions

# Missing values
is.na(world)

which(is.na(world))

# What's the difference between NA and Null??
world_2 <- na.omit(world)
# Quick test 1: Create a dataset removing all NA's
world_subset_2 <- world[which(!is.na(world$energy_use_percapita)),]
world <- world[-1,]
# Quick test 2: Calculate the % of NA values in each column in the dataset 'world'
per <- function(x)
{
  k <- (length(which(is.na(x)))*100/length(x))
  k <- round(k, digits = 2)
  k <- paste(k,"%", sep = "")
}

out <- sapply(world[,2:9], per)
out

# Tables
table(as.factor(cust$City), as.factor(cust$Customer_Number))

# Correlations
attach(world)
cor(energy_use_percapita, gni_per_capita)

cor(world$energy_use_percapita, world$gni_per_capita,use="pairwise.complete",
    method = "spearman")

# Quantile subsets
quantile(energy_use_percapita, probs = c(0.05,0.95), na.rm=T)

# Small mathematics
# mean
mean(birth_rate)

# standard deviation
sd(birth_rate)

# Sampling
# From normal distribution
rnorm(10, mean = 10, sd = 22)


# From t-distribution
rt(100, df=2,ncp=23)

set.seed(2123)
rnorm(10, mean = 10, sd = 22)

# Create your own dataset with your employee number
set.seed(3547)
c1 <- c("India","Pakistan","Sri Lanka", "Bangladesh") 
c2 <- rnorm(4, mean = 200, sd = 50)
c3 <- rnorm(4, mean = 5, sd = 2)
c4 <- rbeta(4, 1,2)

asia <- data.frame(Country = c1, Avg_team_score = c2,
                   Avg_team_Wickets = c3, Stat = c4)

asia

a <- na.omit(world[2:6])
b <- cor(a[1],a[2:5])
max(b)
colnames(b)[order(-b)[which(b[1,] %in% b[1,order(-b)[1:4]])]]

order(-b)[1:2]
which(b = max(b))



