#####################################################################################################################
#####################################################################################################################
### INTRODUCTION TO DATA SCIENCE
### Homework 2
#####################################################################################################################
#####################################################################################################################

rm(list=ls())
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2")

#####################################################################################################################
###############################################   Q2    #############################################################
#####################################################################################################################
# This is a simulated social network dataset of 10000 users (think Facebook or Google Plus). There are 10 files in total. 
# You can find them in CourseWorks. These files provide snapshots of the network from Monday (2013-10-01) to Sunday (2013-10-07) 
# in 7 csv files respectively. Each file contains the number of users visiting the social network site, the number of posts, 
# time spent on website, and new friends made on that day (if users didn’t visit the site that day, then there is no record 
# in the corresponding file). There is a csv file about users profile. More details can be found in “README.txt”. 
# There is also a big csv file (a 10000×10000 matrix) representing all the friend links between the 10000 users. 
# If users i and user j are friends, then the (i,j) entry will be 1, otherwise it’s 0. Note that the matrix is symmetric.
#####################################################################################################################
### (a). Join the 7 csv files on the users actions. Note that most users don’t visit the site everyday. 
# You are supposed to calculate the total number of visits, the total number of posts, the total time spent, 
# and the total number of new friends made during this week for each user. You should also extract the total number 
# of friends for each user from the friendship csv file. Add all these statistics into the users’ profile dataset. 
# The profile should be finally be a data frame of 10000 rows and 15 columns.

### Read in the 7 csv files (Monday to Sunday) and check the dimensions and the first few lines
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/social_network_dataset")
Monday <- read.table(file="Monday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Monday) # [1] 4417    6
head(Monday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            1            0  1.2886910          0      1
# 2 2            4            9  2.4038215          1      2
# 3 4            1            1  0.2165628          2      4
Tuesday <- read.table(file="Tuesday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Tuesday) # [1] 4407    6
head(Tuesday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 2            1            4  1.6712005          0      2
# 2 3            4            3  0.8775062          1      3
# 3 4            1            0  4.2689376          3      4
Wednesday <- read.table(file="Wednesday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Wednesday) # [1] 4406    6
head(Wednesday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            4            2  3.4117510          1      1
# 2 3            1            0  0.1541116          1      3
# 3 7            2            4  0.6703984          1      7
Thursday <- read.table(file="Thursday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Thursday) # [1] 4446    6
head(Thursday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            2            0   1.150650          0      1
# 2 3            3            3   1.145542          1      3
# 3 4            3            3   2.119080          0      4
Friday <- read.table(file="Friday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Friday) # [1] 5701    6
head(Friday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            1            0  0.4466331          1      1
# 2 2            4            0  0.8880539          0      2
# 3 3            2            0  1.1681180          1      3
Saturday <- read.table(file="Saturday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Saturday) # [1] 7073    6
head(Saturday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            2            1  7.6082675          0      1
# 2 7            1            1  0.6306993          1      7
# 3 9            7            3  0.8208116          0      9
Sunday <- read.table(file="Sunday.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Sunday) # [1] 7068    6
head(Sunday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            4            2  0.1405583          0      1
# 2 2            1            1  0.4169505          1      2
# 3 3            5            4  5.1679125          0      3

### Stack up one on top of eachother the 7 datasets (Monday to Sunday)
MondayToSunday <- rbind(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
dim(MondayToSunday) # [1] 37518     6
head(MondayToSunday, 3)
#   X number_visit number_posts time_spend new_friend userid
# 1 1            1            0  1.2886910          0      1
# 2 2            4            9  2.4038215          1      2
# 3 4            1            1  0.2165628          2      4
### Double check if the number of rows in our dataset is correct
dim(Monday)[1] + dim(Tuesday)[1] + dim(Wednesday)[1] + dim(Thursday)[1] + dim(Friday)[1] + dim(Saturday)[1] + dim(Sunday)[1]
# [1] 37518 
dim(MondayToSunday)[1]
# [1] 37518 

### Calculate the total number of visits, total number of posts, total time spent, total number of new friends made
# during this week for each user
MondayToSundayAggregate <- aggregate(formula = cbind(number_visit, number_posts, time_spend, new_friend) ~ userid, 
                           data = MondayToSunday, FUN = sum)
dim(MondayToSundayAggregate) # [1] 9950    5
head(MondayToSundayAggregate, 3)
#   userid number_visit number_posts time_spend new_friend
# 1      1           14            5  14.046551          2
# 2      2           10           14   5.380026          2
# 3      3           15           10   8.513190          4


### Read in the profile.csv file and look at the dimensions and the first few lines
Profile <- read.table(file="profile.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Profile) # [1] 10000    11
head(Profile, 3)
#   X age gender relation_status hometown living perference total_action    sign_up last_login userid
# 1 1  23      0               2        6      1          4          253 2013-03-10 2013-10-07      1
# 2 2  50      1               4        4      4          6          119 2013-03-10 2013-10-07      2
# 3 3  49      1               4        2      3          4          230 2013-02-08 2013-10-07      3

### Notes on Profile data
# In the gender column, 0-female, 1-male.
# In the relation_status column, 1-single, 2-in a relationship, 3-engaged, 4-married.
# The living column represents the place users are currently living in.
# For both hometown and living columns, possible values are 1-6 which measures different 
      # levels of locations. Specifically, the smaller the number, more west the location is.
# In the preference column, possible values are 1-6 which measures different 
      # like level for art and science. Specifically, the smaller the number, the more
      # users like science.
# Total_action records all the actions the user made during this week.
# sign_up and last_login mean users' sign up date and the last log in date respectively.

### Merge MondayToSunday to Profile by userid
SocialNetwork <- merge(Profile, MondayToSundayAggregate, by.x="userid", by.y="userid", all=TRUE)
dim(SocialNetwork) # [1] 10000    15
require(gdata)
SocialNetwork <- remove.vars(SocialNetwork, "X")
dim(SocialNetwork) # [1] 10000    14
head(SocialNetwork, 3)
#   userid age gender relation_status hometown living perference total_action    sign_up
# 1      1  23      0               2        6      1          4          253 2013-03-10
# 2      2  50      1               4        4      4          6          119 2013-03-10
# 3      3  49      1               4        2      3          4          230 2013-02-08
# last_login number_visit number_posts time_spend new_friend
# 1 2013-10-07           14            5  14.046551          2
# 2 2013-10-07           10           14   5.380026          2
# 3 2013-10-07           15           10   8.513190          4

### Extract the total number of friends for each user from the Linkage.csv file
Linkage <- read.table(file="Linkage.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
dim(Linkage) # [1] 10000 10001

### Add the total number of friends to the SocialNetwork
SocialNetwork$number_friends <- apply(X=Linkage[,2:10001], MARGIN=1, FUN=sum)
dim(SocialNetwork) # [1] 10000    15
head(SocialNetwork, 3)
#   userid age gender relation_status hometown living perference total_action    sign_up last_login
# 1      1  23      0               2        6      1          4          253 2013-03-10 2013-10-07
# 2      2  50      1               4        4      4          6          119 2013-03-10 2013-10-07
# 3      3  49      1               4        2      3          4          230 2013-02-08 2013-10-07
#   number_visit number_posts time_spend new_friend number_friends
# 1           14            5  14.046551          2             71
# 2           10           14   5.380026          2             68
# 3           15           10   8.513190          4             75

#####################################################################################################################

### (b). Perform a basic exploratory data analysis to better under understand this social network. 
# Here are some questions you should answer/guidelines for exploration:
# (Some reference to help with plotting in R: http://flowingdata.com/category/tutorials/). 
# Keep a log of your exploration and turn it in as part of your assignment.

#####################################################################################################################

### What’s the distribution of the number of friends?
summary(SocialNetwork$number_friends)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    38.0    62.0    71.0   109.2    99.0   511.0 
# the median (71) is smaller than the mean (109) => the distribution is skewed to the right
# the max (511) indicates that we have outliers
# Formula to calculate outliers: Q3 + 1.5 × IQR
Q1 <- as.numeric(summary(SocialNetwork$number_friends)[2])
Q3 <- as.numeric(summary(SocialNetwork$number_friends)[5])
IQR <- Q3 - Q1
sum(as.numeric(SocialNetwork$number_friends > Q3+1.5*IQR))
# [1] 1593 - this is the number of ourliers in number_friends
# Among the outliers the mean number of friends is: 299
mean(SocialNetwork$number_friends[SocialNetwork$number_friends > Q3+1.5*IQR])
# [1] 299.3264

require(ggplot2)
require(ggthemes)
p <- ggplot(data=SocialNetwork, aes(x=number_friends))

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot1a.png", height=600, width=600)
p + geom_histogram(colour = "darkgrey", fill = "lightgrey", binwidth = 10) + 
  ylab("Count") + xlab("Number of Friends") + 
  ggtitle("Distribution of the number of friends") +
  theme_economist()  
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot1b.png", height=600, width=600)
p + geom_density(color="darkgrey", fill="darkgrey") +
  ylab("Density") + xlab("Number of Friends") + 
  ggtitle("Distribution of the number of friends") +
  theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### What’s the relation between the number of friends and age?
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot2.png", height=600, width=600)
p <- ggplot(data=SocialNetwork, aes(y=number_friends, x=age))
p + geom_point() + geom_smooth(method="lm") +
  ylab("Number of Friends") + xlab("Age") + 
  ggtitle("Relation between the number of friends and age") +
  theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### Does age affect whether users move away from their hometown to live another city?

SocialNetwork$moved <- as.numeric(SocialNetwork$hometown != SocialNetwork$living)
require(scales)

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot3a.png", height=600, width=900)
p <- ggplot(data=SocialNetwork, aes(y=moved, x=age, color=factor(moved)))
p + geom_point(position="jitter") +
  ylab("") + xlab("Age") +
  ggtitle("Relation between age and whether the user moved away") +
  labs(color="User Moved from Hometown") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot3b.png", height=600, width=900)
p <- ggplot(data=SocialNetwork, aes(y=moved, x=age, color=factor(moved)))
p + geom_point(position=position_jitter(height=0.1)) + 
  ylab("") + xlab("Age") + 
  ggtitle("Relation between age and whether the user moved away") +
  labs(color="User Moved from Hometown") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot3c.png", height=600, width=900)
SocialNetwork$moved2 <- factor(SocialNetwork$moved, levels=c("0", "1"), labels=c("Stayed", "Moved"))
p <- ggplot(data=SocialNetwork, aes(x=age))
p + geom_histogram(colour = "darkgrey", fill = "lightgrey", binwidth = 5) + 
  ylab("Count") + xlab("Age") + facet_wrap(~moved2) + 
  ggtitle("Age Distribution by whether the user moved away or not from his hometown") +
  theme_economist() + scale_color_economist()   
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot3d.png", height=600, width=900)
p <- ggplot(data=SocialNetwork, aes(x=age))
p + geom_density(colour = "darkgrey", fill = "lightgrey") + 
  ylab("Density") + xlab("Age") + facet_wrap(~moved2) + 
  ggtitle("Age Distribution by whether the user moved away or not from his hometown") +
  theme_economist()  
dev.off()

cor(SocialNetwork$moved, SocialNetwork$age)
# [1] 0.0242882

MYlm1 <- glm(moved ~ age, family="binomial", data=SocialNetwork)
summary(MYlm1)
require(coefplot)
coefplot(model=MYlm1)

#####################################################################################################################

### Characterize the relation between age and relationship status.
# In the relation_status column: 1-single, 2-in a relationship, 3-engaged, 4-married.

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot4a.png", height=600, width=900)
SocialNetwork$relation_status2 <- factor(SocialNetwork$relation_status, 
                                         levels=c("1", "2", "3", "4"), 
                                         labels=c("Single", "Relationship", "Engaged", "Married"))
p <- ggplot(data=SocialNetwork, aes(x=age))
p + geom_histogram(colour = "darkgrey", fill = "lightgrey", binwidth = 5) + 
  ylab("Count") + xlab("Age") + facet_wrap(~relation_status2) + 
  ggtitle("Age Distribution by Relationship Status") + theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot4b.png", height=600, width=900)
p <- ggplot(data=SocialNetwork, aes(x=age))
p + geom_density(colour = "darkgrey", fill = "lightgrey") + 
  ylab("Density") + xlab("Age") + facet_wrap(~relation_status2) + 
  ggtitle("Age Distribution by Relationship Status") + theme_economist()  
dev.off()

#####################################################################################################################

### Draw scatter plots between the number of total actions, visits, posts and time spent.

SocialNetwork2 <- subset(x=SocialNetwork[complete.cases(SocialNetwork),], 
                         select=c(total_action, number_visit, number_posts, time_spend))
names(SocialNetwork2) <- c("TotalAction", "Visits", "Posts", "TimeSpend")

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot5a.png", height=900, width=900)
require(GGally)
ggpairs(data=SocialNetwork2, alpha=0.4, axisLabels="none", 
        title = "Scatter Plots between total actions, visits, posts and time spent by the user") 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot5b.png", height=900, width=900)
library(gclus)
dta <- SocialNetwork2 # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Scatter Plots between total actions, visits, posts and time spent by the user
       Colored by Correlation" )
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot5c.png", height=900, width=900)
pairs(~ TotalAction + Visits + Posts + TimeSpend, data=SocialNetwork2, 
      main="Scatter Plots between total actions, visits, posts and time spent by the user")
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot5d.png", height=900, width=900)
require(lattice)
splom(x=SocialNetwork2, 
      varnames=c("Number of Total Actions", "Number of Visits", "Number of Posts", "Time Spend"))
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot5e.png", height=900, width=900)
require(car)
scatterplot.matrix(~ TotalAction + Visits + Posts + TimeSpend, data=SocialNetwork2,
                   main="Scatter Plots between total actions, visits, posts and time spent by the user")
dev.off()

#####################################################################################################################

### How does the sign up date influence the number of new friends they made?

# Change the sign_up from character to date
SocialNetwork$sign_up <- as.POSIXct(x=SocialNetwork$sign_up, format="%Y-%m-%d", out="%Y-%m-%d")
class(SocialNetwork$sign_up) # [1] "POSIXct" "POSIXt" 

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot6a.png", height=600, width=600)
p <- ggplot(data=SocialNetwork, aes(y=new_friend, x=sign_up))
p + geom_point() + geom_smooth(method="lm") +
  ylab("Number of New Friends") + xlab("Date when user signed up") + 
  ggtitle("Relation between the number of new friends and the sign up date of the user") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot6b.png", height=600, width=600)
require(lubridate)
SocialNetwork$Year <- year(SocialNetwork$sign_up)
SocialNetwork$Month <- month(SocialNetwork$sign_up)
p <- ggplot(data=SocialNetwork, mapping=aes(x=Month, y=new_friend, color=factor(Year))) 
p + geom_line() + labs(color="Year") +
  ylab("Number of New Friends") + xlab("Month") + 
  ggtitle("Relation between the number of new friends and the sign up date of the user
          Broken down by Year") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot6c.png", height=600, width=600)
p <- ggplot(data=SocialNetwork, mapping=aes(x=Month, y=new_friend))  
p + geom_point(position="jitter") +
  ylab("Number of New Friends") + xlab("Month") + facet_wrap(~Year) +
  ggtitle("Relation between the number of new friends and the sign up date of the user
          broken down by Year") +
  theme_economist() + scale_color_economist() 
dev.off()

cor(SocialNetwork$new_friend, SocialNetwork$Year)
# [1] 0.0242882

MYlm2 <- lm(new_friend ~ Year, data=SocialNetwork)
summary(MYlm2)
require(coefplot)
coefplot(model=MYlm2)

#####################################################################################################################

### Is there any relation between age and users’ actions?

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot7.png", height=600, width=600)
p <- ggplot(data=SocialNetwork, mapping=aes(x=age, y=total_action))
p + geom_point(color="darkgrey") + ylab("Number of Total Actions of the User") +
  xlab("Age") + ggtitle("Relationship between age and the users' actions") + 
  geom_smooth(method="lm") +
  theme_economist() + scale_color_economist() 
dev.off()

cor(SocialNetwork$total_action, SocialNetwork$age)
# [1] -0.03112057

MYlm3 <- lm(total_action ~ age, data=SocialNetwork)
summary(MYlm3)
require(coefplot)
coefplot(MYlm3)

#####################################################################################################################

### Do earlier users have more friends?

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot8a.png", height=600, width=600)
p <- ggplot(data=SocialNetwork, mapping=aes(x=Year, y=number_friends, color=factor(Year))) 
p + geom_point(position="jitter") + labs(color="Year") +
  ylab("Number of Friends") + xlab("Year") + 
  ggtitle("Relation between the number of friends and the sign up date of the user
          broken down by Year") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot8b.png", height=600, width=600)
p <- ggplot(data=SocialNetwork, mapping=aes(x=Month, y=number_friends))  
p + geom_point(position="jitter") +
  ylab("Number of Friends") + xlab("Month") + facet_wrap(~Year) +
  ggtitle("Relation between the number of friends and the sign up date of the user
          broken down by Year") +
  theme_economist() + scale_color_economist() 
dev.off()

mean(SocialNetwork$number_friends[SocialNetwork$Year == 2009]) # [1] 473.4653
mean(SocialNetwork$number_friends[SocialNetwork$Year == 2010]) # [1] 394.5956
mean(SocialNetwork$number_friends[SocialNetwork$Year == 2011]) # [1] 271.4576
mean(SocialNetwork$number_friends[SocialNetwork$Year == 2012]) # [1] 135.3091
mean(SocialNetwork$number_friends[SocialNetwork$Year == 2013]) # [1] 67.15174

aggregate(formula = number_friends ~ Year, data = SocialNetwork, FUN = mean)
#   Year number_friends
# 1 2009      473.46535
# 2 2010      394.59557
# 3 2011      271.45760
# 4 2012      135.30914
# 5 2013       67.15174

AggData <- as.data.frame(x=aggregate(formula = number_friends ~ Year, data = SocialNetwork, FUN = mean))
class(AggData) # [1] "data.frame"

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot8c.png", height=600, width=600)
p <- ggplot(data=AggData, mapping=aes(x=Year, y=number_friends))  
p + geom_point() +
  ylab("Average Number of Friends") + xlab("Sign Up Year") + 
  ggtitle("Relation between the number of friends and the sign up year of the user") +
  theme_economist() + scale_color_economist() 
dev.off()

cor(SocialNetwork$number_friends, SocialNetwork$Year)
# [1] -0.9632887

MYlm4 <- lm(number_friends ~ Year, data=SocialNetwork)
summary(MYlm4)
require(coefplot)
coefplot(MYlm4)

#####################################################################################################################

### (c). Think of the social network is a graph where each user is a node and a friendship is a edge between nodes. 
# An interesting character of a graph is the average degree. Let G be the graph, N be the number of nodes, and di be
# the number of edges that node i has. The average degree of the graph can be computed as follows:
# η(G) = 1/N sum(di)
# For a graph of 10000 you might be able to compute average degree. But for real social networks the number of users 
# (i.e., N) is more like 109 instead of 104. It is then computationally infeasible to get an exact answer. 
# However, we can sample. Let’s consider two sampling methods to estimate average degree. 
# In both methods, we begin with a uniformly random sample without replacement, let’s call this subgraph G ̃.

# Method 1: for each sampled user, we observe all the edges directly linked to that user.
# Method 2: an edge is observed if and only if both nodes of that edge have been sampled.

# Based on the subgraph G ̃ we sampled, calculate the estimates using the same formula above.
# Take the sample size as 3000, repeat this process for 10000 trials. 
# Draw the distributions of the two estimates (using histograms). 
# Also calculate the true average degree. 
# How do the two estimates compare to the true average degree? 
# Which sampling method is better? Why? Can you come up with an easy adjustment to correct the worse one?

# In order to use the Linkage matrix we need to first eliminate the user ID calumns
dim(Linkage) # [1] 10000 10001
Linkage2 <- Linkage[,-1] # eliminate the first column (names "X" which is the user ID)
dim(Linkage2) # [1] 10000 10000

###############################################################################################
### 1. Method 1: for each sampled user, we observe all the edges directly linked to that user.
###############################################################################################

set.seed(123456) # in order to replicate results
# 1. Build a function that samples 3,000 random rows
Function_that_samples_3000 <- function(x)
{
  sample(x=nrow(x), size=3000, replace=FALSE)
}
# 2. Average degree - build function that calculates agerage degree: sum rows / n
Function_calculates_average_degree <- function(x)
{
  sum(x=Linkage2[c(x),]) / length(x)
}
# 3. Build function that calls those functions (1 and 2)
Function_calls_other_functions <- function()
{
  Function_calculates_average_degree(Function_that_samples_3000(Linkage))
}
# 4. Call function 3 10,000 times
Method1_Edges <- replicate(n=10000, expr=Function_calls_other_functions()) # started at 7:22 am, ended at 9:38 am
head(Method1_Edges) # [1] 109.9900 108.7703 109.6700 108.6840 106.8717 108.6017
length(Method1_Edges) # [1] 10000

# Draw the distributions of the estimates (using histograms) from Method 1 
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_hist_Method1.png", height=600, width=600)
p <- ggplot(data=as.data.frame(Method1_Edges), aes(x=Method1_Edges))
p + geom_histogram(colour = "darkgrey", fill = "lightgrey", binwidth = .1) + 
  ylab("Count") + xlab("Average Degree") + 
  ggtitle("Distribution of the Average Degree using Method 1") +
  theme_economist()  
dev.off()

###############################################################################################
### Method 2: an edge is observed if and only if both nodes of that edge have been sampled.
###############################################################################################

set.seed(123456) # in order to replicate results
# 1. Build a function that samples 3,000 random rows
Function_that_samples_3000 <- function(x)
{
  sample(x=nrow(x), size=3000, replace=FALSE)
}
# 2. Average degree - build function that calculates agerage degree: sum by row / n
Function_calculates_average_degree <- function(x)
{
  sum(x=Linkage2[c(x),c(x)]) / length(x)
}
# 3. Build function that calls those functions (1 and 2)
Function_calls_other_functions <- function()
{
  Function_calculates_average_degree(Function_that_samples_3000(Linkage))
}
# 4. Call function 3 10,000 times
Method2_Edges <- replicate(n=10000, expr=Function_calls_other_functions()) # started at 9:44 am, ended at 10:26
head(Method2_Edges) # [1] 33.12600 32.55467 32.92667 32.27400 31.32733 32.25600
length(Method2_Edges) # [1] 10000

# Draw the distributions of the estimates (using histogram) from Method 2
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_hist_Method2.png", height=600, width=600)
p <- ggplot(data=as.data.frame(Method2_Edges), aes(x=Method2_Edges))
p + geom_histogram(colour = "darkgrey", fill = "lightgrey", binwidth = .1) + 
  ylab("Count") + xlab("Average Degree") + 
  ggtitle("Distribution of the Average Degree using Method 2") +
  theme_economist()  
dev.off()

### How do the two estimates compare to the true average degree? 
# Calculate the mean of Method 1 and Method 2
mean(Method1_Edges) # [1] 109.2418
mean(Method2_Edges) # [1] 32.76659
# Calculate the true average degree
True_average_degree <- function(x)
{
  sum(x=x) / dim(x)[1]
}
True_average_degree(Linkage2) # [1] 109.235

### Which sampling method is better? Why? Can you come up with an easy adjustment to correct the worse one?
# Method 1 for calculating average degree (109.2418) is identical to the first decimal point 
# to the true average degree (109.235)  
# Method 2 for calculating average degree (32.76659) is much smaller than the true average degree (109.235)
# So the sampling with Method 1 is superior in calculating average degree
# In the process of sampling with method two we used only 3000 of the 10000 possible edges, so we used only 30% of
# the edges to estimate the average degree, so we can look at this as if 32.76659 is the mean of 30% of the data,
# than what is the mean of 100%  of the data
# Using the formula below: mean(of 30% of the data) times 100 divided over 30
mean(Method2_Edges) * 100 / 30 # [1] 109.222
# With this calculation we can obtain a very good estimation of the average degree

#####################################################################################################################

### (d). Based on your analysis in (b),
### 1. Build a model to predict the number of total actions. Begin with a simple model and build up to 
# make more complex models by adding additional predictors, interactions between predictors and transforming 
# your predictors. Use cross-validation to evaluate the models.

SocialNetwork2 <- SocialNetwork[complete.cases(SocialNetwork),]

# I think it would be a good idea to bin age (as Andrew Gelman advices)
summary(SocialNetwork2$age)
#    Min.  1st Qu.  Median    Mean    3rd Qu.    Max. 
#    1.00   17.00   21.00     22.67    25.00    50.00 
SocialNetwork2$age2 <- cut(x=SocialNetwork2$age, breaks=c(0, 17, 20, 23, 26, 29, 39, 51), 
                          labels=c("Under 17 years", "18 to 20 years", "21 to 23 years", "24 to 26 years",
                                   "27 to 29 years", "30 to 39 years", "40 to 50 years"))
table(SocialNetwork2$age2)
# Under 17 years 18 to 20 years 21 to 23 years 24 to 26 years 27 to 29 years 30 to 39 years 40 to 50 years 
#     2510           2013           1938           1433            706            679            670 

SocialNetwork2 <- SocialNetwork2[complete.cases(SocialNetwork2),]

###########################################################################################################
### Linear Model 1
### Age, Gender, Relationship Status, Visits, Posts, Time Spend, New Friends, Total Friends, Moved, Year
###########################################################################################################
Model1 <- lm(total_action ~ age2 + factor(gender) + factor(relation_status) + number_visit + number_posts + 
               time_spend + new_friend + number_friends + factor(moved) + factor(Year), data=SocialNetwork2)
summary(Model1) # Look at the coefficients
# Plot the coefficients
require(coefplot)
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model1a.png", height=600, width=600)
coefplot(Model1, intercept=FALSE)
dev.off()
### Cross validation
require(boot) 
# Leave-one-out and 6-fold cross-validation prediction error
Model1_CV1 <- glm(total_action ~ age2 + factor(gender) + factor(relation_status) + number_visit + number_posts + 
                  time_spend + new_friend + number_friends + factor(moved) + factor(Year), 
                  data=SocialNetwork2, family=gaussian(link="identity"))
summary(Model1_CV1)
set.seed(123456)
Model1_CV1_GLM <- cv.glm(data=SocialNetwork2, glmfit=Model1_CV1, K=5)
Model1_CV1_GLM$delta # [1] 12864.19 12780.58
# As this is a linear model we could calculate the leave-one-out cross-validation estimate 
# without any extra model-fitting
MUhat <- fitted(Model1_CV1)
SocialNetwork2.diag <- glm.diag(Model1_CV1)
cv.err <- mean((Model1_CV1$y - MUhat)^2/(1 - SocialNetwork2.diag$h)^2)
cv.err # [1] 12423.66
### Plot cross-validation:
# Create a train variable to divide the data into train and test 
SocialNetwork2$train <- c(rep(TRUE, 8000), rep(FALSE, 1949))
require(pls)
Model1_CV2 <- plsr(total_action ~ age2 + factor(gender) + factor(relation_status) + number_visit + number_posts + 
                   time_spend + new_friend + number_friends + factor(moved) + factor(Year), 
                   ncomp = 10, data = SocialNetwork2[SocialNetwork2$train,], validation = "CV")
# Prediction Plot
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model1b.png", height=600, width=600)
predplot(Model1_CV2, ncomp = 10)
dev.off()
# Both cross-validated and test set predictions
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model1c.png", height=600, width=1200)
predplot(Model1_CV2, ncomp = 10, which = c("validation", "test"), 
         newdata = SocialNetwork2[!SocialNetwork2$train,])
dev.off()


###########################################################################################################
### Linear Model 2
### Age, Visits, Posts, Time Spend, New Friends, Total Friends, Year
### Interactions: Age:Visits
###########################################################################################################
Model2 <- lm(total_action ~ age2 + number_visit + number_posts + time_spend + 
             new_friend + number_friends + factor(Year) + age2:number_visit, 
             data=SocialNetwork2)
summary(Model2) # Look at the coefficients
# Plot the coefficients
require(coefplot)
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model2a.png", height=600, width=600)
coefplot(Model2, intercept=FALSE)
dev.off()
### Cross validation
require(boot) 
# Leave-one-out and 6-fold cross-validation prediction error
Model2_CV1 <- glm(total_action ~ age2 + number_visit + number_posts + time_spend + 
                  new_friend + number_friends + factor(Year) + age2:number_visit, 
                  data=SocialNetwork2, family=gaussian(link="identity"))
summary(Model2_CV1)
set.seed(123456)
Model2_CV1_GLM <- cv.glm(data=SocialNetwork2, glmfit=Model2_CV1, K=5)
Model2_CV1_GLM$delta # [1] 12953.03 12838.26
# As this is a linear model we could calculate the leave-one-out cross-validation estimate 
# without any extra model-fitting
MUhat <- fitted(Model2_CV1)
SocialNetwork2.diag <- glm.diag(Model2_CV1)
cv.err <- mean((Model2_CV1$y - MUhat)^2/(1 - SocialNetwork2.diag$h)^2)
cv.err # [1] 12848.51
### Plot cross-validation:
# Create a train variable to divide the data into train and test 
SocialNetwork2$train <- c(rep(TRUE, 8000), rep(FALSE, 1949))
require(pls)
Model2_CV2 <- plsr(total_action ~ age2 + number_visit + number_posts + time_spend + 
                   new_friend + number_friends + factor(Year) + age2:number_visit, 
                   ncomp = 8, data = SocialNetwork2[SocialNetwork2$train,], validation = "CV")
# Prediction Plot
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model2b.png", height=600, width=600)
predplot(Model2_CV2) 
dev.off()
# Both cross-validated and test set predictions
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model2c.png", height=600, width=1200)
predplot(Model2_CV2, which = c("validation", "test"), 
         newdata = SocialNetwork2[!SocialNetwork2$train,])
dev.off()


###########################################################################################################
### Linear Model 3
### Age, Visits, Posts, Time Spend, New Friends, Total Friends, Year
### interactions: Age:Visits, Year:NewFriends
###########################################################################################################
Model3 <- lm(total_action ~ age2 + number_visit + number_posts + time_spend + 
             new_friend + number_friends + factor(Year) + age2:number_visit + factor(Year):new_friend, 
             data=SocialNetwork2)
summary(Model3) # Look at the coefficients
# Plot the coefficients
require(coefplot)
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model3a.png", height=600, width=600)
coefplot(Model3, intercept=FALSE)
dev.off()
### Cross validation
require(boot) 
# Leave-one-out and 6-fold cross-validation prediction error
Model3_CV1 <- glm(total_action ~ age2 + number_visit + number_posts + time_spend + 
                  new_friend + number_friends + factor(Year) + age2:number_visit + factor(Year):new_friend, 
                  data=SocialNetwork2, family=gaussian(link="identity"))
summary(Model3_CV1)
set.seed(123456)
Model3_CV1_GLM <- cv.glm(data=SocialNetwork2, glmfit=Model3_CV1, K=5)
Model3_CV1_GLM$delta # [1] 12497.43 12345.00
# As this is a linear model we could calculate the leave-one-out cross-validation estimate 
# without any extra model-fitting
MUhat <- fitted(Model3_CV1)
SocialNetwork2.diag <- glm.diag(Model3_CV1)
cv.err <- mean((Model3_CV1$y - MUhat)^2/(1 - SocialNetwork2.diag$h)^2)
cv.err # [1] 11984.28
### Plot cross-validation:
# Create a train variable to divide the data into train and test 
SocialNetwork2$train <- c(rep(TRUE, 8000), rep(FALSE, 1949))
require(pls)
Model3_CV2 <- plsr(total_action ~ age2 + number_visit + number_posts + time_spend + 
                   new_friend + number_friends + factor(Year) + age2:number_visit + factor(Year):new_friend, 
                   ncomp = 9, data = SocialNetwork2[SocialNetwork2$train,], validation = "CV")
# Prediction Plot
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model3b.png", height=600, width=600)
predplot(Model3_CV2) 
dev.off()
# Both cross-validated and test set predictions
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model3c.png", height=600, width=1200)
predplot(Model3_CV2, which = c("validation", "test"),
         newdata = SocialNetwork2[!SocialNetwork2$train,])
dev.off()


###########################################################################################################
### Linear Model 4
### Age, Visits, Posts, Time Spend, New Friends, Total Friends, Year
### interactions: Age:Visits, Year:NewFriends, Age:TimeSpend, Visits:posts, TimeSpend:NumberFriends
###########################################################################################################
Model4 <- lm(total_action ~ age2 + number_visit + number_posts + time_spend + 
             new_friend + number_friends + factor(Year) + age2:number_visit + factor(Year):new_friend +
             age2:time_spend + number_visit:number_posts + time_spend:number_friends, 
             data=SocialNetwork2)
summary(Model4) # Look at the coefficients
# Plot the coefficients
require(coefplot)
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model4a.png", height=600, width=600)
coefplot(Model4, intercept=FALSE)
dev.off()
### Cross validation
require(boot) 
# Leave-one-out and 6-fold cross-validation prediction error
Model4_CV1 <- glm(total_action ~ age2 + number_visit + number_posts + time_spend + 
                    new_friend + number_friends + factor(Year) + age2:number_visit + factor(Year):new_friend +
                    age2:time_spend + number_visit:number_posts + time_spend:number_friends, 
                  data=SocialNetwork2, family=gaussian(link="identity"))
summary(Model4_CV1)
set.seed(123456)
Model4_CV1_GLM <- cv.glm(data=SocialNetwork2, glmfit=Model4_CV1, K=5)
Model4_CV1_GLM$delta # [1] 8085.096 7996.161
# As this is a linear model we could calculate the leave-one-out cross-validation estimate 
# without any extra model-fitting
MUhat <- fitted(Model4_CV1)
SocialNetwork2.diag <- glm.diag(Model4_CV1)
cv.err <- mean((Model4_CV1$y - MUhat)^2/(1 - SocialNetwork2.diag$h)^2)
cv.err # [1] 7818.903
### Plot cross-validation:
# Create a train variable to divide the data into train and test 
SocialNetwork2$train <- c(rep(TRUE, 8000), rep(FALSE, 1949))
require(pls)
Model4_CV2 <- plsr(total_action ~ age2 + number_visit + number_posts + time_spend + 
                     new_friend + number_friends + factor(Year) + age2:number_visit + factor(Year):new_friend +
                     age2:time_spend + number_visit:number_posts + time_spend:number_friends, 
                   ncomp = 12, data = SocialNetwork2[SocialNetwork2$train,], validation = "CV")
# Prediction Plot
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model4b.png", height=600, width=600)
predplot(Model4_CV2) 
dev.off()
# Both cross-validated and test set predictions
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Model4c.png", height=600, width=1200)
predplot(Model4_CV2, which = c("validation", "test"),
         newdata = SocialNetwork2[!SocialNetwork2$train,])
dev.off()


##########################################
### Plot coefficients from multiple models
##########################################
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Multiplot_Model.png", height=1200, width=900)
multiplot(Model1, Model2, Model3, Model4, intercept=FALSE)
dev.off()

#####################################################################################################################

### 2. Segment your users based first on reasonable heuristics and then using k-means. Try varying k=2,3,4,5. 
# What is the optimal k? Why? Find a way to characterize your clusters using summary statistics and/or visualizations.

######################################################
### Segment users based on reasonable heuristics
######################################################
### One possible segmentation is based on the year the user signed up
table(SocialNetwork2$Year)
# 2009 2010 2011 2012 2013 
# 100  494  565 1458 7332 
### Another segmentation based on the relationship status of the user
table(SocialNetwork2$relation_status2)
#       Single Relationship      Engaged      Married 
#       3904         3198          210         2637 
### Another segmentation based on bins of age of the user
table(SocialNetwork2$age2)
# Under 17 years 18 to 20 years 21 to 23 years 24 to 26 years 27 to 29 years 30 to 39 years 40 to 50 years 
#   2510           2013           1938           1433            706            679            670 

#############################
### Segment users by K-Means
### K = 2
#############################
require(useful)
set.seed(123456) # set seed in order to replicate results
sum(as.numeric(complete.cases(SocialNetwork))) # [1] 9950 - complete cases
SocialNetwork3 <- SocialNetwork[complete.cases(SocialNetwork),c(1:8, 11:15)]
SocialNetwork_K2 <- kmeans(x=SocialNetwork3, centers=2)
SocialNetwork_K2
K2 <- as.vector(SocialNetwork_K2$cluster) # each row is in each cluster
K2 # vector with indices (1 or 2) to direct to the which cluster the each row belongs to  
SocialNetwork_K2$centers # centers for each cluster
#     userid      age    gender relation_status hometown   living perference total_action
# 1 7501.503 22.69294 0.4970843        2.178765 3.527850 3.517193   3.524432     215.8438
# 2 2501.008 22.65702 0.4976894        2.138638 3.492867 3.519992   3.512156     219.4840
# number_visit number_posts time_spend new_friend number_friends
# 1     10.43817     10.38045   8.589851   4.268450       110.1550
# 2     10.38336     10.23106   8.499862   4.394816       108.4135
SocialNetwork_K2$totss # points within a cluster should be more similar than points ouside cluster
# [1] 84100172728     # totss = total sum of squares
SocialNetwork_K2$size # how many points are in each cluster
# [1] 4973 4977

#############################
### Segment users by K-Means
### K = 3
#############################
require(useful)
set.seed(123456) # set seed in order to replicate results
SocialNetwork_K3 <- kmeans(x=SocialNetwork3, centers=3)
SocialNetwork_K3
K3 <- as.vector(SocialNetwork_K3$cluster) # each row is in each cluster
K3 # vector with indices (1, 2 or 3) to direct to the which cluster the each row belongs to  
SocialNetwork_K3$centers # centers for each cluster
#     userid      age    gender relation_status hometown   living perference total_action
# 1 1665.286 22.65812 0.5057333        2.121304 3.492758 3.510561   3.508147     221.9882
# 2 8331.823 22.72998 0.4990969        2.177604 3.540638 3.539434   3.516556     215.2495
# 3 4995.600 22.63669 0.4873265        2.177127 3.497586 3.505733   3.530175     215.7619
# number_visit number_posts time_spend new_friend number_friends
# 1     10.40103     10.23386   8.550748   4.461376       109.5483
# 2     10.49849     10.38892   8.610498   4.261288       110.4401
# 3     10.33253     10.29421   8.473111   4.272480       107.8606
SocialNetwork_K3$totss # points within a cluster should be more similar than points ouside cluster
# [1] 84100172728     # totss = total sum of squares
SocialNetwork_K3$size # how many points are in each cluster
# [1] 3314 3322 3314

#############################
### Segment users by K-Means
### K = 4
#############################
require(useful)
set.seed(123456) # set seed in order to replicate results
SocialNetwork_K4 <- kmeans(x=SocialNetwork3, centers=4)
SocialNetwork_K4
K4 <- as.vector(SocialNetwork_K4$cluster) # each row is in each cluster
K4 # vector with indices (1, 2, 3 or 4) to direct to the which cluster the each row belongs to  
SocialNetwork_K4$centers # centers for each cluster
#     userid      age    gender relation_status hometown   living perference total_action
# 1 8750.373 22.61712 0.5082362        2.171555 3.525914 3.515066   3.523503     216.2784
# 2 6248.612 22.76357 0.4857258        2.186570 3.528347 3.517893   3.524729     215.3671
# 3 3748.254 22.56930 0.4945761        2.144235 3.489755 3.522700   3.516272     216.7079
# 4 1248.738 22.75010 0.5010060        2.132394 3.497384 3.518712   3.508652     222.3107
# number_visit number_posts time_spend new_friend number_friends
# 1     10.56730     10.40297   8.606019   4.220972       112.1101
# 2     10.30921     10.35947   8.573303   4.314033       108.1492
# 3     10.40056     10.24468   8.451279   4.319004       107.9630
# 4     10.36579     10.21569   8.548782   4.472837       108.9119
SocialNetwork_K4$totss # points within a cluster should be more similar than points ouside cluster
# [1] 84100172728     # totss = total sum of squares
SocialNetwork_K4$size # how many points are in each cluster
# [1] 2489 2487 2489 2485

#############################
### Segment users by K-Means
### K = 5
#############################
require(useful)
set.seed(123456) # set seed in order to replicate results
SocialNetwork_K5 <- kmeans(x=SocialNetwork3, centers=5)
SocialNetwork_K5
K5 <- as.vector(SocialNetwork_K5$cluster) # each row is in each cluster
K5 # vector with indices (1, 2, 3, 4 or 5) to direct to the which cluster the each row belongs to  
SocialNetwork_K5$centers # centers for each cluster
#     userid      age    gender relation_status hometown   living perference total_action
# 1 9000.862 22.47588 0.5080402        2.167337 3.517588 3.496985   3.529648     216.9960
# 2 7006.132 22.99747 0.4909091        2.205051 3.507071 3.509091   3.509596     215.4207
# 3 5008.196 22.55020 0.4859438        2.175201 3.538655 3.563253   3.507028     211.9578
# 4 1002.467 22.74586 0.4967419        2.130827 3.501253 3.508772   3.500251     214.1850
# 5 3006.720 22.60712 0.5052684        2.115404 3.487205 3.514802   3.544907     229.7486
# number_visit number_posts time_spend new_friend number_friends
# 1     10.61709     10.47688   8.604820   4.174874       112.8673
# 2     10.29949     10.26818   8.565778   4.379293       108.2414
# 3     10.37600     10.33032   8.488805   4.167671       107.7395
# 4     10.25414     10.13484   8.543738   4.340351       107.2466
# 5     10.50677     10.31862   8.521251   4.596086       110.3246
SocialNetwork_K5$totss # points within a cluster should be more similar than points ouside cluster
# [1] 84100172728     # totss = total sum of squares
SocialNetwork_K5$size # how many points are in each cluster
# [1] 1990 1980 1992 1995 1993

#####################################################
### Optimal K
### How do you know how many clusters are appropriate
#####################################################
### Hardigans rule - measure between sum of squares and within sum of squares
SocialNetworkBest <- FitKMeans(x=SocialNetwork3, max.clusters=5, seed=123456)
SocialNetworkBest
#   Clusters  Hartigan AddCluster
# 1        2 28253.637       TRUE
# 2        3 11060.803       TRUE
# 3        4  6307.504       TRUE
# 4        5  4147.131       TRUE
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Hartigan.png", height=600, width=600)
PlotHartigan(SocialNetworkBest)
dev.off()
### Gap Statistics - bootstrap best way to do things
require(cluster)
theGap <- clusGap(x=SocialNetwork3, K.max=5, FUNcluster=pam) 
# clusGap() - calculate with bootstrap to see which is the best number of clusters
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Gap.png", height=600, width=600)
plot(theGap) # look at the plot at which k-clusters are the closest to the ideal
dev.off()

##############################################################################
### In conclusion I decided that the model with 5 clusters is the optimal one
##############################################################################
### Next we will explore through visualization the characteristics of our 5 clusters

SocialNetwork3$Clusters <- as.vector(SocialNetwork_K5$cluster)
table(SocialNetwork3$Clusters)
#    1    2    3    4    5 
# 1990 1980 1992 1995 1993 
head(SocialNetwork3, 3)
#   userid age gender relation_status hometown living perference total_action
# 1      1  23      0               2        6      1          4          253
# 2      2  50      1               4        4      4          6          119
# 3      3  49      1               4        2      3          4          230
#   number_visit number_posts time_spend new_friend number_friends Clusters
# 1           14            5  14.046551          2             71        4
# 2           10           14   5.380026          2             68        4
# 3           15           10   8.513190          4             75        4

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot9a.png", height=600, width=600)
p <- ggplot(data=SocialNetwork3, mapping=aes(x=age, y=number_friends))  
p + geom_point(position="jitter") +
  ylab("Number of Friends") + xlab("Age") + facet_wrap(~Clusters) +
  ggtitle("Relation between the number of friends and the age of the user
          Broken down by Cluster") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot9b.png", height=600, width=600)
p <- ggplot(data=SocialNetwork3, mapping=aes(x=number_visit, y=total_action))  
p + geom_point(position="jitter") +
  ylab("Total Actions of the User") + xlab("Number of Visits") + facet_wrap(~Clusters) +
  ggtitle("Relation between the total actions of the user and his number of visits
          Broken down by Cluster") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot9c.png", height=600, width=600)
p <- ggplot(data=SocialNetwork3, mapping=aes(x=number_posts, y=total_action))  
p + geom_point(position="jitter") +
  ylab("Total Actions of the User") + xlab("Number of Posts") + facet_wrap(~Clusters) +
  ggtitle("Relation between the total actions of the user and his number of posts
          Broken down by Cluster") +
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_plot9d.png", height=600, width=600)
p <- ggplot(data=SocialNetwork3, mapping=aes(x=time_spend, y=total_action))  
p + geom_point(position="jitter") +
  ylab("Total Actions of the User") + xlab("Time Spend") + facet_wrap(~Clusters) +
  ggtitle("Relation between the total actions of the user and his time spend
          Broken down by Cluster") +
  theme_economist() + scale_color_economist() 
dev.off()


#####################################################################################################################

### 3. Once you’ve settled on your user segments, count the number of edges within each cluster of users and 
# between each pair of clusters. Now imagine you didn’t have access to the dataset that contains user profile 
# information, is it possible to detect these same clusters of users using only the friendship matrix?

dim(Linkage) # [1] 10000 10001
dim(SocialNetwork3) # [1] 9950   14 - here we needed to exclude the  NA's in order to do K-Means
SocialNetwork_Linkage <- merge(SocialNetwork3, Linkage, by.x="userid", by.y="X")
head(names(SocialNetwork_Linkage), 50)
dim(SocialNetwork_Linkage) # [1]  9950 10014
table(SocialNetwork_Linkage$Clusters)
#    1    2    3    4    5 
# 1990 1980 1992 1995 1993 

### Create vector with the row indices for each cluster, to use it in the next step to subset the data by 
Cluster1 <- SocialNetwork_Linkage$userid[SocialNetwork_Linkage$Clusters == 1]
length(Cluster1) # [1] 1990
Cluster2 <- SocialNetwork_Linkage$userid[SocialNetwork_Linkage$Clusters == 2]
length(Cluster2) # [1] 1980
Cluster3 <- SocialNetwork_Linkage$userid[SocialNetwork_Linkage$Clusters == 3]
length(Cluster3) # [1] 1992
Cluster4 <- SocialNetwork_Linkage$userid[SocialNetwork_Linkage$Clusters == 4]
length(Cluster4) # [1] 1995
Cluster5 <- SocialNetwork_Linkage$userid[SocialNetwork_Linkage$Clusters == 5]
length(Cluster5) # [1] 1993

################################################
### Number of edges within each cluster of users
################################################

### Count the number of edges within Cluster 1
sum(SocialNetwork_Linkage[c(Cluster1), c(Cluster1)], na.rm=TRUE) # [1] 45332

### Count the number of edges within Cluster 2
sum(SocialNetwork_Linkage[c(Cluster2), c(Cluster2)], na.rm=TRUE) # [1] 41974

### Count the number of edges within Cluster 3
sum(SocialNetwork_Linkage[c(Cluster3), c(Cluster3)], na.rm=TRUE) # [1] 41636

### Count the number of edges within Cluster 4
sum(SocialNetwork_Linkage[c(Cluster4), c(Cluster4)], na.rm=TRUE) # [1] 2842778

### Count the number of edges within Cluster 5
sum(SocialNetwork_Linkage[c(Cluster5), c(Cluster5)], na.rm=TRUE) # [1] 43895

#################################################
### Number of edges between each pair of clusters
#################################################

### Count the number of edges between Cluster 1 and Cluster 2
sum(SocialNetwork_Linkage[c(Cluster1), c(Cluster2)], na.rm=TRUE) # [1] 43378

### Count the number of edges between Cluster 1 and Cluster 3
sum(SocialNetwork_Linkage[c(Cluster1), c(Cluster3)], na.rm=TRUE) # [1] 43031

### Count the number of edges between Cluster 1 and Cluster 4
sum(SocialNetwork_Linkage[c(Cluster1), c(Cluster4)], na.rm=TRUE) # [1] 18332815

### Count the number of edges between Cluster 1 and Cluster 5
sum(SocialNetwork_Linkage[c(Cluster1), c(Cluster5)], na.rm=TRUE) # [1] 44491

### Count the number of edges between Cluster 2 and Cluster 3
sum(SocialNetwork_Linkage[c(Cluster2), c(Cluster3)], na.rm=TRUE) # [1] 41945

### Count the number of edges between Cluster 2 and Cluster 4
sum(SocialNetwork_Linkage[c(Cluster2), c(Cluster4)], na.rm=TRUE) # [1] 14767454

### Count the number of edges between Cluster 2 and Cluster 5
sum(SocialNetwork_Linkage[c(Cluster2), c(Cluster5)], na.rm=TRUE) # [1] 43062

### Count the number of edges between Cluster 3 and Cluster 4
sum(SocialNetwork_Linkage[c(Cluster3), c(Cluster4)], na.rm=TRUE) # [1] 10853189

### Count the number of edges between Cluster 3 and Cluster 5
sum(SocialNetwork_Linkage[c(Cluster3), c(Cluster5)], na.rm=TRUE) # [1] 43404

### Count the number of edges between Cluster 4 and Cluster 5
sum(SocialNetwork_Linkage[c(Cluster4), c(Cluster5)], na.rm=TRUE) # [1] 42895

###############################################################################################
### Now imagine you didn’t have access to the dataset that contains user profile information, 
### is it possible to detect these same clusters of users using only the friendship matrix ?
###############################################################################################

### The way I am approaching the problem here is that I want to use the K_means on the Linkage
# dataset, however the linkage is a 10000 by 10000 data frame, so what I decided to do instead
# is to subset the the linkage and the social network datasets by the same list is user ID's
### First, I will take a random sample of 50 rows from the Social Network dataset, and subset 
# the Social Network dataset by these rows. I will also subset the Linkage data frame by the 
# sampled row, so the new Linkage file should be a 50 by 50 data frame instead of 10000 by 10000
### The reason I am doing this is because of time constraints. If I can obtain viable results
# o such a small sample than I would expect to be able to replicate the results on the
# entire data frame

### First take the random sample of 50 rows
set.seed(123456)
SampleRows <- sample(x=SocialNetwork3$userid, size=50, replace=FALSE)
SampleRows
#  [1] 7980 7537 3912 3416 3613 1983 5344  965 9871 1673 7975 5929 9042 8798 9924 8947 8774 1972 3344 7759
# [21] 1591  805 1323 1714 4766 6989 8778 8756 8517 1654 5213 8666 2304 1187 8214  800 9488 8686 7095 6028
# [41] 2245 7366 8919 2932 5131 4328 5724 7376 6751 3604

### Subset the two data frames by the same 50 rows
Sample_SocialNetwork <- SocialNetwork3[c(SampleRows),]
dim(Sample_SocialNetwork) # [1] 50 14
Sample_Linkage <- Linkage2[c(SampleRows),c(SampleRows)]
dim(Sample_Linkage) # [1] 50 50
Sample_SocialNetwork$number_friends <- apply(X=Sample_Linkage, MARGIN=1, FUN=sum)
Sample_Linkage2 <- as.data.frame(apply(X=Sample_Linkage, MARGIN=1, FUN=sum))

#########################################################
### Optimal K for Sample_SocialNetwork and Sample_Linkage
### How do you know how many clusters are appropriate
#########################################################

### Hardigans rule - measure between sum of squares and within sum of squares
Sample_SocialNetworkBest <- FitKMeans(x=Sample_SocialNetwork, max.clusters=5, seed=123456)
Sample_SocialNetworkBest
#   Clusters   Hartigan AddCluster
# 1        2 200.603652       TRUE
# 2        3  59.335843       TRUE
# 3        4  37.368386       TRUE
# 4        5   9.129961      FALSE
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Hartigan_Sample1.png", height=600, width=600)
PlotHartigan(Sample_SocialNetworkBest)
dev.off()
Sample_LinkageBest <- FitKMeans(x=Sample_Linkage2, max.clusters=5, seed=123456)
Sample_LinkageBest
#   Clusters   Hartigan AddCluster
# 1        2   91.86561       TRUE
# 2        3   16.92245       TRUE
# 3        4   25.35309       TRUE
# 4        5 1224.64286       TRUE
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Hartigan_Sample2.png", height=600, width=600)
PlotHartigan(Sample_LinkageBest)
dev.off()

### Gap Statistics - bootstrap best way to do things
Sample_SocialNetwork_theGap <- clusGap(x=Sample_SocialNetwork, K.max=5, FUNcluster=pam) 
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Gap_Sample1.png", height=600, width=600)
plot(Sample_SocialNetwork_theGap) # look at the plot at which k-clusters are the closest to the ideal
dev.off()

Sample_Linkage_theGap <- clusGap(x=Sample_Linkage2, K.max=5, FUNcluster=pam) 
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 2/Graphics")
png ("HW2_Gap_Sample2.png", height=600, width=600)
plot(Sample_Linkage_theGap) # look at the plot at which k-clusters are the closest to the ideal
dev.off()


#############################
### Segment users by K-Means
#############################
require(useful)
set.seed(123456) # set seed in order to replicate results
Sample_SocialNetwork_K <- kmeans(x=Sample_SocialNetwork, centers=2)
Sample_SocialNetwork_K
Sample_K <- as.vector(Sample_SocialNetwork_K$cluster) # each row is in each cluster
Sample_K # vector with indices (1, 2, 3, 4 or 5) to direct to the which cluster the each row belongs to  
# [1] 2 2 1 1 1 1 1 1 2 1 2 2 2 2 2 2 2 1 1 2 1 1 1 1 1 2 2 2 2 1 1 2 1 1 2 1 2 2 2 2 1 2 2 1 1 1 2 2 2 1
Sample_SocialNetwork_K$centers # centers for each cluster
Sample_SocialNetwork_K$size # how many points are in each cluster
# [1] 24 26

set.seed(123456) # set seed in order to replicate results
Sample_Linkage_K <- kmeans(x=Sample_Linkage2, centers=2)
Sample_Linkage_K
Sample_K_link <- as.vector(Sample_Linkage_K$cluster) # each row is in each cluster
Sample_K_link # vector with indices (1, 2, 3, 4 or 5) to direct to the which cluster the each row belongs to  
# [1] 1 2 1 1 1 1 2 1 2 1 2 2 1 1 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 2 2 2 1 2 1 1 1 1 2 2 2 1 1 1 1
Sample_Linkage_K$centers # centers for each cluster
Sample_Linkage_K$size # how many points are in each cluster
# [1] 33 17

# Calculate for many element were distributed in the same clusters in the SocialNetwrk and Linkage
sum(as.numeric(Sample_K == Sample_K_link))
# [1] 25
# So only half of the numbers were corectly placed in the same clusters using K-Means for 
# the Sample_SocialNetwork and Sample_Linkage

#####################################################################################################################