#####################################################################################################################
#####################################################################################################################
### INTRODUCTION TO DATA SCIENCE
### Homework 1
#####################################################################################################################
#####################################################################################################################

rm(list=ls())
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 1")

#####################################################################################################################
###############################################   Q1    #############################################################
# http://www.nuforc.org/webreports/ndxshape.html
# "readLines", "grep", "gsub"  and "strsplit" 
# UFOs corresponding to one of three shapes: circle, triangle and fireball.
# Circle: http://www.nuforc.org/webreports/ndxsCircle.html
# Triangle: http://www.nuforc.org/webreports/ndxsTriangle.html
# Fireball: http://www.nuforc.org/webreports/ndxsFireball.html
# Collect all sightings from that list up to and including September 9, 2013
# Eight features: 
                # Data of sighting, 
                # Time of sighting, 
                # City, 
                # State, 
                # Shape, 
                # Duration, 
                # Summary, 
                # Posted date (when the sighting was posted to the website)
# Convert all Durations to seconds
# Cleaned dataset in csv format
# If a duration has a“<” sign, you should simply ignore the“<” sign. 
          # For example if the duration is specified as “< 1 minute”, 
          # consider the duration to be “1 minute”. You should subsequently 
          # convert “1 minute” to “60 seconds”.
# If a duration has a range, use the upper limit as its value. 
          # For example, if the duration is listed as “5-8 minutes”, 
          # you should consider the duration as “8 minutes”. 
          # (Again, you will need to eventually convert minutes into seconds).
# You may encounter some other oddities in the data. 
          # Do your best to extract maximum value from the messy data; 
          # be sure to explain to us the decisions you have made in terms 
          # of data extraction and cleaning.
#####################################################################################################################

#########################
###### UFO Circles ######
#########################

# Get the page's source - Circle UFO's
WebPage <- readLines("http://www.nuforc.org/webreports/ndxsCircle.html")
head(WebPage, 50)
length(WebPage) # [1] 85906

# Pull out the appropriate headers
Header <- WebPage[grep("<TH BGCOLOR=#c0c0c0 BORDERCOLOR=#000000 >", WebPage)]
head(Header)

# Delete unwanted characters in the lines we pulled out above (Header)
MyPatternHeader <- c("<TH BGCOLOR=#c0c0c0 BORDERCOLOR=#000000 >", "</TH>",
                     "<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>")
MyReplacementHeader <- c(rep(x="", length(MyPatternHeader)))
MYgsub <- function(MyPattern, MyReplacement, x) 
{
  for(i in 1:length(MyPattern))
    x <- gsub(MyPattern[i], MyReplacement[i], x)
  x
}
CleanHeader <- MYgsub(MyPatternHeader, MyReplacementHeader, Header)
CleanHeader[1] <- "DateTime"
CleanHeader # [1] "DateTime" "City"     "State"    "Shape"    "Duration" "Summary"  "Posted"  
class(CleanHeader) # [1] "character"

# Pull out the appropriate lines of text
Lines <- WebPage[grep("<TD", WebPage)]
head(Lines, 50)
length(Lines) # [1] 54642

# Delete unwanted characters in the lines we pulled out above (Lines)
MyPatternLines <- c("<TD><FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>", "</TD>",  "</A>",
                    "<TD bgcolor=\"#FFFFCC\" ><FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>",
                    paste0("<A HREF=", "[[:digit:]]{3}", "/S", "[[:digit:]]{6}", ".html>", collapse = "|"),
                    paste0("<A HREF=", "[[:digit:]]{3}", "/S", "[[:digit:]]{5}", ".html>", collapse = "|"))
MyReplacementLines <- c(rep(x="", length(MyPatternLines)))
CleanLines <- MYgsub(MyPatternLines, MyReplacementLines, Lines)
head(CleanLines, 50)
class(CleanLines)
length(CleanLines) # [1] 54642

# Create a vector of CleanHeaders the lenght of the CleanLines
length(CleanHeader) # [1] 7
CleanHeaderLong <- rep(x=CleanHeader, length(CleanLines)/length(CleanHeader))
length(CleanHeaderLong) # [1] 54642
length(CleanLines) # [1] 54642

# Combine the two vectors into one dataframe
Number <- c(1:(length(CleanLines)/7))
ID <- rep(Number, each=7)
length(ID) # [1] 54642
MyUFOCircles <- data.frame(cbind(ID, CleanHeaderLong, CleanLines))
class(MyUFOCircles) # [1] "data.frame"
names(MyUFOCircles) # [1] "ID" "CleanHeaderLong" "CleanLines"  
MyUFOCircles[1:50, 1:3]

# We have a long data frame and we need to transform it into a wide one (decast)
require(reshape2)
MyUFOCirclesWide <- dcast(data = MyUFOCircles, formula = ID ~ CleanHeaderLong, value.var="CleanLines")
head(MyUFOCirclesWide)
names(MyUFOCirclesWide) # [1] "ID" "City" "DateTime" "Duration" "Posted" "Shape" "State" "Summary"
dim(MyUFOCirclesWide) # [1] 7806    8

# "Date / Time" are combined in one vector and we need to split them up
# First, extract the Date vector
MyUFOCirclesWide$DateRaw <- gsub(pattern=" .*", replacement="", MyUFOCirclesWide$DateTime)
MyUFOCirclesWide$DateRaw

# Clean the Date vector
MyPatternDate <- c(paste0("[[:digit:]]{2}", ":", "[[:digit:]]{2}", collapse = "|"),
                   "1972-1974", "1998", "[[:alpha:]]{5,}")
MyReplacementDate <- c(rep(x="", length(MyPatternDate)))
MyUFOCirclesWide$Date <- MYgsub(MyPatternDate, MyReplacementDate, MyUFOCirclesWide$DateRaw)
MyUFOCirclesWide$Date

# Change the Date vector from character to date format
class(MyUFOCirclesWide$Date) # [1] "character"
MyUFOCirclesWide$Date <- as.Date(x=MyUFOCirclesWide$Date, format="%m/%d/%y")
MyUFOCirclesWide$Date
class(MyUFOCirclesWide$Date) # [1] "Date"

# Second, extract the Time vector 
MyPatternTime <- paste0("[[:digit:]]{1,}", "/", "[[:digit:]]{1,}", "/", "[[:digit:]]{2}", " ", collapse = "|")
MyUFOCirclesWide$TimeRaw <- gsub(pattern = MyPatternTime, replacement="", x = MyUFOCirclesWide$DateTime)
MyUFOCirclesWide$TimeRaw

# Clean the Time vector
MyPatternTime2 <- c(paste0("[[:digit:]]{4}", "-", "[[:digit:]]{4}", collapse = "|"),
                    "1972-1974", "[[:digit:]]{4}", "[[:alpha:]]{1,}", paste0("", ",", "", collapse = "|"),
                    paste0("[[:digit:]]{2}", " ", "[[:digit:]]{2}", collapse = "|"), 
                    paste0(" ", " ", collapse = "|"), " ", "80",
                    paste0("[[:digit:]]{1,}", "/", "[[:digit:]]{1,}", "/", "[[:digit:]]{2}", collapse = "|") )
MyReplacementTime <- c(rep(x="", length(MyPatternTime2)))
MyUFOCirclesWide$Time <- MYgsub(MyPatternTime2, MyReplacementTime, MyUFOCirclesWide$TimeRaw)
MyUFOCirclesWide$Time

# Change the Time vector from character to time format
class(MyUFOCirclesWide$Time) # [1] "character"
MyUFOCirclesWide$Time <- as.POSIXct(x=MyUFOCirclesWide$Time, format="%H:%M")
MyUFOCirclesWide$Time
class(MyUFOCirclesWide$Time) # [1] "POSIXlt" "POSIXt" 

# Subset dataset so we can exclude NA values in Time
dim(MyUFOCirclesWide) # [1] 7806   12
MyUFOCirclesWideSubset <- MyUFOCirclesWide[(!is.na(MyUFOCirclesWide$Time)),]
dim(MyUFOCirclesWideSubset) # [1] 7696  12
MyUFOCirclesWideSubset$Time <- format(x=MyUFOCirclesWideSubset$Time, format="%H:%M")
MyUFOCirclesWideSubset$Time

# Clean the Posted vector
MyPatternPosted <- c("<TD><")
MyReplacementPosted <- c(rep(x="", length(MyPatternPosted)))
MyUFOCirclesWideSubset$Posted <- MYgsub(MyPatternPosted, MyReplacementPosted, MyUFOCirclesWideSubset$Posted)
MyUFOCirclesWideSubset$Posted

# Change the Date vector from character to date format
class(MyUFOCirclesWideSubset$Posted) # [1] "character"
MyUFOCirclesWideSubset$Posted <- as.Date(x=MyUFOCirclesWideSubset$Posted, format="%m/%d/%y")
MyUFOCirclesWideSubset$Posted
class(MyUFOCirclesWideSubset$Posted) # [1] "Date"

# Subset dataset so we can exclude NA values in Posted
dim(MyUFOCirclesWideSubset) # [1] 7696   12
MyUFOCirclesWideSubset2 <- MyUFOCirclesWideSubset[(!is.na(MyUFOCirclesWideSubset$Posted)),]
dim(MyUFOCirclesWideSubset2) # [1] 7696   12

# Clean the State vector
table(MyUFOCirclesWideSubset2$City[MyUFOCirclesWideSubset2$State == "<BR>"], 
      MyUFOCirclesWideSubset2$State[MyUFOCirclesWideSubset2$State == "<BR>"])
# We can obser that where State == "<BR>" the UFO was outside the US
# So we can replace "<BR>" with a character string "NOT US", and later on create a dummy variable
# indicating which if is US or NOT US
MyPatternState <- c("<BR>")
MyReplacementState <- c(rep(x="NOT US", length(MyPatternState)))
MyUFOCirclesWideSubset2$State <- MYgsub(MyPatternState, MyReplacementState, MyUFOCirclesWideSubset2$State)
MyUFOCirclesWideSubset2$State

# Clean the City vector
MyUFOCirclesWideSubset2$City
# City is pretty clean and due to time constraints I will leave it alone
# However, it would be interesting to look at the cities outside the US and take the countries from
# the paranthesis and create a new vector Country

# Clean the Duration vector
MyUFOCirclesWideSubset2$Duration
# Delete everything before "-", "/" and "to" in order to take out ranges
MyPatternDuration1 <- c(".*-", ".*/", ".*to")
MyReplacementDuration1 <- c(rep(x="", length(MyPatternDuration1)))
MyUFOCirclesWideSubset2$Duration <- MYgsub(MyPatternDuration1, MyReplacementDuration1, MyUFOCirclesWideSubset2$Duration)
MyUFOCirclesWideSubset2$Duration

# If we find string "sec" replace with 1, string "min" replace with 60 and string "hour" replace with 360
MyPatternDuration <- c("sec.", "sec", "min.", "min", "Minutes", "mln", "MIN.", "hour", "\\:")
MyReplacementDuration <- c(" 1", " 1", " 60", " 60", " 60", " 60", " 60", " 3600", " 60 ")
MyUFOCirclesWideSubset2$Duration <- MYgsub(MyPatternDuration, MyReplacementDuration, MyUFOCirclesWideSubset2$Duration)
MyUFOCirclesWideSubset2$Duration

# Take out character values and where we have ranges of numbers
MyPatternDuration2 <- c("<BR>", "[[:alpha:]]{1,}", paste0("[[:digit:]]{1}", "/", collapse = "|"),
                        paste0("[[:digit:]]{1}", "-", collapse = "|"), "\\?", "\\+", "\\<", "\\,",
                        "\\&", "\\;", "\\>", paste0("\\(", "\\.", "\\)", collapse = "|"),
                        paste0("\\.", "[[:space:]]", collapse = "|"), "\\~", "[[$:space:]]{2}",
                        "[[$:space:]]{1}", "\\(", "\\)",
                        paste0("\\(", "[[:space:]]","\\)", collapse = "|"), "1--1/2",
                        paste0("[[:space:]]", "\\.", collapse = "|"), "[[:space:]]{3,}",
                        paste0("[[:digit:]]{1}", "[[:space:]]{1}", "-", collapse = "|"),
                        paste0("[[:digit:]]{1}", "[[:space:]]{2}", "-", collapse = "|"))
MyReplacementDuration2 <- c(rep(x="", length(MyPatternDuration2)))
MyUFOCirclesWideSubset2$Duration2 <- MYgsub(MyPatternDuration2, MyReplacementDuration2, MyUFOCirclesWideSubset2$Duration)
MyUFOCirclesWideSubset2$Duration2

# Eliminate spaces at the begining and end of data
require("gdata")
MyUFOCirclesWideSubset2$Duration2 <- trim(s=MyUFOCirclesWideSubset2$Duration2)

# Calculate the product of each line of numbers 
# EG: 3 minutes now is 3 60, so we will do 3*60 = 180 
require(tau)
length(MyUFOCirclesWideSubset2$Duration2) # [1] 4400
MYmultiply <- function(Duration) 
{
  x = as.numeric(length(Duration))
  for(i in 1:length(Duration))
    x[i] <- floor(prod(as.numeric(tokenize(x=Duration[i], lines=FALSE)), na.rm=TRUE))
  x[x == 1] <- NA
  x
}
MyUFOCirclesWideSubset2$DurationSec <- MYmultiply(MyUFOCirclesWideSubset2$Duration2)
MyUFOCirclesWideSubset2$DurationSec
class(MyUFOCirclesWideSubset2$DurationSec) # [1] "numeric"

# We can compare the first 50 entries in the original Duration vector with the numeric DurationSec vector (in seconds)
MyUFOCirclesWideSubset$Duration[1:50]
MyUFOCirclesWideSubset2$DurationSec[1:50] # Pretty nice !!! ... and took FOREVER

# Clean the Shape vector
table(MyUFOCirclesWideSubset2$Shape) 
# circle Circle 
#    8   7688
MyUFOCirclesWideSubset2$Shape[MyUFOCirclesWideSubset2$Shape=="circle"] <- "Circle"
table(MyUFOCirclesWideSubset2$Shape) 
# Circle 
#  7696

# Clean the Summary vector
MyUFOCirclesWideSubset2$Summary
# For now I am going to leave this vector alone

# Let subset the dataset so we have only the variable of interest and spefic dates
# Collect all sightings from that list up to and including September 9, 2013
# Eight features: 
          # Data of sighting, 
          # Time of sighting, 
          # City, 
          # State, 
          # Shape, 
          # Duration, 
          # Summary, 
          # Posted date (when the sighting was posted to the website)
MyUFOCirclesFinal <- subset(x = MyUFOCirclesWideSubset2, 
                            select = c(Date, Time, City, State, Shape, DurationSec, Summary, Posted), 
                            subset = (Date <= "2013-09-09"))
head(MyUFOCirclesFinal) 
dim(MyUFOCirclesFinal) # [1] 7439    8    because we subsetted to only dates before "2013-09-09"
# Looks like its not sorted corectly, so I'll sort it by Date
MyUFOCirclesFinal <- MyUFOCirclesFinal[order(MyUFOCirclesFinal$Date, decreasing=TRUE),] 
dim(MyUFOCirclesFinal) # [1] 7439    8
head(MyUFOCirclesFinal, 10)

# Quality check:
# Excerpt from the website
# 9/9/13 22:20           Vienna	      VA	           Circle	          5 seconds	
# Saw a five gold lit cicular craft moving fastly from rght to left.	9/30/13
MyUFOCirclesFinal[1,1:8]
#     Date       Time      City       State     Shape      DurationSec
# 2013-09-09    22:20     Vienna        VA      Circle           5
# Summary                                                                      Posted
# Saw a five gold lit cicular craft moving fastly from rght to left.         2013-09-30

### NOTES:
# I have subsetted the data to exclude NA's in the time and date (due to characters and ?)
# Also I have subbsetted the data where we had NA's inside the Duration (character strings)
# So my final dataset is smaller than the original one

# Write the MyUFOCirclesFinal as a csv file
write.csv(MyUFOCirclesFinal, file = "UFOCircles.csv", row.names = FALSE)

#####################################################################################################################

##########################
###### UFO Triangle ######
##########################

rm(list=ls())

# Get the page's source - Triangle UFO's
WebPage <- readLines("http://www.nuforc.org/webreports/ndxsTriangle.html")
head(WebPage, 50)
length(WebPage) # [1] 88546

# Pull out the appropriate headers
Header <- WebPage[grep("<TH BGCOLOR=#c0c0c0 BORDERCOLOR=#000000 >", WebPage)]
Header

# Delete unwanted characters in the lines we pulled out above (Header)
MyPatternHeader <- c("<TH BGCOLOR=#c0c0c0 BORDERCOLOR=#000000 >", "</TH>",
                     "<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>")
MyReplacementHeader <- c(rep(x="", length(MyPatternHeader)))
MYgsub <- function(MyPattern, MyReplacement, x) 
{
  for(i in 1:length(MyPattern))
    x <- gsub(MyPattern[i], MyReplacement[i], x)
  x
}
CleanHeader <- MYgsub(MyPatternHeader, MyReplacementHeader, Header)
CleanHeader[1] <- "DateTime"
CleanHeader # [1] "DateTime" "City"     "State"    "Shape"    "Duration" "Summary"  "Posted"  
class(CleanHeader) # [1] "character"

# Pull out the appropriate lines of text
Lines <- WebPage[grep("<TD", WebPage)]
head(Lines, 50)
length(Lines) # [1] 56322

# Delete unwanted characters in the lines we pulled out above (Lines)
MyPatternLines <- c("<TD><FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>", "</TD>",  "</A>",
                    "<.+?>",
                    paste0("<A HREF=", "[[:digit:]]{3}", "/S", "[[:digit:]]{6}", ".html>", collapse = "|"),
                    paste0("<A HREF=", "[[:digit:]]{3}", "/S", "[[:digit:]]{5}", ".html>", collapse = "|"))
MyReplacementLines <- c(rep(x="", length(MyPatternLines)))
CleanLines <- MYgsub(MyPatternLines, MyReplacementLines, Lines)
head(CleanLines, 50)
class(CleanLines) # [1] "character"
length(CleanLines) # [1] 56322

# Create a vector of CleanHeaders the lenght of the CleanLines
length(CleanHeader) # [1] 7
CleanHeaderLong <- rep(x=CleanHeader, length(CleanLines)/length(CleanHeader))
length(CleanHeaderLong) # [1] 56322
length(CleanLines) # [1] 56322

# Combine the two vectors into one dataframe
Number <- c(1:(length(CleanLines)/7))
ID <- rep(Number, each=7)
length(ID) # [1] 56322
MyUFOTriangle <- data.frame(cbind(ID, CleanHeaderLong, CleanLines))
class(MyUFOTriangle) # [1] "data.frame"
names(MyUFOTriangle) # [1] "ID" "CleanHeaderLong" "CleanLines"  
MyUFOTriangle[1:50, 1:3]

# We have a long data frame and we need to transform it into a wide one (decast)
require(reshape2)
MyUFOTriangleWide <- dcast(data = MyUFOTriangle, formula = ID ~ CleanHeaderLong, value.var="CleanLines")
head(MyUFOTriangleWide)
names(MyUFOTriangleWide) # [1] "ID" "City" "DateTime" "Duration" "Posted" "Shape" "State" "Summary"
dim(MyUFOTriangleWide) # [1] 8046    8

# "Date / Time" are combined in one vector and we need to split them up
# First, extract the Date vector
MyUFOTriangleWide$DateRaw <- gsub(pattern=" .*", replacement="", MyUFOTriangleWide$DateTime)
MyUFOTriangleWide$DateRaw

# Clean the Date vector
MyPatternDate <- c(paste0("[[:digit:]]{2}", ":", "[[:digit:]]{2}", collapse = "|"),
                   "1972-1974", "1998", "[[:alpha:]]{5,}")
MyReplacementDate <- c(rep(x="", length(MyPatternDate)))
MyUFOTriangleWide$Date <- MYgsub(MyPatternDate, MyReplacementDate, MyUFOTriangleWide$DateRaw)
MyUFOTriangleWide$Date

# Change the Date vector from character to date format
class(MyUFOTriangleWide$Date) # [1] "character"
MyUFOTriangleWide$Date <- as.Date(x=MyUFOTriangleWide$Date, format="%m/%d/%y")
MyUFOTriangleWide$Date
class(MyUFOTriangleWide$Date) # [1] "Date"

# Second, extract the Time vector 
MyPatternTime <- paste0("[[:digit:]]{1,}", "/", "[[:digit:]]{1,}", "/", "[[:digit:]]{2}", " ", collapse = "|")
MyUFOTriangleWide$TimeRaw <- gsub(pattern = MyPatternTime, replacement="", x = MyUFOTriangleWide$DateTime)
MyUFOTriangleWide$TimeRaw

# Clean the Time vector
MyPatternTime2 <- c(paste0("[[:digit:]]{4}", "-", "[[:digit:]]{4}", collapse = "|"),
                    "1972-1974", "[[:digit:]]{4}", "[[:alpha:]]{1,}", paste0("", ",", "", collapse = "|"),
                    paste0("[[:digit:]]{2}", " ", "[[:digit:]]{2}", collapse = "|"), 
                    paste0(" ", " ", collapse = "|"), " ", "80",
                    paste0("[[:digit:]]{1,}", "/", "[[:digit:]]{1,}", "/", "[[:digit:]]{2}", collapse = "|") )
MyReplacementTime <- c(rep(x="", length(MyPatternTime2)))
MyUFOTriangleWide$Time <- MYgsub(MyPatternTime2, MyReplacementTime, MyUFOTriangleWide$TimeRaw)
MyUFOTriangleWide$Time

# Change the Time vector from character to time format
class(MyUFOTriangleWide$Time) # [1] "character"
MyUFOTriangleWide$Time <- as.POSIXct(x=MyUFOTriangleWide$Time, format="%H:%M")
MyUFOTriangleWide$Time
class(MyUFOTriangleWide$Time) # [1] "POSIXlt" "POSIXt" 

# Subset dataset so we can exclude NA values in Time
dim(MyUFOTriangleWide) # [1] 8046   12
MyUFOTriangleWideSubset <- MyUFOTriangleWide[(!is.na(MyUFOTriangleWide$Time)),]
dim(MyUFOTriangleWideSubset) # [1] 7968  12
MyUFOTriangleWideSubset$Time <- format(x=MyUFOTriangleWideSubset$Time, format="%H:%M")
MyUFOTriangleWideSubset$Time

# Clean the Posted vector
MyPatternPosted <- c("<TD><")
MyReplacementPosted <- c(rep(x="", length(MyPatternPosted)))
MyUFOTriangleWideSubset$Posted <- MYgsub(MyPatternPosted, MyReplacementPosted, MyUFOTriangleWideSubset$Posted)
MyUFOTriangleWideSubset$Posted

# Change the Date vector from character to date format
class(MyUFOTriangleWideSubset$Posted) # [1] "character"
MyUFOTriangleWideSubset$Posted <- as.Date(x=MyUFOTriangleWideSubset$Posted, format="%m/%d/%y")
MyUFOTriangleWideSubset$Posted
class(MyUFOTriangleWideSubset$Posted) # [1] "Date"

# Subset dataset so we can exclude NA values in Posted
dim(MyUFOTriangleWideSubset) # [1] 7968   12
MyUFOTriangleWideSubset2 <- MyUFOTriangleWideSubset[(!is.na(MyUFOTriangleWideSubset$Posted)),]
dim(MyUFOTriangleWideSubset2) # [1] 7968   12

# Clean the State vector
table(MyUFOTriangleWideSubset2$City[MyUFOTriangleWideSubset2$State == ""], 
      MyUFOTriangleWideSubset2$State[MyUFOTriangleWideSubset2$State == ""])
# We can obser that where State == "" the UFO was outside the US
table(MyUFOTriangleWideSubset2$State)

# Clean the City vector
MyUFOTriangleWideSubset2$City
# City is pretty clean and due to time constraints I will leave it alone
# However, it would be interesting to look at the cities outside the US and take the countries from
# the paranthesis and create a new vector Country

# Clean the Duration vector
MyUFOTriangleWideSubset2$Duration
# Delete everything before "-", "/" and "to" in order to take out ranges
MyPatternDuration1 <- c(".*-", ".*/", ".*to")
MyReplacementDuration1 <- c(rep(x="", length(MyPatternDuration1)))
MyUFOTriangleWideSubset2$Duration <- MYgsub(MyPatternDuration1, MyReplacementDuration1, MyUFOTriangleWideSubset2$Duration)
MyUFOTriangleWideSubset2$Duration

# If we find string "sec" replace with 1, string "min" replace with 60 and string "hour" replace with 360
MyPatternDuration <- c("sec.", "sec", "min.", "min", "Minutes", "mln", "MIN.", "hour", "Hour", "HRS", "\\:")
MyReplacementDuration <- c(" 1", " 1", " 60", " 60", " 60", " 60", " 60", " 3600", " 3600", " 3600"," 60 ")
MyUFOTriangleWideSubset2$Duration <- MYgsub(MyPatternDuration, MyReplacementDuration, MyUFOTriangleWideSubset2$Duration)
MyUFOTriangleWideSubset2$Duration

# Take out character values and where we have ranges of numbers
MyPatternDuration2 <- c("<BR>", "[[:alpha:]]{1,}", paste0("[[:digit:]]{1}", "/", collapse = "|"),
                        paste0("[[:digit:]]{1}", "-", collapse = "|"), "\\?", "\\+", "\\<", "\\,",
                        "\\&", "\\;", "\\>", paste0("\\(", "\\.", "\\)", collapse = "|"),
                        paste0("\\.", "[[:space:]]", collapse = "|"), "\\~", "[[$:space:]]{2}",
                        "[[$:space:]]{1}", "\\(", "\\)",
                        paste0("\\(", "[[:space:]]","\\)", collapse = "|"), "1--1/2",
                        paste0("[[:space:]]", "\\.", collapse = "|"), "[[:space:]]{3,}",
                        paste0("[[:digit:]]{1}", "[[:space:]]{1}", "-", collapse = "|"),
                        paste0("[[:digit:]]{1}", "[[:space:]]{2}", "-", collapse = "|"))
MyReplacementDuration2 <- c(rep(x="", length(MyPatternDuration2)))
MyUFOTriangleWideSubset2$Duration2 <- MYgsub(MyPatternDuration2, MyReplacementDuration2, MyUFOTriangleWideSubset2$Duration)
MyUFOTriangleWideSubset2$Duration2

# Eliminate spaces at the begining and end of data
require("gdata")
MyUFOTriangleWideSubset2$Duration2 <- trim(s=MyUFOTriangleWideSubset2$Duration2)

# Calculate the product of each line of numbers 
# EG: 3 minutes now is 3 60, so we will do 3*60 = 180 
require(tau)
length(MyUFOTriangleWideSubset2$Duration2) # [1] 7968
MYmultiply <- function(Duration) 
{
  x = as.numeric(length(Duration))
  for(i in 1:length(Duration))
    x[i] <- floor(prod(as.numeric(tokenize(x=Duration[i], lines=FALSE)), na.rm=TRUE))
  x[x == 1] <- NA
  x
}
MyUFOTriangleWideSubset2$DurationSec <- MYmultiply(MyUFOTriangleWideSubset2$Duration2)
MyUFOTriangleWideSubset2$DurationSec
class(MyUFOTriangleWideSubset2$DurationSec) # [1] "numeric"

# We can compare the first 50 entries in the original Duration vector with the numeric DurationSec vector (in seconds)
MyUFOTriangleWideSubset$Duration[1:50]
MyUFOTriangleWideSubset2$DurationSec[1:50] # Pretty nice !!! ... and took FOREVER

# Clean the Shape vector
table(MyUFOTriangleWideSubset2$Shape) 
# triangle Triangle 
#    18     7950 
MyUFOTriangleWideSubset2$Shape[MyUFOTriangleWideSubset2$Shape=="triangle"] <- "Triangle"
table(MyUFOTriangleWideSubset2$Shape) 
# Triangle 
#   7968 

# Clean the Summary vector
MyUFOTriangleWideSubset2$Summary
# For now I am going to leave this vector alone

# Let subset the dataset so we have only the variable of interest and spefic dates
# Collect all sightings from that list up to and including September 9, 2013
# Eight features: 
# Data of sighting, 
# Time of sighting, 
# City, 
# State, 
# Shape, 
# Duration, 
# Summary, 
# Posted date (when the sighting was posted to the website)
MyUFOTriangleFinal <- subset(x = MyUFOTriangleWideSubset2, 
                            select = c(Date, Time, City, State, Shape, DurationSec, Summary, Posted), 
                            subset = (Date <= "2013-09-09"))
head(MyUFOTriangleFinal) 
dim(MyUFOTriangleFinal) # [1] 7856    8    # because we subsetted to only dates before "2013-09-09"
# Looks like its not sorted corectly, so I'll sort it by Date
MyUFOTriangleFinal <- MyUFOTriangleFinal[order(MyUFOTriangleFinal$Date, decreasing=TRUE),] 
dim(MyUFOTriangleFinal) # [1] 7856    8
head(MyUFOTriangleFinal, 10)

# Quality check:
# Excerpt from the website
# 9/9/13         21:00                Gainesville	                FL	    Triangle	    1 minute	
# Three lights in the sky that didn't look like plane	         9/30/13
MyUFOTriangleFinal[1,1:8]
#     Date       Time                     City                    State     Shape      DurationSec
# 2013-09-09     21:00                 Gainesville                 FL     Triangle          60
# Summary                                                      Posted
# Three lights in the sky that didn't look like plane        2013-09-30

### NOTES:
# I have subsetted the data to exclude NA's in the time and date (due to characters and ?)
# Also I have subbsetted the data where we had NA's inside the Duration (character strings)
# So my final dataset is smaller than the original one

# Write the MyUFOCirclesFinal as a csv file
write.csv(MyUFOTriangleFinal, file = "UFOTriangle.csv", row.names = FALSE)


#####################################################################################################################

##########################
###### UFO Fireball ######
##########################

rm(list=ls())

# Get the page's source - Fireball UFO's
WebPage <- readLines("http://www.nuforc.org/webreports/ndxsFireball.html")
head(WebPage, 50)
length(WebPage) # [1] 66403

# Pull out the appropriate headers
Header <- WebPage[grep("<TH BGCOLOR=#c0c0c0 BORDERCOLOR=#000000 >", WebPage)]
Header

# Delete unwanted characters in the lines we pulled out above (Header)
MyPatternHeader <- c("<TH BGCOLOR=#c0c0c0 BORDERCOLOR=#000000 >", "</TH>",
                     "<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>")
MyReplacementHeader <- c(rep(x="", length(MyPatternHeader)))
MYgsub <- function(MyPattern, MyReplacement, x) 
{
  for(i in 1:length(MyPattern))
    x <- gsub(MyPattern[i], MyReplacement[i], x)
  x
}
CleanHeader <- MYgsub(MyPatternHeader, MyReplacementHeader, Header)
CleanHeader[1] <- "DateTime"
CleanHeader # [1] "DateTime" "City"     "State"    "Shape"    "Duration" "Summary"  "Posted"  
class(CleanHeader) # [1] "character"

# Pull out the appropriate lines of text
Lines <- WebPage[grep("<TD", WebPage)]
head(Lines, 50) 
length(Lines) # [1] 42231

# Delete unwanted characters in the lines we pulled out above (Lines)
MyPatternLines <- c("<TD>", "<TD bgcolor=\"#FFFFCC\" >", 
                    "<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>", "</TD>",  "</A>",
                    paste0("<A HREF=", "[[:digit:]]{3}", "/S", "[[:digit:]]{6}", ".html>", collapse = "|"),
                    paste0("<A HREF=", "[[:digit:]]{3}", "/S", "[[:digit:]]{5}", ".html>", collapse = "|"))
MyReplacementLines <- c(rep(x="", length(MyPatternLines)))
CleanLines <- MYgsub(MyPatternLines, MyReplacementLines, Lines)
head(CleanLines, 50)
class(CleanLines) # [1] "character"
length(CleanLines) # [1] 42231

# Create a vector of CleanHeaders the lenght of the CleanLines
length(CleanHeader) # [1] 7
CleanHeaderLong <- rep(x=CleanHeader, length(CleanLines)/length(CleanHeader))
length(CleanHeaderLong) # [1] 42231
length(CleanLines) # [1] 42231

# Combine the two vectors into one dataframe
Number <- c(1:(length(CleanLines)/7))
ID <- rep(Number, each=7)
length(ID) # [1] 42231
MyUFOFireball <- data.frame(cbind(ID, CleanHeaderLong, CleanLines))
class(MyUFOFireball) # [1] "data.frame"
names(MyUFOFireball) # [1] "ID" "CleanHeaderLong" "CleanLines"  
MyUFOFireball[1:10, 1:3]

# We have a long data frame and we need to transform it into a wide one (decast)
require(reshape2)
MyUFOFireballWide <- dcast(data = MyUFOFireball, formula = ID ~ CleanHeaderLong, value.var="CleanLines")
head(MyUFOFireballWide)
names(MyUFOFireballWide) # [1] "ID" "City" "DateTime" "Duration" "Posted" "Shape" "State" "Summary"
dim(MyUFOFireballWide) # [1] 6033    8

# "Date / Time" are combined in one vector and we need to split them up
# First, extract the Date vector
MyUFOFireballWide$DateRaw <- gsub(pattern=" .*", replacement="", MyUFOFireballWide$DateTime)
MyUFOFireballWide$DateRaw

# Clean the Date vector
MyPatternDate <- c(paste0("[[:digit:]]{2}", ":", "[[:digit:]]{2}", collapse = "|"),
                   "1972-1974", "1998", "[[:alpha:]]{5,}")
MyReplacementDate <- c(rep(x="", length(MyPatternDate)))
MyUFOFireballWide$Date <- MYgsub(MyPatternDate, MyReplacementDate, MyUFOFireballWide$DateRaw)
MyUFOFireballWide$Date

# Change the Date vector from character to date format
class(MyUFOFireballWide$Date) # [1] "character"
MyUFOFireballWide$Date <- as.Date(x=MyUFOFireballWide$Date, format="%m/%d/%y")
MyUFOFireballWide$Date
class(MyUFOFireballWide$Date) # [1] "Date"

# Second, extract the Time vector 
MyPatternTime <- paste0("[[:digit:]]{1,}", "/", "[[:digit:]]{1,}", "/", "[[:digit:]]{2}", " ", collapse = "|")
MyUFOFireballWide$TimeRaw <- gsub(pattern = MyPatternTime, replacement="", x = MyUFOFireballWide$DateTime)
MyUFOFireballWide$TimeRaw

# Clean the Time vector
MyPatternTime2 <- c(paste0("[[:digit:]]{4}", "-", "[[:digit:]]{4}", collapse = "|"),
                    "1972-1974", "[[:digit:]]{4}", "[[:alpha:]]{1,}", paste0("", ",", "", collapse = "|"),
                    paste0("[[:digit:]]{2}", " ", "[[:digit:]]{2}", collapse = "|"), 
                    paste0(" ", " ", collapse = "|"), " ", "80",
                    paste0("[[:digit:]]{1,}", "/", "[[:digit:]]{1,}", "/", "[[:digit:]]{2}", collapse = "|") )
MyReplacementTime <- c(rep(x="", length(MyPatternTime2)))
MyUFOFireballWide$Time <- MYgsub(MyPatternTime2, MyReplacementTime, MyUFOFireballWide$TimeRaw)
MyUFOFireballWide$Time

# Change the Time vector from character to time format
class(MyUFOFireballWide$Time) # [1] "character"
MyUFOFireballWide$Time <- as.POSIXct(x=MyUFOFireballWide$Time, format="%H:%M")
MyUFOFireballWide$Time
class(MyUFOFireballWide$Time) # [1] "POSIXlt" "POSIXt" 

# Subset dataset so we can exclude NA values in Time
dim(MyUFOFireballWide) # [1] 6033   12
MyUFOFireballWideSubset <- MyUFOFireballWide[(!is.na(MyUFOFireballWide$Time)),]
dim(MyUFOFireballWideSubset) # [1] 5980  12
MyUFOFireballWideSubset$Time <- format(x=MyUFOFireballWideSubset$Time, format="%H:%M")
MyUFOFireballWideSubset$Time

# Clean the Posted vector
MyPatternPosted <- c("<TD><")
MyReplacementPosted <- c(rep(x="", length(MyPatternPosted)))
MyUFOFireballWideSubset$Posted <- MYgsub(MyPatternPosted, MyReplacementPosted, MyUFOFireballWideSubset$Posted)
MyUFOFireballWideSubset$Posted

# Change the Date vector from character to date format
class(MyUFOFireballWideSubset$Posted) # [1] "character"
MyUFOFireballWideSubset$Posted <- as.Date(x=MyUFOFireballWideSubset$Posted, format="%m/%d/%y")
MyUFOFireballWideSubset$Posted
class(MyUFOFireballWideSubset$Posted) # [1] "Date"

# Subset dataset so we can exclude NA values in Posted
dim(MyUFOFireballWideSubset) # [1] 5980   12
MyUFOFireballWideSubset2 <- MyUFOFireballWideSubset[(!is.na(MyUFOFireballWideSubset$Posted)),]
dim(MyUFOFireballWideSubset2) # [1] 5980   12

# Clean the State vector
table(MyUFOFireballWideSubset2$City[MyUFOFireballWideSubset2$State == "<BR>"], 
      MyUFOFireballWideSubset2$State[MyUFOFireballWideSubset2$State == "<BR>"])
# We can obser that where State == "<BR>" the UFO was outside the US
# So we can replace "<BR>" with a character string "NOT US", and later on create a dummy variable
# indicating which if is US or NOT US
MyPatternState <- c("<BR>")
MyReplacementState <- c(rep(x="NOT US", length(MyPatternState)))
MyUFOFireballWideSubset2$State <- MYgsub(MyPatternState, MyReplacementState, MyUFOFireballWideSubset2$State)
MyUFOFireballWideSubset2$State

# Clean the City vector
MyUFOFireballWideSubset2$City
# City is pretty clean and due to time constraints I will leave it alone
# However, it would be interesting to look at the cities outside the US and take the countries from
# the paranthesis and create a new vector Country

# Clean the Duration vector
MyUFOFireballWideSubset2$Duration
# Delete everything before "-", "/" and "to" in order to take out ranges
MyPatternDuration1 <- c(".*-", ".*/", ".*to")
MyReplacementDuration1 <- c(rep(x="", length(MyPatternDuration1)))
MyUFOFireballWideSubset2$Duration <- MYgsub(MyPatternDuration1, MyReplacementDuration1, MyUFOFireballWideSubset2$Duration)
MyUFOFireballWideSubset2$Duration

# If we find string "sec" replace with 1, string "min" replace with 60 and string "hour" replace with 360
MyPatternDuration <- c("sec.", "sec", "min.", "min", "Minutes", "mln", "MIN.", "hour", "HRS", "\\:", "thirty")
MyReplacementDuration <- c(" 1", " 1", " 60", " 60", " 60", " 60", " 60", " 3600", " 3600"," 60 ", " 30 ")
MyUFOFireballWideSubset2$Duration <- MYgsub(MyPatternDuration, MyReplacementDuration, MyUFOFireballWideSubset2$Duration)
MyUFOFireballWideSubset2$Duration

# Take out character values and where we have ranges of numbers
MyPatternDuration2 <- c("<BR>", "[[:alpha:]]{1,}", paste0("[[:digit:]]{1}", "/", collapse = "|"),
                        paste0("[[:digit:]]{1}", "-", collapse = "|"), "\\?", "\\+", "\\<", "\\,",
                        "\\&", "\\;", "\\>", paste0("\\(", "\\.", "\\)", collapse = "|"),
                        paste0("\\.", "[[:space:]]", collapse = "|"), "\\~", "[[$:space:]]{2}",
                        "[[$:space:]]{1}",
                        paste0("\\(", "[[:space:]]","\\)", collapse = "|"), "1--1/2",
                        paste0("[[:space:]]", "\\.", collapse = "|"), "[[:space:]]{3,}",
                        paste0("[[:digit:]]{1}", "[[:space:]]{1}", "-", collapse = "|"),
                        paste0("[[:digit:]]{1}", "[[:space:]]{2}", "-", collapse = "|"))
MyReplacementDuration2 <- c(rep(x="", length(MyPatternDuration2)))
MyUFOFireballWideSubset2$Duration2 <- MYgsub(MyPatternDuration2, MyReplacementDuration2, MyUFOFireballWideSubset2$Duration)
MyUFOFireballWideSubset2$Duration2

# Eliminate spaces at the begining and end of data
require("gdata")
MyUFOFireballWideSubset2$Duration2 <- trim(s=MyUFOFireballWideSubset2$Duration2)

# Calculate the product of each line of numbers 
# EG: 3 minutes now is 3 60, so we will do 3*60 = 180 
require(tau)
length(MyUFOFireballWideSubset2$Duration2) # [1] 3046
MYmultiply <- function(Duration) 
{
  x = as.numeric(length(Duration))
  for(i in 1:length(Duration))
    x[i] <- floor(prod(as.numeric(tokenize(x=Duration[i], lines=FALSE)), na.rm=TRUE))
  x[x == 1] <- NA
  x
}
MyUFOFireballWideSubset2$DurationSec <- MYmultiply(MyUFOFireballWideSubset2$Duration2)
MyUFOFireballWideSubset2$DurationSec
class(MyUFOFireballWideSubset2$DurationSec) # [1] "numeric"

# We can compare the first 50 entries in the original Duration vector with the numeric DurationSec vector (in seconds)
MyUFOFireballWideSubset$Duration[1:50]
MyUFOFireballWideSubset2$DurationSec[1:50] # Pretty nice !!! ... and took FOREVER

# Clean the Shape vector
table(MyUFOFireballWideSubset2$Shape) 
# fireball Fireball 
#     2     5978
MyUFOFireballWideSubset2$Shape[MyUFOFireballWideSubset2$Shape=="fireball"] <- "Fireball"
table(MyUFOFireballWideSubset2$Shape) 
# Fireball
#   5980

# Clean the Summary vector
MyUFOFireballWideSubset2$Summary
# For now I am going to leave this vector alone

# Let subset the dataset so we have only the variable of interest and spefic dates
# Collect all sightings from that list up to and including September 9, 2013
# Eight features: 
# Data of sighting, 
# Time of sighting, 
# City, 
# State, 
# Shape, 
# Duration, 
# Summary, 
# Posted date (when the sighting was posted to the website)
MyUFOFireballFinal <- subset(x = MyUFOFireballWideSubset2, 
                             select = c(Date, Time, City, State, Shape, DurationSec, Summary, Posted), 
                             subset = (Date <= "2013-09-09"))
head(MyUFOFireballFinal) 
dim(MyUFOFireballFinal) # [1] 5838    8    # because we subsetted to only dates before "2013-09-09"
# Looks like its not sorted corectly, so I'll sort it by Date
MyUFOFireballFinal <- MyUFOFireballFinal[order(MyUFOFireballFinal$Date, decreasing=TRUE),] 
dim(MyUFOFireballFinal) # [1] 5838    8
head(MyUFOFireballFinal, 50)

# Quality check:
# Excerpt from the website
# 9/9/13        21:00               Milton (Canada)	                ON	   Fireball	    3 minutes	
# Massive Bright Orange Fireball in Sky	                          9/30/13
MyUFOFireballFinal[1,1:8]
#     Date       Time                     City                    State     Shape      DurationSec
# 2013-09-09    21:00                Milton (Canada)                ON     Fireball         180
# Summary                                                           Posted
# Massive Bright Orange Fireball in Sky                           2013-09-30

### NOTES:
# I have subsetted the data to exclude NA's in the time and date (due to characters and ?)
# Also I have subbsetted the data where we had NA's inside the Duration (character strings)
# So my final dataset is smaller than the original one

# Write the MyUFOCirclesFinal as a csv file
write.csv(MyUFOFireballFinal, file = "UFOFireball.csv", row.names = FALSE)

#####################################################################################################################

################################################
###### UFO Circles, Triangle and Fireball ######
################################################

# Read the CSV files that we created above
Circle <- read.csv(file="UFOCircles.csv", header=TRUE, sep=",")
head(Circle)
dim(Circle) # [1] 7439    8
Triangle <- read.csv(file="UFOTriangle.csv", header=TRUE, sep=",")
head(Triangle)
dim(Triangle) # [1] 7856    8
Fireball <- read.csv(file="UFOFireball.csv", header=TRUE, sep=",")
head(Fireball)
dim(Fireball) # [1] 5838    8

# Combine the three data frames into one huge data frame
UFOCircleTriangleFireball <- rbind(Circle, Triangle, Fireball)
head(UFOCircleTriangleFireball)
dim(UFOCircleTriangleFireball) # [1] 21133     8

# Write the UFOCircleTriangleFireball as a csv file
write.csv(UFOCircleTriangleFireball, file = "UFOCircleTriangleFireball.csv", row.names = FALSE)

#####################################################################################################################

############################################################################
###### Q1: Based on your cleaned data, answer the following questions ######
############################################################################

######################## NOTE ############################################## 
# I am using the UFOCircleTriangleFireball.csv data frame to answer this question
# UFOCircleTriangleFireball contains UFO sightings of three shapes: circle, triangle and fireball
# I have subsetted the data to exclude NA's in the time and date (due to characters and ?)
# Also, I have subbsetted the data where we had NA's inside the Duration (character strings)
# So my final dataset is smaller than the original one
############################################################################

# How many UFO sightings in Alaska?
aggregate(formula=Date[State == "AK"]~State[State == "AK"], FUN=length, data=UFOCircleTriangleFireball)
# AK  66

# How many UFO sightings of durations less than 2 minutes in NY? 
aggregate(formula=Date[State == "NY" & DurationSec <= 120]~State[State == "NY" & DurationSec <= 120], 
          FUN=length, data=UFOCircleTriangleFireball)
# NY  400

# What’s the average duration of UFO sightings of fireball?
mean(x=UFOCircleTriangleFireball$DurationSec[UFOCircleTriangleFireball$Shape == "Fireball"], na.rm=TRUE)
# [1] 1796.442
floor(mean(x=UFOCircleTriangleFireball$DurationSec[UFOCircleTriangleFireball$Shape == "Fireball"], na.rm=TRUE))
# [1] 1796

# Which year has the maximum number of sightings?
require(lubridate)
UFOCircleTriangleFireball$Year <- year(x=UFOCircleTriangleFireball$Date) # take year out of the vector Date
table(UFOCircleTriangleFireball$Year)
class(UFOCircleTriangleFireball$Year) # [1] "numeric"
SightingsPerYear <- aggregate(formula=Date~Year, FUN=length, data=UFOCircleTriangleFireball)
SightingsPerYear$Year[SightingsPerYear$Date == max(SightingsPerYear$Date)]
# [1] 2012

# During the December, 2012, how many states witnessed UFO more than 30 times?
UFOCircleTriangleFireball$Month <- month(x=UFOCircleTriangleFireball$Date) # take month out of the vector Date
table(UFOCircleTriangleFireball$Month)
class(UFOCircleTriangleFireball$Month) # [1] "numeric"
SightingsPerState <- aggregate(formula=Date[Year == 2012 & Month == 12]~State[Year == 2012 & Month == 12], 
                               FUN=length, data=UFOCircleTriangleFireball)
length(SightingsPerState$Date[SightingsPerState$Date >= 30])
# [1] 1
SightingsPerState$State[SightingsPerState$Date >= 30]
# [1] FL

#####################################################################################################################
###############################################   Q2    #############################################################
# Download the first dataset from http://acube.di.unipi.it/tmn-dataset/ and structure it. 
# The final structured dataset is expected to be a matrix with five columns corresponding to:
          # date (in the format mmddyyyy), 
          # source, 
          # category, 
          # the number of words (not including punctuation) in the title, 
          # the number of commas in the description. 
#####################################################################################################################

rm(list=ls())

# Read in the txt file
connection <- file("news.txt", open='r')
NewsDataLines <- readLines(connection)
head(NewsDataLines)
close(connection)

# Create the matrix 
NewsData <- matrix(NewsDataLines, ncol=8, byrow=T)
dim(NewsData) # [1] 32604     8
head(NewsData, 10)
class(NewsData) # [1] "matrix"

# Clean the date (in the format mmddyyyy)
class(NewsData[,5]) # [1] "character"
# made the matrix column as a vector so I can manipulate it
a <- as.character(NewsData[,5])
a[1:5]
# [1] "04 May 2011 07:39:03" "20 May 2011 15:13:57" "07 Jun 2011 17:54:54" "06 May 2011 23:36:21" "01 Apr 2011 05:52:29"
b <- as.POSIXct(x=a, format="%d %b %Y %H:%M:%S") # date format
b[1:5]
# [1] "2011-05-04 07:39:03 EDT" "2011-05-20 15:13:57 EDT" "2011-06-07 17:54:54 EDT" "2011-05-06 23:36:21 EDT"
# [5] "2011-04-01 05:52:29 EDT"
# subset the strings and paste them back together in the format mmddyyyy
require(stringr)
MYpaste <- function(String) 
{
  x = as.numeric(length(String))
  for(i in 1:length(String))
    x[i] <- paste0(str_sub(string=b[i], start=6, end=7), str_sub(string=b[i], start=9, end=10), 
                   str_sub(string=b[i], start=1, end=4), collapse = "|")
  x
}
Date <- MYpaste(b)
Date[1:5]
#  [1] "05042011" "05202011" "06072011" "05062011" "04012011" 

# Source
Source <- NewsData[, 6]

# Category
Category <- NewsData[, 7]

# The number of words (not including punctuation) in the title
class(NewsData[,1]) # [1] "character"
# made the matrix column as a vector so I can manipulate it
x <- as.character(NewsData[,1])
x[1:10]
# split the string at each space
y <- str_split(string=x, pattern=" ")
class(y)
# count in how many pieces was the string split up
MYsum <- function(List) 
{
  x = as.numeric(length(List))
  for(i in 1:length(List))
    x[i] <- sum(!is.na(List[[i]])) 
  x
}
NoWordsTitle <- MYsum(y)
NoWordsTitle[1:10]

# The number of commas in the description
class(NewsData[,2]) # [1] "character"
# made the matrix column as a vector so I can manipulate it
t <- as.character(NewsData[,2])
t[1:10]
# take out of each description all the commas
r <- str_extract_all(string=t, pattern=",")
class(r)
# count in how many commas each description has
NoCommasDescription <- MYsum(r)
NoCommasDescription[1:10]

# Replace the date in the matric with the new formated vectors
MyNewsData <- NewsData[,1:5]
MyNewsData[,1] <- Date
MyNewsData[,2] <- Source
MyNewsData[,3] <- Category
MyNewsData[,4] <- NoWordsTitle
MyNewsData[,5] <- NoCommasDescription
head(MyNewsData, 10)
class(MyNewsData) # [1] "matrix"

#####################################################################################################################

#########################################################################################
###### Q2: Based on the structured dataset you get, answer the following questions ######
#########################################################################################

# Which category of news is most popular?
NewsCategory <- as.data.frame(aggregate(formula=MyNewsData[,1] ~ MyNewsData[,3], FUN=length, data=MyNewsData))
colnames(NewsCategory) <- c("Category", "Count")
NewsCategory$Category[NewsCategory$Count == max(NewsCategory$Count)]
# [1] "sport"

# How many business events happened during April(04) to May(05) 2011?
abc <- as.data.frame(MyNewsData)
abc$year <- str_sub(string=abc$V1, start=5, end=8)
abc$month <- str_sub(string=abc$V1, start=1, end=2)
abc2 <- subset(x=abc, year == "2011")
BusinessEvents <- as.data.frame(aggregate(formula=V1[month == "04" | month == "05"] ~ 
                                            V3[month == "04" | month == "05"], FUN=length, data=abc2))
colnames(BusinessEvents) <- c("Category", "Count")
BusinessEvents$Count[BusinessEvents$Category == "business"]
# [1] 3481

# How many news events come from usatoday.com?
Website <- NewsData[,3] # this is the original matrix
Website[1:10]
sum(as.numeric(str_detect(string=Website, pattern="usatoday.com")))
# [1] 10110

# Which newspaper website has the longest title in average?
klm <- as.data.frame(MyNewsData, stringsAsFactors=FALSE)
colnames(klm) <- c("Date", "Source", "Category", "NoWordsTitle", "NoCommasDescription")
head(klm, 10)
table(klm$NoWordsTitle)
klm$NoWordsTitle <- as.numeric(klm$NoWordsTitle)
AvgTitle <- aggregate(formula=klm$NoWordsTitle ~ klm$Source, FUN=mean)
colnames(AvgTitle) <- c("Source", "TitleWordMean")
AvgTitle$Source[AvgTitle$TitleWordMean == max(AvgTitle$TitleWordMean)]
# [1] "nyt"

# Which newspaper website uses least commas (in average) in description?
table(klm$NoCommasDescription)
klm$NoCommasDescription <- as.numeric(klm$NoCommasDescription)
AvgCommas <- aggregate(formula=klm$NoCommasDescription ~ klm$Source, FUN=mean)
colnames(AvgCommas) <- c("Source", "CommasMean")
AvgCommas$Source[AvgCommas$CommasMean == min(AvgCommas$CommasMean)]
# [1] "ut"

#####################################################################################################################
###############################################   Q3    #############################################################
# Pick one dataset from the list here http://www.infochimps.com/datasets and join it with the UFO dataset 
# (joining on date or location is one place to start). 
# Answer the following questions :
      # Describe the dataset and why you chose it
      # Create 5 questions based on the joined data set and answer them (use SQL)
      # Create one interesting visualization and explain what it shows (use R)
#####################################################################################################################

rm(list=ls())

### My UFO Dataset that I cleaned for Q1
UFOCircleTriangleFireball <- read.csv(file="UFOCircleTriangleFireball.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
head(UFOCircleTriangleFireball)
dim(UFOCircleTriangleFireball) # [1] 21133     8
  
######################################################################################################################
### URL: http://www.infochimps.com/datasets/airports-and-their-locations
### AIRPORTS AND THEIR LOCATIONS
### Fields:
# airport code	- String	-  Standard airport code as designated by the International Air Transport Association (IATA)
# latitude -	Float	- Latitude
# longitude	- Float	- Longitude
# airport name	- String	-  Airport name
# city -	String	-  City
# country -	String	-  Country
# country abbreviation	- String	 - Country abbreviation
# country abbreviation	- String	-  Country abbreviation
# gmt offset	- String	- GMT offset
# runway length	- Integer	-  Runway length in feet (ft.) when given
# runway elevation
######################################################################################################################

Airports <- read.table(file="airport_locations.tsv", header=FALSE, sep="\t", na.strings=TRUE, fill=TRUE)
dim(Airports) # [1] 2949   11
head(Airports)

# Based on the documentation I added significant column names
colnames(Airports) <- c("AirportCode", "Latitude", "Longitude", "AirportName", "City", "Country", 
                        "Country", "CountryAbb", "GMToffset", "RunwayLenght", "RunwayElevation")
head(Airports)

# I decided to subset the data to only USA because I intend to merge it with the UFO dataset on state abbreviation
USAirports <- subset(Airports, Country == "United States")
head(USAirports)

# The problem is that in this dataset we do not have the state vector
# What we do have is latitude and longitude for every airport
# Next we will try to take these coordinates and convert them into State names

require(sp)
require(maps)
require(maptools)

# Function that transforms points into State Names
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,proj4string=CRS("+proj=longlat +datum=wgs84"))
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, proj4string=CRS("+proj=longlat +datum=wgs84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# The single argument to this function, pointsDF, is a data.frame in which:
# column 1 contains the longitude in degrees (negative in the US)
# column 2 contains the latitude in degrees
StateNames <- data.frame(USAirports$Longitude, USAirports$Latitude)
StateNames$USAirports.Longitude[1:10]
StateNames$USAirports.Latitude[1:10]

# Here we have craeted a new vector with full names of states
StateFullName <- latlong2state(StateNames)

# before we could extract the abbreviation, we need to capitalize the first letter of the 
StateFullNameCaps <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", StateFullName, perl=TRUE)

# In order to merge we need to transform these full names of states into 2 letters abbreviations
USAirports$StateAbb <- state.abb[match(StateFullNameCaps, state.name)]
head(USAirports)

# Format the state vector from the UFO file so its in the same format as USAirports$StateAbb
class(UFOCircleTriangleFireball$State) # [1] "character"
table(UFOCircleTriangleFireball$State)
UFOCircleTriangleFireball$StateAbb <- state.abb[match(UFOCircleTriangleFireball$State, state.abb)]
UFOCircleTriangleFireball$StateAbb

# Sort the two datasets by StabeAbb
UFOCircleTriangleFireball$StateAbb[1:10] # [1] "VA" "ID" "CA" "NH" NA   NA   "RI" "TX" "WA" "VT"
UFOCircleTriangleFireballSort <- UFOCircleTriangleFireball[order(UFOCircleTriangleFireball$StateAbb, decreasing=FALSE),] 
UFOCircleTriangleFireballSort$StateAbb[1:10] # [1] "AK" "AK" "AK" "AK" "AK" "AK" "AK" "AK" "AK" "AK"
USAirports$StateAbb[1:10] # [1] "WI" NA   "AL" "NM" "OK" NA   NA   NA   NA   NA  
USAirportsSort <- USAirports[order(USAirports$StateAbb, decreasing=FALSE),] 
USAirportsSort$StateAbb[1:10] # [1] "AL" "AL" "AL" "AL" "AR" "AR" "AR" "AR" "AR" "AR"

# Merge the two datasets:
    # UFOCirclesTrianglesFireball
    # USAirports
US_UFO_Airports <- merge(x=UFOCircleTriangleFireballSort, y=USAirportsSort, 
                         by='StateAbb', all.x=FALSE, all.y=FALSE)# by.y=USAirports$StateAbb)
dim(US_UFO_Airports) # [1] 526132     21
head(US_UFO_Airports)

# Write the US_UFO_Airports as a csv file so we can import it into SQL
write.csv(US_UFO_Airports, file = "US_UFO_Airports.csv", row.names = FALSE)

# Create a US map color coded in intensity based on the count of UFO sightings in each State
# Also, add airport locations as a circle, where the size of the circle is proportional to the 
# size of the airport
require(maps)
# choose colors
require(RColorBrewer)
colors <- brewer.pal( n = 7, name = "BuPu") 
# choose data breakdown for the map (use summary to decide for the data is spread)
CountUFOperState <- aggregate(formula=US_UFO_Airports$Date ~ US_UFO_Airports$StateAbb, FUN=length)
colnames(CountUFOperState) <- c("StateAbb", "Count")
summary(CountUFOperState)
CountUFOperState$colorBuckets <- as.numeric(cut(CountUFOperState$Count, c(100, 300, 500, 1000, 1500, 2000, max(CountUFOperState$Count))))
leg.txt <- c("<100", "100-300", "300-500", "500-1000", "1000-1500", "1500-2000", ">2000")
# match the colors to the values in the map using the state abbreviation
data(state.fips)
names(state.fips)
colorsmatched <- CountUFOperState$colorBuckets[match(state.fips$abb, CountUFOperState$StateAbb)]

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 1")
png ("HW1_UFOmap_airports.png", height=900, width=1200)
# plot the map
map(database="state", fill=TRUE, col=colors[colorsmatched], res=0)
# add airports
# points(x=US_UFO_Airports$Longitude, y=US_UFO_Airports$Latitude, col='red', cex=6, type="p", pch=".")
symbols(x = US_UFO_Airports$Longitude, y = US_UFO_Airports$Latitude, lwd = 2,
        circles = US_UFO_Airports$RunwayLenght, inches = 0.4, add = TRUE, fg = "black")
# add title
title(main = "Count of UFO Sightings by state with airport locations
      (proportional to the size of the airport)", cex.main = 2, font.main = 2, col.main = "black")
# add legend
legend("bottomleft", leg.txt, horiz=TRUE, fill=colors)
dev.off()

#####################################################################################################################
################################################ THE END ############################################################
#####################################################################################################################