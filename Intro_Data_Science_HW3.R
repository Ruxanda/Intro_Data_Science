#####################################################################################################################
#####################################################################################################################
### INTRODUCTION TO DATA SCIENCE
### Homework 3
#####################################################################################################################
#####################################################################################################################

rm(list=ls())
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3")

#####################################################################################################################
###############################################   Q1    #############################################################
#####################################################################################################################

### Q1 [Machine Learning] Recall the UFO dataset in Assignment 1.

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 1")
UFO <- read.csv(file="UFOCircleTriangleFireball.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
dim(UFO) # [1] 21133     8
require(ggplot2)
require(ggthemes)
names(UFO) # [1] "Date" "Time" "City" "State" "Shape" "DurationSec" "Summary" "Posted"  

### (a). To better understand this dataset, perform exploratory data analysis on the infochimps UFO data 
# (http://www.infochimps.com/tags/ufo). Specifically, we expect to see the following:

### A boxplot of the duration of UFO sightings of each shape (one boxplot per shape).
summary(UFO$DurationSec)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#         0        20       120     39520       600 648000000      1758
UFO2 <- UFO[!is.na(UFO$DurationSec), ]
dim(UFO2) # [1] 19375     8
summary(UFO2$DurationSec)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#         0        20       120     39520       600   648000000

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_boxplot.png", height=600, width=600)
p <- ggplot(data=UFO2, mapping=aes(x=factor(Shape), y=log(DurationSec), fill=factor(Shape)))
p + geom_boxplot() + ylab("Log of Duration (measured in seconds)") + 
  xlab("Shape of UFO") + ggtitle("Duration of UFO Sightings by Shape of the UFO") +
  theme_economist() + scale_color_economist() + labs(fill="Shape")
dev.off()

#####################################################################################################################

### A time series figure with the number of sightings per year (one line per shape).

require(lubridate)
UFO$Year <- year(x=UFO$Date) # take year out of the vector Date
table(UFO$Year)
# 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 
#  28   16   29   44   47   66   73   50   68   72   72   41   37   43   41   59   57   59   49 
# 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
#  61   65   71   70   62   100  129  172  177  331  602  861  837  849  862  993 1064 1073 1003 
# 2007 2008 2009 2010 2011 2012 2013 
# 1175 1251 1225 1266 1648 2647 1588 
class(UFO$Year) # [1] "numeric"
SightingsPerYear <- aggregate(formula=Date~Year+Shape, FUN=length, data=UFO)
SightingsPerYear

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_timeseries.png", height=600, width=600)
p <- ggplot(data=SightingsPerYear, mapping=aes(x=Year, y=Date, color=factor(Shape)))
p + geom_line() + labs(color="UFO Shape") +
  ylab("UFO Sightings") + xlab("Year") + 
  ggtitle("UFO Sightings over Time") +
  theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### A bar chart for sightings by state.

# Format the state vector from the UFO file so it has Standard State Abreviations 
class(UFO$State) # [1] "character"
table(UFO$State)
#        AB     AK     AL     AR     AZ     BC     Ca     CA     CO     CT     DC     DE     FL     GA     HI     IA 
# 540     79     66    190    176    649    180      1   2460    383    274     21     65   1118    357     84    208 
# ID     IL     IN     KS     KY     LA     MA     MB     MD     ME     MI     MN     MO     MS     MT     NB     NC 
# 127    834    396    170    252    158    336     40    206    177    593    287    431    112    129     25    494 
# ND     NE     NF     NH     NJ     NM NOT US     NS     NT     NV     NY     OH     OK     ON     OR     PA     PE 
# 40    100      2    114    381    150   1177     31      7    181    883    691    186    446    444    703      7 
# PQ     PR     QC     RI     SA     SC     SD     SK     TN     TX     UT     VA     VI     VT     WA     WI     WV 
# 21      4     35     78      7    276     48     28    311    917    203    354      1     68   1074    364    129 
# WY     YK     YT 
# 48      1      5 
UFO$StateAbb <- state.abb[match(UFO$State, state.abb)]
table(UFO$StateAbb)
# AK   AL   AR   AZ   CA   CO   CT   DE   FL   GA   HI   IA   ID   IL   IN   KS   KY   LA   MA   MD   ME   MI   MN   MO   MS 
# 66  190  176  649 2460  383  274   65 1118  357   84  208  127  834  396  170  252  158  336  206  177  593  287  431  112 
# MT   NC   ND   NE   NH   NJ   NM   NV   NY   OH   OK   OR   PA   RI   SC   SD   TN   TX   UT   VA   VT   WA   WI   WV   WY 
# 129  494   40  100  114  381  150  181  883  691  186  444  703   78  276   48  311  917  203  354   68 1074  364  129   48 
SightingsPerState <- aggregate(formula=Date~StateAbb, FUN=length, data=UFO)
head(SightingsPerState, 4)
#   StateAbb Date
# 1       AK   66
# 2       AL  190
# 3       AR  176
# 4       AZ  649

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByState1.png", height=600, width=900)
p <- ggplot(data=SightingsPerState, mapping=aes(x=factor(StateAbb), y=Date))
p + geom_bar(fill='darkblue', stat='identity') +  ylab("UFO Sightings") + xlab("US States") + 
  ggtitle("UFO Sightingsby State") + theme_economist() + scale_color_economist() 
dev.off()

SightingsPerStateSort <- SightingsPerState[order(SightingsPerState$Date, decreasing=FALSE),] 
head(SightingsPerStateSort)
#    StateAbb Date
# 28       ND   40
# 41       SD   48
# 50       WY   48
# 8        DE   65
# 1        AK   66
# 46       VT   68
table(SightingsPerStateSort)

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByState2.png", height=600, width=900)
p <- ggplot(data=SightingsPerStateSort, mapping=aes(x=reorder(StateAbb, Date), y=Date))
p + geom_bar(fill='darkblue', stat='identity') +  ylab("UFO Sightings") + xlab("US States") + 
  ggtitle("UFO Sightings by State") + theme_economist() + scale_color_economist() 
dev.off()

SightingsPerStateShape <- aggregate(formula=Date~StateAbb+Shape, FUN=length, data=UFO)
head(SightingsPerStateShape, 4)
#   StateAbb  Shape Date
# 1       AK Circle   27
# 2       AL Circle   66
# 3       AR Circle   60
# 4       AZ Circle  234

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByState3.png", height=600, width=900)
p <- ggplot(data=SightingsPerStateShape, mapping=aes(x=reorder(StateAbb, Date), y=Date, fill=factor(Shape)))
p + geom_bar(stat='identity') +  ylab("UFO Sightings") + xlab("US States") + labs(fill='UFO Shape') +
  ggtitle("UFO Sightings by State") + theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### A custom plot (any plot is fine) designed by you to explore an interesting aspect of the data.

head(UFO$Time)
# [1] "22:20" "22:00" "23:15" "22:55" "22:30" "22:30"
UFO$TimeNumeric <- as.numeric(gsub(pattern=":[[:digit:]][[:digit:]]", replacement="", x=UFO$Time))
head(UFO$TimeNumeric)
# [1] 22 22 23 22 22 22

SightingsPerTime <- aggregate(formula=Date~TimeNumeric, FUN=length, data=UFO)
head(SightingsPerTime, 4)
#   TimeNumeric Date
# 1           0 1189
# 2           1  896
# 3           2  604
# 4           3  500

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByTime1.png", height=600, width=900)
p <- ggplot(data=SightingsPerTime, mapping=aes(x=TimeNumeric, y=Date))
p + geom_bar(fill='darkblue', stat='identity') +  ylab("UFO Sightings") + xlab("Time of Day") + 
  ggtitle("UFO Sightings by Time of Day") + theme_economist() + scale_color_economist() 
dev.off()

SightingsPerTimeShape <- aggregate(formula=Date~TimeNumeric+Shape, FUN=length, data=UFO)
head(SightingsPerTimeShape, 4)
#   TimeNumeric  Shape Date
# 1           0 Circle  439
# 2           1 Circle  308
# 3           2 Circle  202
# 4           3 Circle  146

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByTime2.png", height=600, width=900)
p <- ggplot(data=SightingsPerTimeShape, mapping=aes(x=TimeNumeric, y=Date, fill=factor(Shape)))
p + geom_bar(stat='identity') +  ylab("UFO Sightings") + xlab("Time of Day") + labs(fill='UFO Shape') +
  ggtitle("UFO Sightings by Time of Day") + theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### (b). You should now move towards identifying interesting insights from the data:

### Normalize the sightings by state population. What do you observe? Anything interesting?

# Scrape date to investigate state population
# http://simple.wikipedia.org/wiki/List_of_U.S._states_by_population

# Get the page's source - US States by Population
WebPage <- readLines("http://simple.wikipedia.org/wiki/List_of_U.S._states_by_population")
head(WebPage, 100)
length(WebPage) # [1] 715

# Pull out the appropriate headers
Header <- WebPage[grep("<th", WebPage)]
Header

# Delete unwanted characters in the lines we pulled out above (Header)
MyPatternHeader <- c("<th width=\"65\">", "<th>", "<br />", "</th>", "<th width=\"150\">",
                     "<th width=\"90\">", "<th width=\"20\">", "</a>", 
                     "<a href=\"/wiki/United_States_House_of_Representatives\" title=\"United States House of Representatives\">",  
                     "<a href=\"/wiki/United_States_Electoral_College\" title=\"United States Electoral College\" class=\"mw-redirect\">")
MyReplacementHeader <- c(rep(x="", length(MyPatternHeader)))
MYgsub <- function(MyPattern, MyReplacement, x) 
{
  for(i in 1:length(MyPattern))
    x <- gsub(MyPattern[i], MyReplacement[i], x)
  x
}
CleanHeader <- MYgsub(MyPatternHeader, MyReplacementHeader, Header)
CleanHeader[4] <- "HouseSeats"
CleanHeader[6] <- "PopulationHouseSeat"
CleanHeader[7] <- "PopulationElectoralVote"
CleanHeader 
# [1] "Rank"                    "State"                   "Population"              "HouseSeats"             
# [5] "Electoral Votes"         "PopulationHouseSeat"     "PopulationElectoralVote"
class(CleanHeader) # [1] "character"

# Pull out the appropriate lines of text
Lines <- WebPage[grep("<td>", WebPage)]
head(Lines, 50)
length(Lines) # [1] 350

# Delete unwanted characters in the lines we pulled out above (Lines)
require(stringr)
Lines <- str_replace(string = Lines, pattern = "<.+?>(.+?)<.+?>", replacement = "\\1")
MyPatternLines <- c("</td>")
MyReplacementLines <- c(rep(x="", length(MyPatternLines)))
CleanLines <- MYgsub(MyPatternLines, MyReplacementLines, Lines)
head(CleanLines, 50)
class(CleanLines) # [1] "character"
length(CleanLines) # [1] 350

# Create a vector of CleanHeaders the lenght of the CleanLines
length(CleanHeader) # [1] 7
CleanHeaderLong <- rep(x=CleanHeader, length(CleanLines)/length(CleanHeader))
length(CleanHeaderLong) # [1] 350
length(CleanLines) # [1] 350

# Combine the two vectors into one dataframe
Number <- c(1:(length(CleanLines)/7))
ID <- rep(Number, each=7)
length(ID) # [1] 350
StatePopulationLong <- data.frame(cbind(ID, CleanHeaderLong, CleanLines))
class(StatePopulationLong) # [1] "data.frame"
names(StatePopulationLong) # [1] "ID" "CleanHeaderLong" "CleanLines"  
StatePopulationLong[1:50, 1:3]

# We have a long data frame and we need to transform it into a wide one (decast)
require(reshape2)
StatePopulation <- dcast(data = StatePopulationLong, formula = ID ~ CleanHeaderLong, value.var="CleanLines")
head(StatePopulation)
names(StatePopulation) 
# [1] "ID"                      "Electoral Votes"         "HouseSeats"              "Population"             
# [5] "PopulationElectoralVote" "PopulationHouseSeat"     "Rank"                    "State" 
dim(StatePopulation) # [1] 50    8

# Take the state names and transform them into State Abbreviations in order to merge to UFO data
StatePopulation$StateAbb <- state.abb[match(StatePopulation$State, state.name)]
head(StatePopulation)
#   ID Electoral Votes HouseSeats Population PopulationElectoralVote PopulationHouseSeat Rank          State StateAbb
# 1  1              55         53 38,041,430                 691,662             717,763   01     California       CA
# 2 10              15         13  9,752,073                 650,138             750,159   10 North Carolina       NC
# 3 11              14         12  8,864,590                 633,185             738,716   11     New Jersey       NJ
# 4 12              13         11  8,185,867                 629,682             744,170   12       Virginia       VA
# 5 13              12         10  6,897,012                 574,751             689,701   13     Washington       WA
# 6 14              11          9  6,646,144                 604,195             738,460   14  Massachusetts       MA
dim(StatePopulation) # [1] 50  9

# Sort the two datasets by StabeAbb
UFO$StateAbb[1:10] # [1] "VA" "ID" "CA" "NH" NA   NA   "RI" "TX" "WA" "VT"
UFOSort <- UFO[order(UFO$StateAbb, decreasing=FALSE),] 
UFOSort$StateAbb[1:10] # [1] "AK" "AK" "AK" "AK" "AK" "AK" "AK" "AK" "AK" "AK"

StatePopulation$StateAbb[1:10] # "CA" "NC" "NJ" "VA" "WA" "MA" "AZ" "IN" "TN" "MO"
StatePopulationSort <- StatePopulation[order(StatePopulation$StateAbb, decreasing=FALSE),] 
StatePopulationSort$StateAbb[1:10] # [1] "AK" "AL" "AR" "AZ" "CA" "CO" "CT" "DE" "FL" "GA"

# Merge the two datasets:
# UFOSort
# StatePopulationSort
UFO3 <- merge(x=UFOSort, y=StatePopulationSort, by='StateAbb', all.x=FALSE, all.y=FALSE)
dim(UFO3) # [1] 18475     19
names(UFO3)
#  [1] "StateAbb"                "Date"                    "Time"                    "City"                   
#  [5] "State.x"                 "Shape"                   "DurationSec"             "Summary"                
#  [9] "Posted"                  "Year"                    "TimeNumeric"             "ID"                     
# [13] "Electoral Votes"         "HouseSeats"              "Population"              "PopulationElectoralVote"
# [17] "PopulationHouseSeat"     "Rank"                    "State.y"             

# Normalize sightings by state population
SightingsByState <- aggregate(formula=Date~StateAbb, FUN=length, data=UFO3)
head(SightingsByState, 4)
#   StateAbb Date
# 1       AK   66
# 2       AL  190
# 3       AR  176
# 4       AZ  649
SightingsByState2 <- merge(x=SightingsByState, y=StatePopulationSort, by='StateAbb', all.x=FALSE, all.y=FALSE)
dim(SightingsByState2) # [1] 50 10
head(SightingsByState2, 4)
#   StateAbb Date ID Electoral Votes HouseSeats Population PopulationElectoralVote PopulationHouseSeat Rank    State
# 1       AK   66 47               3          1    731,449                 243,816             731,449   47   Alaska
# 2       AL  190 23               9          7  4,822,023                 535,780             688,860   23  Alabama
# 3       AR  176 32               6          4  2,949,131                 491,522             737,283   32 Arkansas
# 4       AZ  649 15              11          9  6,553,255                 595,750             728,139   15  Arizona
class(SightingsByState2$Population) # [1] "character"
SightingsByState2$Population <- gsub(pattern=",", replacement="", x=SightingsByState2$Population)
SightingsByState2$Population <- as.numeric(SightingsByState2$Population)
class(SightingsByState2$Population) # [1] "numeric"
require(plyr)
SightingsByState2 <- transform(SightingsByState2, Sightings_Normalized = Date / Population * 100000)
head(SightingsByState2$Sightings_Normalized)

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByStateNormalized1.png", height=600, width=900)
p <- ggplot(data=SightingsByState2, mapping=aes(x=factor(StateAbb), y=Sightings_Normalized))
p + geom_bar(fill='darkblue', stat='identity') +  ylab("UFO Sightings per 100,000") + 
  xlab("US States") + ggtitle("UFO Sightings (normalized by state population)") + 
  theme_economist() + scale_color_economist() 
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByStateNormalized2.png", height=600, width=900)
p <- ggplot(data=SightingsByState2, mapping=aes(x=reorder(StateAbb, Sightings_Normalized), y=Sightings_Normalized))
p + geom_bar(fill='darkblue', stat='identity') +  ylab("UFO Sightings per 100,000") + 
  xlab("US States") + ggtitle("UFO Sightings (normalized by state population)") + 
  theme_economist() + scale_color_economist() 
dev.off()

SightingsByStateShape <- aggregate(formula=Date~StateAbb+Shape, FUN=length, data=UFO3)
head(SightingsByStateShape, 4)
#   StateAbb  Shape Date
# 1       AK Circle   27
# 2       AL Circle   66
# 3       AR Circle   60
# 4       AZ Circle  234
SightingsByStateShape2 <- merge(x=SightingsByStateShape, y=StatePopulationSort, by='StateAbb', all.x=FALSE, all.y=FALSE)
dim(SightingsByStateShape2) # [1] 150 11
head(SightingsByStateShape2, 4)
#   StateAbb    Shape Date ID Electoral Votes HouseSeats Population PopulationElectoralVote PopulationHouseSeat Rank   State
# 1       AK   Circle   27 47               3          1    731,449                 243,816             731,449   47  Alaska
# 2       AK Fireball   14 47               3          1    731,449                 243,816             731,449   47  Alaska
# 3       AK Triangle   25 47               3          1    731,449                 243,816             731,449   47  Alaska
# 4       AL   Circle   66 23               9          7  4,822,023                 535,780             688,860   23 Alabama
class(SightingsByStateShape2$Population) # [1] "character"
SightingsByStateShape2$Population <- gsub(pattern=",", replacement="", x=SightingsByStateShape2$Population)
SightingsByStateShape2$Population <- as.numeric(SightingsByStateShape2$Population)
class(SightingsByStateShape2$Population) # [1] "numeric"
require(plyr)
SightingsByStateShape2 <- transform(SightingsByStateShape2, Sightings_Normalized = Date / Population * 100000)
head(SightingsByStateShape2$Sightings_Normalized)

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByStateNormalized3.png", height=600, width=900)
p <- ggplot(data=SightingsByStateShape2, mapping=aes(x=reorder(x=StateAbb, X=Sightings_Normalized), 
                                                     y=Sightings_Normalized, fill=Shape))
p + geom_bar(stat='identity') +  ylab("UFO Sightings per 100,000") + xlab("US States") + 
  labs(fill='UFO Shape') + ggtitle("UFO Sightings by State (normalized by state population) ") + 
  theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### Visualize the distributions on a map (some options are basemap, D3, or Google Maps API). 
# Do you notice anything peculiar?

# Create a US map color coded in intensity based on the count of UFO sightings in each State
require(maps)
# choose colors
require(RColorBrewer)
colors <- brewer.pal( n = 6, name = "Blues") 
# choose data breakdown for the map (use summary to decide for the data is spread)
summary(SightingsByState2$Date)
SightingsByState2$colorBuckets <- as.numeric(cut(SightingsByState2$Date, 
                                                 c(min(SightingsByState2$Date), 100, 200, 300, 400, 500, max(SightingsByState2$Date))))
leg.txt <- c("Below 100 sightings", "100 to 200 sightings", "200 to 300 sightings", 
             "300 to 400 sightings", "400 to 500 sightings", "Above 500 sightings")
# match the colors to the values in the map using the state abbreviation
data(state.fips)
names(state.fips)
colorsmatched <- SightingsByState2$colorBuckets[match(state.fips$abb, SightingsByState2$StateAbb)]

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_UFOmap_sightings1.png", height=900, width=1200)
map(database="state", fill=TRUE, col=colors[colorsmatched], res=0)
title(main = "Count of UFO Sightings by State", cex.main = 2, font.main = 2, col.main = "black")
legend("bottomleft", leg.txt, horiz=TRUE, fill=colors)
dev.off()

# Create a US map color coded in intensity based on the count of UFO sightings normalized by state population
# choose data breakdown for the map (use summary to decide for the data is spread)
summary(SightingsByState2$Sightings_Normalized)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3.433   5.133   6.017   6.682   7.335  15.570 
SightingsByState2$colorBuckets_Normalized <- as.numeric(cut(SightingsByState2$Sightings_Normalized, 
                                                 c(min(SightingsByState2$Sightings_Normalized), 4, 5, 6, 7, 10, max(SightingsByState2$Sightings_Normalized))))
leg.txt <- c("Below 4 per 100,000", "4 to 5 per 100,000", "5 to 6 per 100,000", "6 to 7 per 100,000", 
             "7 to 10 per 100,000", "Above 10 per 100,000")
# match the colors to the values in the map using the state abbreviation
data(state.fips)
names(state.fips)
colorsmatched <- SightingsByState2$colorBuckets_Normalized[match(state.fips$abb, SightingsByState2$StateAbb)]

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_UFOmap_sightings2.png", height=900, width=1200)
map(database="state", fill=TRUE, col=colors[colorsmatched], res=0)
title(main = "Count of UFO Sightings by State
      (normalized by state population)", cex.main = 2, font.main = 2, col.main = "black")
legend("bottomleft", leg.txt, horiz=TRUE, fill=colors)
dev.off()

#####################################################################################################################

### Now explore the data based on your own intuition. 
# Ask and answer at least two additional questions beyond the basic data analysis we require above.

###############################################################
### Regional divisions used by the United States Census Bureau:
###############################################################

### Region 1 (Northeast)
# Division 1 (New England) Maine, New Hampshire, Vermont, Massachusetts, Rhode Island, Connecticut
# Division 2 (Mid-Atlantic) New York, Pennsylvania, New Jersey
### Region 2 (Midwest) (Prior to June 1984, the Midwest Region was designated as the North Central Region.)
# Division 3 (East North Central) Wisconsin, Michigan, Illinois, Indiana, Ohio
# Division 4 (West North Central) Missouri, North Dakota, South Dakota, Nebraska, Kansas, Minnesota, Iowa
### Region 3 (South)
# Division 5 (South Atlantic) Delaware, Maryland, District of Columbia, Virginia, West Virginia, 
# North Carolina, South Carolina, Georgia, Florida
# Division 6 (East South Central) Kentucky, Tennessee, Mississippi, Alabama
# Division 7 (West South Central) Oklahoma, Texas, Arkansas, Louisiana
### Region 4 (West)
# Division 8 (Mountain) Idaho, Montana, Wyoming, Nevada, Utah, Colorado, Arizona, New Mexico
# Division 9 (Pacific) Alaska, Washington, Oregon, California, Hawaii

names(UFO)
#  [1] "Date"        "Time"        "City"        "State"       "Shape"       "DurationSec" "Summary"     "Posted"     
#  [9] "Year"        "StateAbb"    "TimeNumeric"
table(UFO$StateAbb)
UFO$Region <- rep(NA, length(UFO$Date))
UFO$Region[ UFO$StateAbb == "ME" | UFO$StateAbb == "NH" | UFO$StateAbb == "VT" | UFO$StateAbb == "MA" |
              UFO$StateAbb == "RI" | UFO$StateAbb == "CT" | UFO$StateAbb == "NY" | UFO$StateAbb == "PA" |
              UFO$StateAbb == "NJ" ] <- "Northeast"
UFO$Region[ UFO$StateAbb == "WI" | UFO$StateAbb == "MI" | UFO$StateAbb == "IL" | UFO$StateAbb == "IN" | 
              UFO$StateAbb == "OH" | UFO$StateAbb == "MO" | UFO$StateAbb == "ND" | UFO$StateAbb == "SD" | 
              UFO$StateAbb == "NE" | UFO$StateAbb == "KS" | UFO$StateAbb == "MN" | UFO$StateAbb == "IA" ] <- "Midwest"
UFO$Region[ UFO$StateAbb == "DE" | UFO$StateAbb == "MD" | UFO$StateAbb == "VA" | UFO$StateAbb == "WV" | 
              UFO$StateAbb == "NC" | UFO$StateAbb == "SC" | UFO$StateAbb == "GA" | UFO$StateAbb == "FL" | 
              UFO$StateAbb == "KY" | UFO$StateAbb == "TN" | UFO$StateAbb == "MS" | UFO$StateAbb == "AL" | 
              UFO$StateAbb == "OK" | UFO$StateAbb == "TX" | UFO$StateAbb == "AR" | UFO$StateAbb == "LA" ] <- "South"
UFO$Region[ UFO$StateAbb == "ID" | UFO$StateAbb == "MT" | UFO$StateAbb == "WY" | UFO$StateAbb == "NV" | 
              UFO$StateAbb == "UT" | UFO$StateAbb == "CO" | UFO$StateAbb == "AZ" | UFO$StateAbb == "NM" | 
              UFO$StateAbb == "AK" | UFO$StateAbb == "WA" | UFO$StateAbb == "OR" | UFO$StateAbb == "CA" | 
              UFO$StateAbb == "HI" ] <- "West"
table(UFO$Region)
#   Midwest Northeast     South      West 
#      4162      3014      5301      5998 
UFO$Region <- factor(UFO$Region, levels=c("West", "Midwest", "South", "Northeast"))
levels(UFO$Region) # [1] "West"      "Midwest"   "South"     "Northeast"

SightingsPerTimeRegion <- aggregate(formula=Date~TimeNumeric+Region, FUN=length, data=UFO)
head(SightingsPerTimeRegion, 4)
#     TimeNumeric  Region Date
#   1           0 Midwest  217
#   2           1 Midwest  177
#   3           2 Midwest  101
#   4           3 Midwest   89

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByTimeRegion1.png", height=600, width=900)
p <- ggplot(data=SightingsPerTimeRegion, mapping=aes(x=TimeNumeric, y=Date))
p + geom_bar(fill='darkblue', stat='identity') +  ylab("UFO Sightings") + xlab("Time of Day") + 
  ggtitle("UFO Sightings by Time of Day") + facet_wrap(~Region) +
  theme_economist() + scale_color_economist() 
dev.off()

SightingsPerTimeShapeRegion <- aggregate(formula=Date~TimeNumeric+Shape+Region, FUN=length, data=UFO)
head(SightingsPerTimeShapeRegion, 4)
#   TimeNumeric  Shape  Region Date
# 1           0 Circle Midwest   64
# 2           1 Circle Midwest   57
# 3           2 Circle Midwest   32
# 4           3 Circle Midwest   18

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByTimeRegion2.png", height=600, width=900)
p <- ggplot(data=SightingsPerTimeShapeRegion, mapping=aes(x=TimeNumeric, y=Date, fill=factor(Shape)))
p + geom_bar(stat='identity') +  ylab("UFO Sightings") + xlab("Time of Day") + labs(fill='UFO Shape') +
  ggtitle("UFO Sightings by Time of Day") + facet_wrap(~Region) + 
  theme_economist() + scale_color_economist() 
dev.off()

### Divide the hours of the day into 4 chuncks in order to plot a grid map
table(UFO$TimeNumeric)
#    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23 
# 1189  896  604  500  358  366  280  198  184  260  290  269  282  247  210  244  329  577  962 1565 2257 3428 3266 2372 
# Time of Day: 
# Night (00:00-05:59)
# Morning (06:00-11:59)
# Afternoon (12:00-17:59)
# Evening (18:00-23:59)
UFO$TimeOfDay <- rep(NA, length(UFO$Date))
UFO$TimeOfDay[ UFO$TimeNumeric <= 5 ] <- "Night"
UFO$TimeOfDay[ UFO$TimeNumeric >= 6 & UFO$TimeNumeric <= 11 ] <- "Morning"
UFO$TimeOfDay[ UFO$TimeNumeric >= 12 & UFO$TimeNumeric <= 17 ] <- "Afternoon"
UFO$TimeOfDay[ UFO$TimeNumeric >= 18 & UFO$TimeNumeric <= 23 ] <- "Evening"
table(UFO$TimeOfDay)
# Afternoon   Evening   Morning     Night 
#     1889     13850      1481      3913 
UFO$TimeOfDay <- factor(UFO$TimeOfDay, levels=c("Night", "Morning", "Afternoon", "Evening"))
levels(UFO$TimeOfDay) # [1] "Night"     "Morning"   "Afternoon" "Evening" 

SightingsPerYearTimeOfDayRegionShape <- aggregate(formula=Date~Year+TimeOfDay+Region+Shape, FUN=length, data=UFO)
head(SightingsPerYearTimeOfDayRegionShape, 4)
#   Year TimeOfDay  Region  Shape Date
# 1 1969 Afternoon Midwest Circle    1
# 2 1970 Afternoon Midwest Circle    1
# 3 1972 Afternoon Midwest Circle    1
# 4 1974 Afternoon Midwest Circle    2

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_SightingsByYearTimeOfDayRegionShape.png", height=600, width=900)
p <- ggplot(data=SightingsPerYearTimeOfDayRegionShape, mapping=aes(x=Year, y=Date, fill=factor(Shape)))
p + geom_bar(stat='identity') +  ylab("UFO Sightings") + xlab("Year") + labs(fill='UFO Shape') +
  ggtitle("UFO Sightings by time of day and region
          1969 - 2013") + facet_grid(TimeOfDay~Region) + 
  theme_economist() + scale_color_economist() 
dev.off()

#####################################################################################################################

### (c). Given your understanding of the data, your goal is now to build a classifier to predict the shape of a UFO. 
# You have three target classes: circle, triangle, and fireball.

### Select an evaluation metric
### Try two different classification methods and compare the evaluation metric

##################
### Decision Trees
##################

require(rpart)
names(UFO)
#  [1] "Date"        "Time"        "City"        "State"       "Shape"       "DurationSec" "Summary"     "Posted"     
#  [9] "Year"        "StateAbb"    "TimeNumeric" "Region"      "TimeOfDay" 
class(UFO$Shape) # [1] "character"
UFO_Complete <- UFO[complete.cases(UFO), ]
UFOTree <- rpart(factor(Shape) ~ Year + DurationSec + Region + TimeOfDay, data=UFO_Complete)
UFOTree
#  1) root 16961 10480 Triangle (0.3385414 0.2793467 0.3821119)  
#  2) Year>=2010.5 5098  3085 Fireball (0.3505296 0.3948607 0.2546097)  
#  4) TimeOfDay=Evening 3755  2118 Fireball (0.3328895 0.4359521 0.2311585) *
#  5) TimeOfDay=Night,Morning,Afternoon 1343   806 Circle (0.3998511 0.2799702 0.3201787) *
#  3) Year< 2010.5 11863  6680 Triangle (0.3333895 0.2297058 0.4369047)  
#  6) DurationSec< 8.5 1920  1071 Fireball (0.2791667 0.4421875 0.2786458) *
#  7) DurationSec>=8.5 9943  5295 Triangle (0.3438600 0.1886755 0.4674645)  
# 14) TimeOfDay=Morning,Afternoon 1727   926 Circle (0.4638101 0.1580776 0.3781123) *
# 15) TimeOfDay=Night,Evening 8216  4221 Triangle (0.3186465 0.1951071 0.4862463) *

require(rpart.plot) # for visualising trees
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_UFOTree.png", height=400, width=400)
rpart.plot(UFOTree, extra=4)
dev.off()

#################
### Random Forest
#################

require(randomForest)
UFOFormula <- factor(Shape) ~ Year + Region + TimeOfDay
UFOFormula

require(useful)
require(devtools)
install_github("useful", "jaredlander")

# Build matrices for random forest
UFO_X <- build.x(formula=UFOFormula, data=UFO_Complete)
head(UFO_X)  
dim(UFO_X)
UFO_Y <- build.y(formula=UFOFormula, data=UFO_Complete)
head(UFO_Y)
length(UFO_Y)

# Random forest
UFOForest <- randomForest(x=UFO_X, y=UFO_Y)
UFOForest
# Call:
# randomForest(x = UFO_X, y = UFO_Y) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# OOB estimate of  error rate: 55.62%
# Confusion matrix:
#          Circle Fireball Triangle class.error
# Circle     1120     1181     3441   0.8049460
# Fireball    520     1573     2645   0.6680034
# Triangle    834      812     4835   0.2539732

UFOFormula2 <- factor(Shape) ~ Year + Region + TimeOfDay + DurationSec + TimeNumeric
# Build matrices for random forest
UFO_X2 <- build.x(formula=UFOFormula2, data=UFO_Complete)
head(UFO_X2)  
dim(UFO_X2)
UFO_Y2 <- build.y(formula=UFOFormula2, data=UFO_Complete)
head(UFO_Y2)
length(UFO_Y2)
# Random forest
UFOForest2 <- randomForest(x=UFO_X2, y=UFO_Y2)
UFOForest2
# Call:
# randomForest(x = UFO_X, y = UFO_Y) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# OOB estimate of  error rate: 52.63%
# Confusion matrix:
#          Circle Fireball Triangle class.error
# Circle     1263     1537     2942   0.7800418
# Fireball    613     2345     1780   0.5050654
# Triangle    929     1125     4427   0.3169264

###############
### Elastic Net
###############

require(glmnet)

UFOFormula3 <- factor(Shape) ~ StateAbb + Year + DurationSec + Region + TimeOfDay - 1 # no intercept
UFO_X3 <- build.x(UFOFormula3, data=UFO_Complete, contrasts=FALSE) 
UFO_Y3 <- build.y(UFOFormula3, data=UFO_Complete)

UFONet <- cv.glmnet(x=UFO_X3, y=UFO_Y3, family="multinomial", nfold=5)
UFONet$lambda.min # what minimazes
# [1] 0.006426145

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_UFONet.png", height=600, width=900)
plot(UFONet)
dev.off()

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_UFONet2.png", height=600, width=900)
plot(UFONet$glmnet.fit)
dev.off()

coef(UFONet, s="lambda.min") 
# the variables with numbers get kepted in, and the variables with dots get thrown out for this lambda
# now we can take these var with betas and put them in a regular glm model in order to get confidence interval
###### $Circle
# (Intercept)         3.024003315        
# StateAbbGA          0.001346572        
# StateAbbME          0.011692159         
# StateAbbMO         -0.099766061          
# StateAbbNH          0.087391901         
# StateAbbOR          0.041139189         
# RegionMidwest      -0.110067199      
# RegionNortheast     0.016706867        
# TimeOfDayMorning    0.173433592
# TimeOfDayAfternoon  0.505788531    
###### $Fireball
# (Intercept)        -48.44695128       
# StateAbbAR          -0.09596486
# StateAbbAZ          -0.01604617        
# StateAbbDE          -0.06736177
# StateAbbFL           0.27392033        
# StateAbbMN          -0.02524985      
# StateAbbMT           0.39390255        
# StateAbbTX          -0.00626503      
# StateAbbWA           0.17795640       
# StateAbbWV          -0.04150649        
# Year                 0.02547505       
# RegionSouth         -0.04704362        
# TimeOfDayEvening     0.28412880
###### $Triangle
# (Intercept)        45.422947967        
# StateAbbAR          0.105713926        
# StateAbbCO          0.138952755       
# StateAbbHI         -0.321529772
# StateAbbIA          0.286064635        
# StateAbbMN          0.082138030        
# StateAbbND          0.414022607         
# StateAbbTX          0.062028924       
# StateAbbWA         -0.005235996
# StateAbbWI         -0.111343184        
# Year               -0.021075007         
# RegionNortheast    -0.019890411
# TimeOfDayNight      0.014076417

coef(UFONet, s="lambda.1se") 
# this is the parsemonious model, with one standard deviation from the lambda minimum
# we can take these variables and put them in a regular glm model now
# the parsemonious model (lambda.1se) have less variables than the lambda minimum model
###### $Circle
# (Intercept)         1.84596159        
# RegionMidwest      -0.07793759        
# TimeOfDayMorning    0.09645112
# TimeOfDayAfternoon  0.44011842        
###### $Fireball
# (Intercept)        -43.46859549        
# StateAbbFL           0.13995478        
# StateAbbMT           0.12039225        
# StateAbbWA           0.08308152       
# Year                 0.02241492       
# TimeOfDayEvening     0.25311857
###### $Triangle
# (Intercept)        41.6226339037        
# StateAbbHI         -0.0010279393
# StateAbbIA          0.1125486595         
# StateAbbND          0.0004273764          
# Year               -0.0197667640          

#####################################################################################################################
##################################################  Q2  #############################################################
#####################################################################################################################

### Q2 [Visualization] Your task is to design an infographic for the Kaggle dataset. 
# It can either be static or interactive. While you must use the Kaggle dataset, note that you are free to filter, 
# transform and augment the data as you see fit to highlight the elements that you think are most important in the 
# data set. Feel free to join other datasets to this dataset for your infographic.

# Part of this assignment is for you to improve your skills in a tool and document that process. 
# The choice of tools you use is up to you (e.g., R, d3, Illustrator, Processing), as long as it is more complicated 
# than a spreadsheet (i.e., no Excel). Here are some sample tutorials to get you started:
# 1. D3(Data-Driven Documents):
            # http://christopheviau.com/d3_tutorial/ 
# 2. R:
            # http://flowingdata.com/2012/12/17/getting-started-with-charts-in-r/
            # http://www.r-bloggers.com/basic-introduction-to-ggplot2/
            # http://flowingdata.com/category/tutorials/
            # http://www.r-bloggers.com/using-javascript-visualization-libraries-with-r/
            # http://www.r-bloggers.com/visualize-large-data-sets-with-the-bigvis-package/
# 3. Adobe Illustrator:
            # http://flowingdata.com/2008/12/16/how-to-make-a-graph-in-adobe-illustrator/ 
# 4. Processing:
            # http://processing.org/tutorials/ 
# 5. A general one:
            # http://guides.library.duke.edu/content.php?pid=355157&sid= 2976256

# To document your process:
### (a) Describe the Infographic you would like to create (4-5 bullet points). 
# This link has perspectives from experts in the field about what makes a good info- graphic :
# http://marketingland.com/8-experts-talk-about-making-great-infographics-34958

### (b) Describe the tools you will use (2-3 bullet points).

### (c) Create an infographic. This infographic should effectively communicate this
# data and provide a short write-up (3-4 paragraphs) describing your design.

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Kaggle")
library(gdata)

#############
### Load Data
#############

train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE)
dim(train) # [1] 7395   27
names(train)
#  [1] "url"                            "urlid"                          "boilerplate"                   
#  [4] "alchemy_category"               "alchemy_category_score"         "avglinksize"                   
#  [7] "commonlinkratio_1"              "commonlinkratio_2"              "commonlinkratio_3"             
# [10] "commonlinkratio_4"              "compression_ratio"              "embed_ratio"                   
# [13] "framebased"                     "frameTagRatio"                  "hasDomainLink"                 
# [16] "html_ratio"                     "image_ratio"                    "is_news"                       
# [19] "lengthyLinkDomain"              "linkwordscore"                  "news_front_page"               
# [22] "non_markup_alphanum_characters" "numberOfLinks"                  "numwords_in_url"               
# [25] "parametrizedLinkRatio"          "spelling_errors_ratio"          "label"     
head(train, 3)
head(train$url) # [1] http://www.bloomberg.com/news/2010-12-23/ibm-predicts-holographic-calls-air-breathing-batteries-by-2015.html
head(train$urlid) # [1] 4042 8471 1164 6684 9006 7018
head(train$boilerplate, 1) # [1] {"title":"IBM Sees Holographic Calls Air Breathing Batteries ...............

### Look at the alchemy_category variable to see what is the problem and if we can introduce it in the random forest
head(train$alchemy_category) 
# [1] business   recreation health     health     sports     ?         
# 14 Levels: ? arts_entertainment business computer_internet culture_politics gaming health law_crime recreation ... weather
class(train$alchemy_category) # [1] "factor"
levels(train$alchemy_category)
# [1] "?"                  "arts_entertainment" "business"           "computer_internet"  "culture_politics"   "gaming"            
# [7] "health"             "law_crime"          "recreation"         "religion"           "science_technology" "sports"            
# [13] "unknown"            "weather"
length(train$alchemy_category) # [1] 7395
length(train$alchemy_category[train$alchemy_category == "?"]) # [1] 2342 # urls that are "?"
length(train$url[train$alchemy_category == "unknown"]) # [1] 6 # only six are unknown
levels(train$alchemy_category)[1] <- "unknown" # rename "?" to "unknown"
levels(train$alchemy_category)
# [1] "unknown"            "arts_entertainment" "business"           "computer_internet"  "culture_politics"   "gaming"            
# [7] "health"             "law_crime"          "recreation"         "religion"           "science_technology" "sports"            
# [13] "weather" 

### Look at the alchemy_category variable to see what is the problem and if we can introduce it in the random forest
head(train$alchemy_category_score) 
# [1] 0.789131 0.574147 0.996526 0.801248 0.719157 ?       
# 4806 Levels: ? 0.0708333 0.075 0.0784091 0.0799003 0.0815657 0.0822804 0.0898907 0.0926504 0.0943429 0.0951544 0.0957115 ... 0.999426
class(train$alchemy_category_score) # [1] "factor"
levels(train$alchemy_category_score) # too many need to make them numeric
train$alchemy_category_score[train$alchemy_category == "unknown"] # 0.400001 
levels(train$alchemy_category_score)[1] <- "0.400001" # rename "?" to "0.400001"
levels(train$alchemy_category_score) 
train$alchemy_category_score <- as.numeric(train$alchemy_category_score)
class(train$alchemy_category_score) # [1] "numeric"

### Look at the weather category
train$alchemy_category_score[train$alchemy_category == "weather"] # [1] 2914 1749 2277 1938
train$url[train$alchemy_category == "weather"]
# [1] http://www.thefuckingweather.com/?zipcode=47250                                          
# [2] http://www.buzzfeed.com/urgentgenius/weather-girl-notices-some-arrows-are-pointing-u-28em
# [3] http://www.fusioncharts.com/flex/demos/weather/                                          
# [4] http://usafilm.info/weather-report-on-your-toast/  

### Just for this exercise let's make weather a unknown category so we can introduce it in the random forest
### Later on we should come back and revise this to see what can be done
levels(train$alchemy_category)[13] <- "unknown"
levels(train$alchemy_category)
# [1] "unknown"            "arts_entertainment" "business"           "computer_internet"  "culture_politics"   "gaming"            
# [7] "health"             "law_crime"          "recreation"         "religion"           "science_technology" "sports" 

require(tm)
require(wordcloud)
require(SnowballC)
require(parallel)
require(Rweka)

# Create Corpus for Computer/Internet Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "computer_internet"]))
inspect(MyCorpus)
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudInternet.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
                               use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Arts/Entertainment Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "arts_entertainment"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudEntertainment.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Business Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "business"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudBusiness.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Culture/Politics Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "culture_politics"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudPolitics.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Gaming Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "gaming"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudGaming.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Health Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "health"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudHealth.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Law/Crime Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "law_crime"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudCrime.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Recreation Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "recreation"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudRecreation.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Religion Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "religion"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudReligion.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Science?Technology Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "science_technology"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudTechnology.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Sports Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$alchemy_category == "sports"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics")
png ("HW3_WordCloudSports.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

#############################################################################################################################

#####################################
# Create Word Cloud for Non_Evergreen
#####################################

# Create Corpus for ALL Categories
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$label == 0]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Computer/Internet Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "computer_internet"]))
inspect(MyCorpus)
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Internet.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Arts/Entertainment Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "arts_entertainment"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Entertainment.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Business Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "business"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Business.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Culture/Politics Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "culture_politics"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Politics.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Gaming Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "gaming"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Gaming.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Health Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "health"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Health.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Law/Crime Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "law_crime"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Crime.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Recreation Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "recreation"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Recreation.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Religion Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "religion"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Religion.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Science?Technology Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "science_technology"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Technology.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Sports Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 0 & train$alchemy_category == "sports"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Nonevergreen_Sports.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

#############################################################################################################################

#################################
# Create Word Cloud for Evergreen
#################################

# Create Corpus for ALL Categories
MyCorpus <- Corpus(VectorSource(train$boilerplate[train$label == 1]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=300, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Computer/Internet Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "computer_internet"]))
inspect(MyCorpus)
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Internet.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Arts/Entertainment Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "arts_entertainment"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Entertainment.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Business Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "business"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Business.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Culture/Politics Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "culture_politics"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Politics.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Gaming Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "gaming"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Gaming.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Health Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "health"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Health.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Law/Crime Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "law_crime"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Crime.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Recreation Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "recreation"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Recreation.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Religion Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "religion"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Religion.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Science?Technology Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "science_technology"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Technology.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Create Corpus for Sports Category
MyCorpus <- Corpus(VectorSource(train$boilerplate[ train$label == 1 & train$alchemy_category == "sports"]))
# Clean up corpus
MyCorpus <- tm_map(MyCorpus, stripWhitespace) # strip unnecessary white space
MyCorpus <- tm_map(MyCorpus, tolower) # convert everything to lower case 
MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("english")) # remove English common words like ‘the’ (so-called ‘stopwords’)
MyCorpus <- tm_map(MyCorpus, stemDocument) # carry out text stemming for the final tidy-up
MyCorpus <- tm_map(MyCorpus, removeNumbers) # remove numbers
MyCorpus <- tm_map(MyCorpus, removePunctuation) # remove punctuation
# Create wordcloud
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 3/Graphics/Infograph")
png ("HW3_WordCloud_Evergreen_Sports.png", height=600, width=600)
wordcloud(MyCorpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

#############################################################################################################################

### (d) Describe the process by which you improved (2-3 paragraphs). Includes websites you might have visited, 
# tutorials you might have completed, books you might have read. Be thorough.
# As different visualizations can emphasize different aspects of a data set, you should document what aspects 
# of the data you are attempting to most effec- tively communicate. In short, what story (or stories) are you 
# trying to tell? Just as important, also note which aspects of the data might be obscured or downplayed due 
# to your visualization design.
# In your write-up, you should provide a rigorous rationale for your design decisions. Document the visual 
# encodings you used and why they are appropriate for the data. These decisions include the choice of visualization 
# type, size, color, scale, and other visual elements, as well as the use of sorting or other data transformations. 
# How do these decisions facilitate effective communication?



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
