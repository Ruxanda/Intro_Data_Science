#####################################################################################################################
#####################################################################################################################
### INTRODUCTION TO DATA SCIENCE
### Homework 4
#####################################################################################################################
#####################################################################################################################

rm(list=ls())
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")

#####################################################################################################################
###############################################   Q1    #############################################################
#####################################################################################################################

### Q1[MapReduce/Feature Generation] 
# Feature generation is at the core of building good models. Generating good feature can be computationally expensive. 
# Last week we learned that computation can be sped up by decomposing a problem and applying mapreduce. 
# This question will focus on using mapreduce to generate features, specifically features of the Kaggle competition dataset.


### (a) If you recall there are two phases in a mapreduce process. A map phase where you divide the data into smaller 
# subproblems and get the results, and a reduce phase where you combine the answers from the subproblems to get the answer 
# you were looking for.
# You will need to implement the mapreduce algorithm for computing word count features for the Kaggle dataset. 
# This means that you must segment your data, write a mapper, and write a reducer. 
# You can find out more about mapreduce by looking at the slides Aaron handed out and searching the web for examples.
# You can implement this algorithm in R, or if you wish you can use the Google Compute Engine credit and use Hadoop. 
# Either way you will have to implement a separate mapper and reducer. And for either case you will need to submit your solution.
### A good tutorial on deploying and using Hadoop on Compute Engine is: 
# https: //github.com/GoogleCloudPlatform/solutions-google-compute-engine-cluster-for-hadoop
### There is also a short screencast video that helps through the process of setting up and using the script: 
# http://www.youtube.com/watch?v=se9vV8eIZME

####################
### Read Kaggle data
####################
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Kaggle")
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
dim(train) # [1] 7395   27

##################################
### Clean text - train$boilerplate
##################################
### Bring all words to lower case
train$boilerplate <- tolower(train$boilerplate)
### Take out stopwords
stop_word_list <- read.table("http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")
stop_word_list <- t(stop_word_list)
for(i in 1:length(stop_word_list))
{ train$boilerplate <- gsub(pattern=paste0(" ", stop_word_list[i], " "), replacement=" ", x=train$boilerplate) }
require(tm)
train$boilerplate <- removeWords(x=train$boilerplate, words=stopwords("english"))
### Take out punctuation and title, body and url indicators
train$boilerplate <- gsub(pattern="\"title\"", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="\"body\"", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="\"url\"", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="[[:punct:]]", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern=paste0(" "," "," "), replacement=" ", x=train$boilerplate)
train$boilerplate <- gsub(pattern=paste0(" "," "), replacement=" ", x=train$boilerplate)
### Take out numbers
train$boilerplate <- removeNumbers(x=train$boilerplate)

##############
### MapReduce
##############

keyval <- function(key, value=NULL)
{
  if (missing(value))
    { list(key=NULL, value=key) }
  else
    { list(key=key, value=value) }
}

values <- function(kv) { kv$value }

keys <- function(kv) { kv$key }

theMapper <- function(key, value)
  {
    theSplit <- unlist(strsplit(x=value, split=" "))
    values <- rep(x=1, times=NROW(theSplit))
    keyval(key=theSplit, value=values)
  }

theReducer <- function(key, value) { keyval(key=key, value=sum(value)) }

mapreduce <- function(data, map, reduce)
{
  mappedResults <- map(., data)
  organizedResults <- tapply(X=values(mappedResults), INDEX=keys(mappedResults), FUN=function(x) x)
  theReduced <- mapply(reduce, key=names(organizedResults), value=organizedResults, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  keyval(key=sapply(theReduced, keys), value=sapply(theReduced, values))
}

MyMapReduce <- mapreduce(data=train$boilerplate, map=theMapper, reduce=theReducer)

head(MyMapReduce$key, 100)
# [1] ""              "a"             "aa"            "aaa"           "aaaaaaaaas"    "aaaaaaaabce"   "aaaaaaaaffs"   "aaaaaaaaota"  
# [9] "aaaaaaaapvs"   "aaaaah"        "aaaaahhhhh"    "aaaah"         "aaaahs"        "aaaedf"        "aaas"          "aaat"         
# [17] "aaaw"          "aabdbf"        "aac"           "aad"           "aae"           "aaercmoatdah"  "aafc"          "aafca"        
# [25] "aafeiivoc"     "aahs"          "aaj"           "aalltop"       "aalsr"         "aalve"         "aamoth"        "aams"         
# [33] "aan"           "aanai"         "aano"          "aanvullingen"  "aao"           "aaos"          "aap"           "aapcc"        
# [41] "aapl"          "aar"           "aaradhya"      "aarle"         "aaron"         "aarp"          "aas"           "aasm"         
# [49] "aasvang"       "aatl"          "aax"           "aaxveqwz"      "ab"            "aba"           "aback"         "abacus"       
# [57] "abaete"        "abajo"         "abalone"       "abandon"       "abandoned"     "abandoning"    "abandons"      "abashi"       
# [65] "abashiri"      "abatement"     "abba"          "abbas"         "abbeeda"       "abbey"         "abbeyfly"      "abbi"         
# [73] "abbia"         "abbie"         "abbild"        "abbinata"      "abbo"          "abbotsford"    "abbottabad"    "abbr"         
# [81] "abbrasions"    "abbreviated"   "abbreviation"  "abbreviations" "abby"          "abbys"         "abc"           "abcabc"       
# [89] "abcde"         "abcdeivillage" "abcdes"        "abcnews"       "abcntrack"     "abcornwell"    "abcs"          "abctv"        
# [97] "abdallah"      "abdel"         "abdicate"      "abdomen" 

head(MyMapReduce$value, 100)
#  [1] 5117  145   26   13    1    1    1    1    1    1    1    3    1    1    2    1    1    1    1    1    1    2    2    1    2    2    1
# [28]    1    2    2    1    1    7    2    2    1    1    1    4    4    2    4    1    2   58    5    2    1    1    2    2    1   48    7
# [55]    3    2    3    2    3   16   54    4    1    1    4    4    4    1    1   37    1    1    1    1    1    1    1    2    3   12    1
# [82]    2    1    1   23    1   74    1    2    1    1   12    2    1    3    1    1    5    1   25


### (b) You must also come up with 20 other features based on the Kaggle dataset. 
# These features can be simple (e.g., the length of a sentence), but they need to be distinct from each other. 
# You do not have to use mapreduce to solve this question.

####################
### Read Kaggle data
####################
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Kaggle")
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
dim(train) # [1] 7395   27

##################################
### Clean text - train$boilerplate
##################################
### Bring all words to lower case
train$boilerplate <- tolower(train$boilerplate)
### Take out stopwords
stop_word_list <- read.table("http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")
stop_word_list <- t(stop_word_list)
for(i in 1:length(stop_word_list))
  { train$boilerplate <- gsub(pattern=paste0(" ", stop_word_list[i], " "), replacement=" ", x=train$boilerplate) }
require(tm)
train$boilerplate <- removeWords(x=train$boilerplate, words=stopwords("english"))
### Take out punctuation and title, body and url indicators
train$boilerplate <- gsub(pattern="\"title\"", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="\"body\"", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="\"url\"", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="[[:punct:]]", replacement="", x=train$boilerplate)
### Take out numbers
train$boilerplate <- removeNumbers(x=train$boilerplate)

#########################
### Feature 1: word count 
#########################
word_count <- rep(NA, length(train$boilerplate))
for(i in 1:length(train$boilerplate))
{
  word_count[i] <- length(unlist(strsplit(x=train$boilerplate[i],  split=" ")))
}
head(word_count, 50)
#  [1]  592  254  153  227 1141  193  106  128   93  291   55  146   69   12  343  123   42   64  160  116   96   25  212 4016   13  101  176
# [28]  778   76  118   51  324  193   37  141  255  200   65   89  441  707  230   97  138  124   82  334   29  343  362


###########################
### Feature 2: letter count 
###########################
letter_count <- rep(NA, length(train$boilerplate))
for(i in 1:length(train$boilerplate))
{
  letter_count[i] <- length(unlist(strsplit(x=train$boilerplate[i],  split="")))
}
head(letter_count, 50)
#  [1]  4322  1962  1116  1603  7907  1404   679   965   559  1781   428   964   405    69  2086   847   337   534   966   873   615   157
# [23]  1364 21324    62   663  1273  4962   467   838   337  2102  1317   276   916  1750  1306   438   723  3336  5103  1549   622  1038
# [45]   838   862  2668   210  2134  2394

####################################
### Feature 3: capital letters count 
####################################
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
capital_letters_count <- rep(NA, length(train$boilerplate))
for(i in 1:length(train$boilerplate))
{
  capital_letters_count[i] <- length(unlist(strsplit(x=train$boilerplate[i],  split="[A-Z]")))
}
head(capital_letters_count, 50)
#  [1] 207  60  43  49 512  40  45  36  10  88  14  56  17   4  76  35   9   7  36  28  27   7  45 926   1  45 171 250  60  34  27  70  52  11
# [35]  29  84  41  17   9 106 211  93  26  32  30  23 101  13 121 119

############################
### Feature 4: numbers count 
############################
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
train$boilerplate <- tolower(train$boilerplate)
train$boilerplate <- gsub(pattern="[[a-z]]", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern="[[:punct:]]", replacement="", x=train$boilerplate)
train$boilerplate <- gsub(pattern=" ", replacement="", x=train$boilerplate)
numbers_count <- rep(NA, length(train$boilerplate))
for(i in 1:length(train$boilerplate))
{
  numbers_count[i] <- length(unlist(strsplit(x=train$boilerplate[i],  split="")))
}
head(numbers_count, 50)
#  [1]   73   24   16   10  230    2    7    4   21   51    0    7   13    0   50   32    8    6   28    7   14    5   33 2382    8   10   27
# [28]  102   15   30    0   50    8    0   28   24   25    7    6   25   68   19    6    6   32   14   29    1   72   67

#################################
### Feature 5: contains "Twitter"
#################################
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
has_Twitter <- rep(0, length(train$boilerplate))
Twitter_index <- grep(pattern="Twitter", x=train$boilerplate, ignore.case=TRUE)
has_Twitter[Twitter_index] <- 1
head(has_Twitter, 50)
# [1] 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

##################################
### Feature 6: contains "Facebook"
##################################
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
has_Facebook <- rep(0, length(train$boilerplate))
Facebook_index <- grep(pattern="Facebook", x=train$boilerplate, ignore.case=TRUE)
has_Facebook[Facebook_index] <- 1
head(has_Facebook, 50)
# [1] 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0

###################################
### Feature 7: contains "Bloomberg"
###################################
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
has_Bloomberg <- rep(0, length(train$boilerplate))
Bloomberg_index <- grep(pattern="Bloomberg", x=train$boilerplate, ignore.case=TRUE)
has_Bloomberg[Bloomberg_index] <- 1
head(has_Bloomberg, 50)
# [1] 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

########################################
### Feature 8: ratio numbers to letters
########################################
ratio_numbers_letters <- numbers_count / letter_count
head(ratio_numbers_letters, 50)
#  [1] 0.016890329 0.012232416 0.014336918 0.006238303 0.029088150 0.001424501 0.010309278 0.004145078 0.037567084 0.028635598 0.000000000
# [12] 0.007261411 0.032098765 0.000000000 0.023969319 0.037780401 0.023738872 0.011235955 0.028985507 0.008018328 0.022764228 0.031847134
# [23] 0.024193548 0.111705121 0.129032258 0.015082956 0.021209741 0.020556227 0.032119914 0.035799523 0.000000000 0.023786870 0.006074412
# [34] 0.000000000 0.030567686 0.013714286 0.019142420 0.015981735 0.008298755 0.007494005 0.013325495 0.012265978 0.009646302 0.005780347
# [45] 0.038186158 0.016241299 0.010869565 0.004761905 0.033739456 0.027986633

########################################
### Feature 9: count of punctuation
########################################
train <- read.table(file="train.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
train$boilerplate <- tolower(train$boilerplate)
train$boilerplate <- gsub(pattern="[a-z]", replacement="", x=train$boilerplate)
train$boilerplate <- removeNumbers(x=train$boilerplate)
train$boilerplate <- gsub(pattern=" ", replacement="", x=train$boilerplate)
punctuation_count <- rep(NA, length(train$boilerplate))
for(i in 1:length(train$boilerplate))
{
  punctuation_count[i] <- length(unlist(strsplit(x=train$boilerplate[i],  split="")))
}
head(punctuation_count, 50)
#  [1] 27 34 39 35 51 24 19 36 42 24 33 35 22 19 25 42 31 26 22 30 36 25 36 25 19 27 19 19 19 40 19 31 19 27 23 57 28 29 63 26 27 19 39 35 43
# [46] 52 20 22 37 21

#############################################
### Feature 10: ratio punctuation to letters
#############################################
ratio_punctuation_letters <- punctuation_count / letter_count
head(ratio_punctuation_letters, 50)
#  [1] 0.006247108 0.017329256 0.034946237 0.021834061 0.006449981 0.017094017 0.027982327 0.037305699 0.075134168 0.013475576 0.077102804
# [12] 0.036307054 0.054320988 0.275362319 0.011984660 0.049586777 0.091988131 0.048689139 0.022774327 0.034364261 0.058536585 0.159235669
# [23] 0.026392962 0.001172388 0.306451613 0.040723982 0.014925373 0.003829101 0.040685225 0.047732697 0.056379822 0.014747859 0.014426727
# [34] 0.097826087 0.025109170 0.032571429 0.021439510 0.066210046 0.087136929 0.007793765 0.005291005 0.012265978 0.062700965 0.033718690
# [45] 0.051312649 0.060324826 0.007496252 0.104761905 0.017338332 0.008771930

######################################
### Feature 11: ratio letters to words
######################################
ratio_letters_words <- letter_count / word_count
head(ratio_letters_words, 50)
#  [1]  7.292230  7.712598  7.294118  7.061674  6.928133  7.274611  6.405660  7.539062  5.978495  6.120275  7.781818  6.575342  5.869565
# [14]  5.750000  6.078717  6.886179  8.023810  8.296875  6.037500  7.525862  6.343750  6.280000  6.433962  5.307271  4.769231  6.564356
# [27]  7.232955  6.377892  6.144737  7.101695  6.607843  6.487654  6.823834  7.459459  6.496454  6.843137  6.515000  6.738462  8.123596
# [40]  7.564626  7.217822  6.734783  6.412371  7.521739  6.758065 10.512195  7.988024  7.241379  6.215743  6.613260

#################################################
### Feature 12: ratio capital letters to letters
#################################################
ratio_capital_letters_letters <- capital_letters_count / letter_count
head(ratio_capital_letters_letters, 50)
#  [1] 0.04794997 0.03062787 0.03853047 0.03056769 0.06476913 0.02849003 0.06627393 0.03730570 0.01798561 0.04941044 0.03271028 0.05833333
# [13] 0.04197531 0.05797101 0.03645084 0.04132231 0.02670623 0.01318267 0.03726708 0.03207331 0.04433498 0.04458599 0.03299120 0.04344562
# [25] 0.01612903 0.06787330 0.13432836 0.05038291 0.12847966 0.04057279 0.08011869 0.03330162 0.03948368 0.03985507 0.03165939 0.04813754
# [37] 0.03146585 0.03881279 0.01244813 0.03177458 0.04134823 0.06003873 0.04180064 0.03082852 0.03579952 0.02668213 0.03785607 0.06190476
# [49] 0.05675422 0.04970760

###################################
### Feature 13: has New York Times
###################################
has_NewYorkTimes <- rep(0, length(train$boilerplate))
NewYorkTimes_index <- grep(pattern="New York", x=train$boilerplate, ignore.case=TRUE)
has_NewYorkTimes[NewYorkTimes_index] <- 1
NewYorkTimes_index <- grep(pattern="NY", x=train$boilerplate, ignore.case=TRUE)
has_NewYorkTimes[NewYorkTimes_index] <- 1
NewYorkTimes_index <- grep(pattern="Times", x=train$boilerplate, ignore.case=TRUE)
has_NewYorkTimes[NewYorkTimes_index] <- 1
head(has_NewYorkTimes, 50)
# [1] 1 1 1 0 1 1 0 1 0 1 0 1 0 0 1 1 0 0 0 0 0 0 0 1 0 1 0 1 1 0 0 1 1 0 0 1 1 1 0 1 1 1 0 0 0 1 1 0 1 1

########################
### Feature 14: has CNN
########################
has_CNN <- rep(0, length(train$boilerplate))
CNN_index <- grep(pattern="CNN", x=train$boilerplate, ignore.case=TRUE)
has_CNN[CNN_index] <- 1
head(has_CNN, 50)
# [1] 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0

#####################################################
### Feature 15: ratio punctuation to capital letters
#####################################################
ratio_punctuation_capital_letters <- punctuation_count / capital_letters_count
head(ratio_punctuation_capital_letters, 50)
#  [1]  0.13043478  0.56666667  0.90697674  0.71428571  0.09960938  0.60000000  0.42222222  1.00000000  4.20000000  0.27272727  2.35714286
# [12]  0.62500000  1.29411765  4.75000000  0.32894737  1.20000000  3.44444444  3.71428571  0.61111111  1.07142857  1.33333333  3.57142857
# [23]  0.80000000  0.02699784 19.00000000  0.60000000  0.11111111  0.07600000  0.31666667  1.17647059  0.70370370  0.44285714  0.36538462
# [34]  2.45454545  0.79310345  0.67857143  0.68292683  1.70588235  7.00000000  0.24528302  0.12796209  0.20430108  1.50000000  1.09375000
# [45]  1.43333333  2.26086957  0.19801980  1.69230769  0.30578512  0.17647059

#########################################################
### Feature 16: total letters, punctuation and numbers
#########################################################
total_characters <- letter_count + punctuation_count + numbers_count
head(total_characters, 50)
#  [1]  9685  4835  2474  3840 17738  3479  1513  2383  1379  4289   975  2205  1012   164  4840  1754   821  1137  2304  1908  1233   371
# [23]  2963 43075   151  1622  2602 12455   988  1764   955  4631  3176   581  1908  4189  3224  1059  1491  7759 12091  3764  1382  2178
# [45]  1766  1841  5904   457  4769  6128

########################
### Feature 17: has ORG
########################
has_ORG <- rep(0, length(train$boilerplate))
ORG_index <- grep(pattern="org", x=train$boilerplate, ignore.case=TRUE)
has_ORG[ORG_index] <- 1
head(has_ORG, 50)
# [1] 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 0 0 1

############################
### Feature 18: about food
############################
about_food <- rep(0, length(train$boilerplate))
about_food_index <- grep(pattern="food", x=train$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
about_food_index <- grep(pattern="cake", x=train$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
about_food_index <- grep(pattern="cup", x=train$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
about_food_index <- grep(pattern="recipe", x=train$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
head(about_food, 50)
# [1] 0 0 1 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 1 0 0 1 0 1 1 0 0 1 1 1 1 1 1 1 0 1 0 0 1 0 1 0 0 0 1 0 0

###################################
### Feature 19: has Huffington Post
###################################
has_Huffington <- rep(0, length(train$boilerplate))
Huffington_index <- grep(pattern="Huffington", x=train$boilerplate, ignore.case=TRUE)
has_Huffington[Huffington_index] <- 1
head(has_Huffington, 50)
# [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

###########################
### Feature 20: about games
###########################
about_games <- rep(0, length(train$boilerplate))
about_games_index <- grep(pattern="games", x=train$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
about_games_index <- grep(pattern="console", x=train$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
about_games_index <- grep(pattern="online", x=train$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
about_games_index <- grep(pattern="developer", x=train$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
head(about_games, 50)
# [1] 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0

############################
### Create new train dataset
############################
MyTrain <- cbind(train$label, word_count, letter_count, capital_letters_count, numbers_count, has_Twitter, has_Facebook,
                 has_Bloomberg, ratio_numbers_letters, punctuation_count, ratio_punctuation_letters, ratio_letters_words,
                 ratio_capital_letters_letters, has_NewYorkTimes, has_CNN, ratio_punctuation_capital_letters, 
                 total_characters, has_ORG, about_food, has_Huffington, about_games)
dim(MyTrain) # [1] 7395   21

#######################################
### Repeat process for the test dataset
#######################################
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
test$boilerplate <- tolower(test$boilerplate)
for(i in 1:length(stop_word_list))
{ test$boilerplate <- gsub(pattern=paste0(" ", stop_word_list[i], " "), replacement=" ", x=test$boilerplate) }
test$boilerplate <- removeWords(x=test$boilerplate, words=stopwords("english"))
test$boilerplate <- gsub(pattern="\"title\"", replacement="", x=test$boilerplate)
test$boilerplate <- gsub(pattern="\"body\"", replacement="", x=test$boilerplate)
test$boilerplate <- gsub(pattern="\"url\"", replacement="", x=test$boilerplate)
test$boilerplate <- gsub(pattern="[[:punct:]]", replacement="", x=test$boilerplate)
test$boilerplate <- removeNumbers(x=test$boilerplate)
word_count <- rep(NA, length(test$boilerplate))
for(i in 1:length(test$boilerplate))
{
  word_count[i] <- length(unlist(strsplit(x=test$boilerplate[i],  split=" ")))
}
letter_count <- rep(NA, length(test$boilerplate))
for(i in 1:length(test$boilerplate))
{
  letter_count[i] <- length(unlist(strsplit(x=test$boilerplate[i],  split="")))
}
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
capital_letters_count <- rep(NA, length(test$boilerplate))
for(i in 1:length(test$boilerplate))
{
  capital_letters_count[i] <- length(unlist(strsplit(x=test$boilerplate[i],  split="[A-Z]")))
}
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
test$boilerplate <- tolower(test$boilerplate)
test$boilerplate <- gsub(pattern="[[a-z]]", replacement="", x=test$boilerplate)
test$boilerplate <- gsub(pattern="[[:punct:]]", replacement="", x=test$boilerplate)
test$boilerplate <- gsub(pattern=" ", replacement="", x=test$boilerplate)
numbers_count <- rep(NA, length(test$boilerplate))
for(i in 1:length(test$boilerplate))
{
  numbers_count[i] <- length(unlist(strsplit(x=test$boilerplate[i],  split="")))
}
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
has_Twitter <- rep(0, length(test$boilerplate))
Twitter_index <- grep(pattern="Twitter", x=test$boilerplate, ignore.case=TRUE)
has_Twitter[Twitter_index] <- 1
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
has_Facebook <- rep(0, length(test$boilerplate))
Facebook_index <- grep(pattern="Facebook", x=test$boilerplate, ignore.case=TRUE)
has_Facebook[Facebook_index] <- 1
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
has_Bloomberg <- rep(0, length(test$boilerplate))
Bloomberg_index <- grep(pattern="Bloomberg", x=test$boilerplate, ignore.case=TRUE)
has_Bloomberg[Bloomberg_index] <- 1
ratio_numbers_letters <- numbers_count / letter_count
test <- read.table(file="test.tsv", header=TRUE, sep="\t", na.strings=TRUE, fill=TRUE, stringsAsFactors=FALSE)
test$boilerplate <- tolower(test$boilerplate)
test$boilerplate <- gsub(pattern="[a-z]", replacement="", x=test$boilerplate)
test$boilerplate <- removeNumbers(x=test$boilerplate)
test$boilerplate <- gsub(pattern=" ", replacement="", x=test$boilerplate)
punctuation_count <- rep(NA, length(test$boilerplate))
for(i in 1:length(test$boilerplate))
{
  punctuation_count[i] <- length(unlist(strsplit(x=test$boilerplate[i],  split="")))
}
ratio_punctuation_letters <- punctuation_count / letter_count
ratio_letters_words <- letter_count / word_count
ratio_capital_letters_letters <- capital_letters_count / letter_count
has_NewYorkTimes <- rep(0, length(test$boilerplate))
NewYorkTimes_index <- grep(pattern="New York", x=test$boilerplate, ignore.case=TRUE)
has_NewYorkTimes[NewYorkTimes_index] <- 1
NewYorkTimes_index <- grep(pattern="NY", x=test$boilerplate, ignore.case=TRUE)
has_NewYorkTimes[NewYorkTimes_index] <- 1
NewYorkTimes_index <- grep(pattern="Times", x=test$boilerplate, ignore.case=TRUE)
has_NewYorkTimes[NewYorkTimes_index] <- 1
has_CNN <- rep(0, length(test$boilerplate))
CNN_index <- grep(pattern="CNN", x=test$boilerplate, ignore.case=TRUE)
has_CNN[CNN_index] <- 1
ratio_punctuation_capital_letters <- punctuation_count / capital_letters_count
total_characters <- letter_count + punctuation_count + numbers_count
has_ORG <- rep(0, length(test$boilerplate))
ORG_index <- grep(pattern="org", x=test$boilerplate, ignore.case=TRUE)
has_ORG[ORG_index] <- 1
about_food <- rep(0, length(test$boilerplate))
about_food_index <- grep(pattern="food", x=test$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
about_food_index <- grep(pattern="cake", x=test$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
about_food_index <- grep(pattern="cup", x=test$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
about_food_index <- grep(pattern="recipe", x=test$boilerplate, ignore.case=TRUE)
about_food[about_food_index] <- 1
has_Huffington <- rep(0, length(test$boilerplate))
Huffington_index <- grep(pattern="Huffington", x=test$boilerplate, ignore.case=TRUE)
has_Huffington[Huffington_index] <- 1
about_games <- rep(0, length(test$boilerplate))
about_games_index <- grep(pattern="games", x=test$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
about_games_index <- grep(pattern="console", x=test$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
about_games_index <- grep(pattern="online", x=test$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1
about_games_index <- grep(pattern="developer", x=test$boilerplate, ignore.case=TRUE)
about_games[about_games_index] <- 1

############################
### Create new test dataset
############################
MyTest <- cbind(word_count, letter_count, capital_letters_count, numbers_count, has_Twitter, has_Facebook,
                has_Bloomberg, ratio_numbers_letters, punctuation_count, ratio_punctuation_letters, ratio_letters_words,
                ratio_capital_letters_letters, has_NewYorkTimes, has_CNN, ratio_punctuation_capital_letters, 
                total_characters, has_ORG, about_food, has_Huffington, about_games)
dim(MyTest) # [1] 3171   20


### (c) Use those features to generate a model and submit that model to the Kaggle website

MyTrain <- as.data.frame(MyTrain)
MyTest <- as.data.frame(MyTest)
require(randomForest)
fit <- randomForest(x=MyTrain[ ,-1], y=as.factor(MyTrain$V1), importance=TRUE, proximity=TRUE)
results <- predict(object=fit, newdata=MyTest)

submission <- as.data.frame(cbind(test$urlid, as.numeric(paste(results))))
head(submission)
names(submission) <- c("urlid", "label")
head(submission)
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=submission, file="Kaggle_submissin.csv", sep=",", row.names=FALSE)

# I submitted this result to the Kaggle website
# Your submission scored 0.66877, which is not an improvement of your best score. Keep trying!


#####################################################################################################################
###############################################   Q2    #############################################################
#####################################################################################################################

### Q2[Naive Bayes–NYTimes API] 
# This problem looks at an application of naive Bayes for multiclass text classification. 
# First, you will use the New York Times Developer API to fetch recent articles from several sections of the Times. 
# Then, using the simple Bernoulli model for word presence, you will implement a classifier which, given the text of 
# an article from the New York Times, predicts the section to which the article belongs.
# First, register for a New York Times Developer API key and request access to the Article Search API
# http://developer.nytimes.com/apps/register
# http://developer.nytimes.com/docs/read/article_search_api 
# http://prototype.nytimes.com/gst/apitool/index.html
# After reviewing the API documentation, write code to download the 2,000 most recent articles for each of the 
# Arts, Business, Obituaries, Sports, and World sections. 
# (Hint: Use the nytd section facet to specify article sections.) 
# The developer console may be useful for quickly exploring the API. 
# Your code should save articles from each section to a separate file in a tab-delimited format, where the first column 
# is the article URL, the second is the article title, and the third is the body returned by the API.

###############################################################################################
# An API Query for the Sports section (the first 10 entries)
# http://api.nytimes.com/svc/search/v1/article?format=json&query=nytd_section_facet:[Sports]&
# fields=url,title,body&rank=newest&begin_date=19900101&end_date=20131101&
# api-key=fe6fae1bdc7ec79ac4c32f97bf49f03c:13:68404381&offset=0
###############################################################################################

require(rjson)
require(plyr)
require(tm)

############################################################
### Create function to extract articles from the NYTimes API
############################################################

get.api <- function(section, lastPage=0)
{
  # create a query to extract the NYTimes API's
  baseQuery <- "http://api.nytimes.com/svc/search/v1/article?format=json&query=nytd_section_facet:[%s]&fields=url,title,body&rank=newest&begin_date=19900101&end_date=20131101&api-key=fe6fae1bdc7ec79ac4c32f97bf49f03c:13:68404381&offset=%s"
  # create and empty vector of lists to populate it with the results of the API 
  results <- vector("list", lastPage+1)
  for(i in 0:lastPage)
  {
    # create temporary urls for all the pages we need to scrape (each query has only 10 articles) 
    tempURL <- sprintf(baseQuery, section, i)
    # convert a JSON object into an R object
    tempResult <- fromJSON(file=tempURL)
    # take the results and transform them into a dataframe
    results[[i+1]] <- ldply(tempResult$results, as.data.frame)
  }
  return(ldply(results))
}

### Obtain the 2,000 articles for the Arts Section and save them into a tab delimited file
resultsArts <- get.api(section="Arts", lastPage=199)
dim(resultsArts) # 2000    3
names(resultsArts) # [1] "body" "title"  "url" 
resultsArts_subset <- subset(x=resultsArts, select=c(url, title, body))
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=resultsArts_subset, file="NYTimes_API_Arts.csv", sep="\t", row.names=FALSE, col.names=TRUE)

### Obtain the 2,000 articles for the Business Section and save them into a tab delimited file
resultsBusiness <- get.api(section="Business", lastPage=199)
dim(resultsBusiness) # 2000    3
names(resultsBusiness) # [1] "body" "title"  "url" 
resultsBusiness_subset <- subset(x=resultsBusiness, select=c(url, title, body))
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=resultsBusiness_subset, file="NYTimes_API_Business.csv", sep="\t", row.names=FALSE, col.names=TRUE)

### Obtain the 2,000 articles for the Obituaries Section and save them into a tab delimited file
resultsObituaries <- get.api(section="Obituaries", lastPage=199)
dim(resultsObituaries) # 2000    3
names(resultsObituaries) # [1] "body" "title"  "url" 
resultsObituaries_subset <- subset(x=resultsObituaries, select=c(url, title, body))
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=resultsObituaries_subset, file="NYTimes_API_Obituaries.csv", sep="\t", row.names=FALSE, col.names=TRUE)

### Obtain the 2,000 articles for the Sports Section and save them into a tab delimited file
resultsSports <- get.api(section="Sports", lastPage=199)
dim(resultsSports) # 2000    3
names(resultsSports) # [1] "body" "title"  "url" 
resultsSports_subset <- subset(x=resultsSports, select=c(url, title, body))
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=resultsSports_subset, file="NYTimes_API_Sports.csv", sep="\t", row.names=FALSE, col.names=TRUE)

### Obtain the 2,000 articles for the World Section and save them into a tab delimited file
resultsWorld <- get.api(section="World", lastPage=199)
dim(resultsWorld) # 2000    3
names(resultsWorld) # [1] "body" "title"  "url" 
resultsWorld_subset <- subset(x=resultsWorld, select=c(url, title, body))
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=resultsWorld_subset, file="NYTimes_API_World.csv", sep="\t", row.names=FALSE, col.names=TRUE)

### Create an indicator variable for each section
resultsArts_subset$section       <- rep(0, 2000)
resultsBusiness_subset$section   <- rep(1, 2000)
resultsObituaries_subset$section <- rep(2, 2000)
resultsSports_subset$section     <- rep(3, 2000)
resultsWorld_subset$section      <- rep(4, 2000)

### Bind the five datasets into one dataset (this will include a indicator variable for the section)
resultsAll <- rbind(resultsArts_subset, resultsBusiness_subset, resultsObituaries_subset, 
                    resultsSports_subset, resultsWorld_subset)
dim(resultsAll) # 10000 4
names(resultsAll) # [1] "body" "title"  "url"  "section"
resultsAll$section <- as.factor(resultsAll$section)
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=resultsAll, file="NYTimes_API_All.csv", sep="\t", row.names=FALSE, col.names=TRUE)


# Next, implement code to train a simple Bernoulli naive Bayes model using these articles. 
# We consider documents to belong to one of C categories, where the label of the i-th document is encoded as 
# yi ∈ {0,1,2,...,C − 1}. For example, Arts=0, Business=1, etc. And documents are represented by the sparse binary matrix X, 
# where Xij = 1 indicates that the i-th document contains the j-th word in our dictionary. We train by counting words and 
# documents within classes to estimate θjc and θc:
# where njc is the number of documents of class c containing the j-th word, nc is the number of documents of class c, 
# n is the total number of documents, and the user-selected hyperparameters α and β are pseudocounts that “smooth” the 
# parameter estimates. Given these estimates and the words in a document x, we calculate the log-odds for each class 
# (relative to the base class c = 0) by simply adding the class-specific weights of the words that appear to the corresponding 
# bias term:
# Your code should read the title and body text for each article, remove unwanted characters (e.g., punctuation) and tokenize 
# the article contents into words, filtering out stop words (given in the stopwords) file. The training phase of your code 
# should use these parsed document features to estimate the weights wˆ, taking the hyperparameters α and β as input. 
# The prediction phase should then accept these weights as inputs, along with the features for new examples, and output posterior 
# probabilities for each class.
# Evaluate performance on a randomized 50/50 train/test split of the data, including accuracy and runtime. Comment on the effects 
# of changing α and β. Present your results in a (5-by-5) confusion table showing counts for the actual and predicted sections, 
# where each document is assigned to its most probable section. For each section, report the top 10 most informative words. 
# Also present and comment on the top 10 “most difficult to classify” articles in the test set. Briefly discuss how you expect 
# the learned classifier to generalize to other contexts, e.g. articles from other sources or time periods.
# (Feel free to use packages to train the simple Bernoulli naive Bayes model. However, coding it up by yourself deserves extra credits.)

########################################
# Read in the data that we created above
########################################
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
articles_All <- read.table(file="NYTimes_API_All.csv", sep="\t", stringsAsFactors=FALSE, header=TRUE)
dim(articles_All) # [1] 10000     4
names(articles_All) # [1] "url"     "title"   "body"    "section"
articles_All$section <- as.factor(articles_All$section)
levels(articles_All$section) # [1] "0" "1" "2" "3" "4"

##########################################################################
# Read in Stopwords
# I did a google search for a list of stop words list 
# http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
##########################################################################
stop_word_list <- read.table("http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")
head(stop_word_list)
#          V1
# 1         a
# 2       a's
# 3      able
# 4     about
# 5     above
# 6 according
stop_word_list <- t(stop_word_list)

##############################
### Clean article text - body 
##############################
articles_All$body[1]
# [1] "Five of James Brown's children are seeking to invalidate his will on grounds that former advisers 
# seeking to profit unduly influenced Brown, below, to create charitable trusts, The Associated Press 
# reported, citing court documents. The children of Brown, the soul singer, were largely excluded from 
# the financial part of the will, which left the bulk"

### Bring all words to lower case
articles_All$body <- tolower(articles_All$body)
articles_All$body[1]
# [1] "five of james brown's children are seeking to invalidate his will on grounds that former advisers 
# seeking to profit unduly influenced brown, below, to create charitable trusts, the associated press 
# reported, citing court documents. the children of brown, the soul singer, were largely excluded from 
# the financial part of the will, which left the bulk"

### Take out stopwords
for(i in 1:length(stop_word_list)){
  articles_All$body <- gsub(pattern=paste0(" ", stop_word_list[i], " "), replacement=" ", x=articles_All$body)
}
articles_All$body[1]
# [1] "five james brown's children seeking invalidate grounds advisers seeking profit unduly influenced 
# brown, below, create charitable trusts, press reported, citing court documents. children brown, soul 
# singer, largely excluded financial part will, left bulk"
articles_All$body <- removeWords(x=articles_All$body, words=stopwords("english"))
articles_All$body[1]
# [1] "five james brown's children seeking invalidate grounds advisers seeking profit unduly influenced 
# brown, , create charitable trusts, press reported, citing court documents. children brown, soul 
# singer, largely excluded financial part will, left bulk"

### Take out punctuation
articles_All$body <- gsub(pattern="[[:punct:]]", replacement="", x=articles_All$body)
articles_All$body[1]
# [1] "five james browns children seeking invalidate grounds advisers seeking profit unduly influenced 
# brown  create charitable trusts press reported citing court documents children brown soul 
# singer largely excluded financial part will left bulk"

### Take out numbers
articles_All$body <- removeNumbers(x=articles_All$body)
articles_All$body[1]
# [1] "five james browns children seeking invalidate grounds advisers seeking profit unduly influenced 
# brown  create charitable trusts press reported citing court documents children brown soul 
# singer largely excluded financial part will left bulk"


##############################
### Clean article text - title
##############################
articles_All$title[1]
# [1] "ARTS, BRIEFLY COMPILED BY LAWRENCE VAN GELDER | CHILDREN CONTEST JAMES BROWN WILL; 
# Children Contest James Brown Will"

### Bring all words to lower case
articles_All$title <- tolower(articles_All$title)
articles_All$title[1]
# [1] "arts, briefly compiled by lawrence van gelder | children contest james brown will; 
# children contest james brown will"

### Take out stopwords
for(i in 1:length(stop_word_list)){
  articles_All$title <- gsub(pattern=paste0(" ", stop_word_list[i], " "), replacement=" ", x=articles_All$title)
}
articles_All$title[1]
# [1] "arts, briefly compiled lawrence van gelder | children contest james brown will; 
# children contest james brown will"
articles_All$title <- removeWords(x=articles_All$title, words=stopwords("english"))
articles_All$title[1]
# [1] "arts, briefly compiled lawrence van gelder | children contest james brown will; 
# children contest james brown will"

### Take out punctuation
articles_All$title <- gsub(pattern="[[:punct:]]", replacement="", x=articles_All$title)
articles_All$title[1]
# [1] "arts briefly compiled lawrence van gelder  children contest james brown will 
# children contest james brown will"

### Take out numbers
articles_All$title <- removeNumbers(x=articles_All$title)
articles_All$title[1]
# [1] "arts briefly compiled lawrence van gelder  children contest james brown will 
# children contest james brown will"

##########################################################################################
# Sparse matrix of the word counts in each article
# Documents are represented by the sparse binary matrix X, 
# where Xij = 1 indicates that the i-th document contains the j-th word in our dictionary
##########################################################################################
require(tm)
articles_All_Corpus <- Corpus(VectorSource(paste(articles_All$body, articles_All$Title, sep=" ")))
articles_All_DocumentTermMatrix <- DocumentTermMatrix(articles_All_Corpus)
articles_All_DocumentTermMatrix <- weightBin(articles_All_DocumentTermMatrix) 
articles_All_RemoveSparseTerms <- removeSparseTerms(x=articles_All_DocumentTermMatrix, sparse=0.999)
articles_All_Matrix <- as.matrix(articles_All_RemoveSparseTerms)
articles_All_DataFrame <- as.data.frame(articles_All_Matrix)
dim(articles_All_DataFrame) # [1] 10000  4533
head(names(articles_All_DataFrame))
# [1] "aaron"     "abandoned" "abbas"     "abc"       "abcs"      "ability"  
tail(names(articles_All_DataFrame))
# [1] "youth"   "youtube" "zach"    "zealand" "zone"    "zurich" 
articles_All_DataFrame$MySectionColumn <- articles_All$section

###############################################
# Randomized 50/50 train/test split of the data
###############################################
set.seed(12345)
# MySample <- sample(x=10000, size=5000)
MySample <- c(sample(x=1:2000, size=1000), sample(x=2001:4000, size=1000), sample(x=4001:6000, size=1000), 
              sample(x=6001:8000, size=1000), sample(x=8001:10000, size=1000))
train <- articles_All_DataFrame[ MySample, ] 
test  <- articles_All_DataFrame[-MySample, ]
dim(train) # [1] 5000 4534
dim(test) # [1] 5000 4534
train$MySectionColumn <- as.factor(train$MySectionColumn)
levels(train$MySectionColumn) # [1] "0" "1" "2" "3" "4"
# Make a vector with the actual section categories of the test data
test_ActualSections <- test$MySectionColumn
# Remove MySectionColumn vector from the test dataset
test <- test[ ,!(names(test) %in% "MySectionColumn")]
names(test)

#######################################################################################################################
# Train Naive Bayes Classifier
# We train by counting words and documents within classes to estimate θjc and θc
# njc is the number of documents of class c containing the j-th word 
# nc is the number of documents of class c
# n is the total number of documents
# the user-selected hyperparameters α and β are pseudocounts that “smooth” the parameter estimates 
# Given these estimates and the words in a document x, we calculate the log-odds for each class 
# (relative to the base class c = 0) by simply adding the class-specific weights of the words that appear to the 
# corresponding bias term
########################################################################################################################

# Function that calculates gives us a dataframe with only the documents of class c (for each section of articles)
NaiveMySection <- function(dataset, section)
{ holder <- dataset[which(dataset$MySectionColumn == section), 1:(ncol(dataset)-1)] }
# Function that calculates gives us a dataframe with all the documents that are NOT class c (for each section of articles)
NaiveOtherSection <- function(dataset, section)
{ holder <- dataset[which(dataset$MySectionColumn != section), 1:(ncol(dataset)-1)] }

# For the Arts Section (MySectionColumn == 0)
train_Arts <- NaiveMySection(dataset=train, section="0")
dim(train_Arts) # [1] 1000 4533
prior_Arts <- nrow(train_Arts) / nrow(train)
prior_Arts # [1] 0.2
counts_Arts <- colSums(train_Arts)
other_Arts <- NaiveOtherSection(dataset=train, section="0")
counts_other_Arts <- colSums(other_Arts)

# For the Business Section (MySectionColumn == 1)
train_Business <- NaiveMySection(dataset=train, section="1")
dim(train_Business) # [1] 1000 4533
prior_Business <- nrow(train_Business) / nrow(train)
prior_Business # [1] 0.2
counts_Business <- colSums(train_Business)
other_Business <- NaiveOtherSection(dataset=train, section="1")
counts_other_Business <- colSums(other_Business)

# For the Obituaties Section (MySectionColumn == 2)
train_Obituaries <- NaiveMySection(dataset=train, section="2")
dim(train_Obituaries) # [1] 1000 4533
prior_Obituaries <- nrow(train_Obituaries) / nrow(train)
prior_Obituaries # [1] 0.2
counts_Obituaries <- colSums(train_Obituaries)
other_Obituaries <- NaiveOtherSection(dataset=train, section="2")
counts_other_Obituaries <- colSums(other_Obituaries)

# For the Sports Section (MySectionColumn == 3)
train_Sports <- NaiveMySection(dataset=train, section="3")
dim(train_Sports) # [1] 1000 4533
prior_Sports <- nrow(train_Sports) / nrow(train)
prior_Sports # [1] 0.2
counts_Sports <- colSums(train_Sports)
other_Sports <- NaiveOtherSection(dataset=train, section="3")
counts_other_Sports <- colSums(other_Sports)

# For the Worls Section (MySectionColumn == 4)
train_World <- NaiveMySection(dataset=train, section="4")
dim(train_World) # [1] 1000 4533
prior_World <- nrow(train_World) / nrow(train)
prior_World # [1] 0.2
counts_World <- colSums(train_World)
other_World <- NaiveOtherSection(dataset=train, section="4")
counts_other_World <- colSums(other_World)

# Combine the results into dataframes
prior_All <- c(prior_Arts, prior_Business, prior_Obituaries, prior_Sports, prior_World)
sums_All <- c(nrow(train_Arts), nrow(train_Business), nrow(train_Obituaries), nrow(train_Sports), nrow(train_World))
counts_All <- data.frame(counts_Arts, counts_Business, counts_Obituaries, counts_Sports, counts_World)
other_sums_All <- c(nrow(other_Arts),nrow(other_Business),nrow(other_Obituaries),nrow(other_Sports),nrow(other_World))
other_counts_All <- data.frame(counts_other_Arts,counts_other_Business,counts_other_Obituaries,counts_other_Sports,counts_other_World)

########################################################
# Make function to add varing alphas and betas 
# θjc = (counts + Alpha - 1) / (sums + alpha + beta - 2)
########################################################
probabilities_All <- function(Alpha, Beta)
{ probabilities_All <- (counts_All + Alpha - 1) / (sums_All + Alpha + Beta - 2) }

other_probabilities_All <- function(Alpha, Beta)
{ other_probabititiess_All <- (other_counts_All + Alpha - 1) / (other_sums_All + Alpha + Beta - 2) }

# Probabilities with Alpha = 20 and Beta = 20
probabilities_All_alpha20_beta20 <- probabilities_All(Alpha=20, Beta=20)
other_probabilities_All_alpha20_beta20 <- other_probabilities_All(Alpha=20, Beta=20)

# Probabilities with Alpha = 10 and Beta = 10
probabilities_All_alpha10_beta10 <- probabilities_All(Alpha=10, Beta=10)
other_probabilities_All_alpha10_beta10 <- other_probabilities_All(Alpha=10, Beta=10)

# Probabilities with Alpha = 7 and Beta = 3
probabilities_All_alpha7_beta3 <- probabilities_All(Alpha=7, Beta=3)
other_probabilities_All_alpha7_beta3 <- other_probabilities_All(Alpha=7, Beta=3)

# Probabilities with Alpha = 10 and Beta = 1
probabilities_All_alpha10_beta1 <- probabilities_All(Alpha=10, Beta=1)
other_probabilities_All_alpha10_beta1 <- other_probabilities_All(Alpha=10, Beta=1)

# Probabilities with Alpha = 5 and Beta = 2
probabilities_All_alpha5_beta2 <- probabilities_All(Alpha=5, Beta=2)
other_probabilities_All_alpha5_beta2 <- other_probabilities_All(Alpha=5, Beta=2)

# Probabilities with Alpha = 2 and Beta = 5
probabilities_All_alpha2_beta5 <- probabilities_All(Alpha=2, Beta=5)
other_probabilities_All_alpha2_beta5 <- other_probabilities_All(Alpha=2, Beta=5)

################
# Calculate Wjc
################
Wjc <- function(θjc, θjo)
  { as.matrix( Wjc <- log( (θjc * (1 - θjo)) / (θjo * (1 - θjc))) ) }

# Use above function to calculate Wjc for each section and for varing alphas and betas

# Arts Section:
Wjc_Arts_alpha20_beta20 <- Wjc(θjc = probabilities_All_alpha20_beta20$counts_Arts, 
                               θjo = other_probabilities_All_alpha20_beta20$counts_other_Arts) 

Wjc_Arts_alpha10_beta10 <- Wjc(θjc = probabilities_All_alpha10_beta10$counts_Arts, 
                               θjo = other_probabilities_All_alpha10_beta10$counts_other_Arts) 

Wjc_Arts_alpha7_beta3 <- Wjc(θjc = probabilities_All_alpha7_beta3$counts_Arts, 
                             θjo = other_probabilities_All_alpha7_beta3$counts_other_Arts) 

Wjc_Arts_alpha10_beta1 <- Wjc(θjc = probabilities_All_alpha10_beta1$counts_Arts, 
                              θjo = other_probabilities_All_alpha10_beta1$counts_other_Arts) 

Wjc_Arts_alpha5_beta2 <- Wjc(θjc = probabilities_All_alpha5_beta2$counts_Arts, 
                             θjo = other_probabilities_All_alpha5_beta2$counts_other_Arts) 

Wjc_Arts_alpha2_beta5 <- Wjc(θjc = probabilities_All_alpha2_beta5$counts_Arts, 
                             θjo = other_probabilities_All_alpha2_beta5$counts_other_Arts) 

# Business Section:
Wjc_Business_alpha20_beta20 <- Wjc(θjc = probabilities_All_alpha20_beta20$counts_Business, 
                                   θjo = other_probabilities_All_alpha20_beta20$counts_other_Business) 

Wjc_Business_alpha10_beta10 <- Wjc(θjc = probabilities_All_alpha10_beta10$counts_Business, 
                                   θjo = other_probabilities_All_alpha10_beta10$counts_other_Business) 

Wjc_Business_alpha7_beta3 <- Wjc(θjc = probabilities_All_alpha7_beta3$counts_Business, 
                                 θjo = other_probabilities_All_alpha7_beta3$counts_other_Business) 

Wjc_Business_alpha10_beta1 <- Wjc(θjc = probabilities_All_alpha10_beta1$counts_Business, 
                                  θjo = other_probabilities_All_alpha10_beta1$counts_other_Business) 

Wjc_Business_alpha5_beta2 <- Wjc(θjc = probabilities_All_alpha5_beta2$counts_Business, 
                                 θjo = other_probabilities_All_alpha5_beta2$counts_other_Business) 

Wjc_Business_alpha2_beta5 <- Wjc(θjc = probabilities_All_alpha2_beta5$counts_Business, 
                                 θjo = other_probabilities_All_alpha2_beta5$counts_other_Business) 

# Obituaries Section:
Wjc_Obituaries_alpha20_beta20 <- Wjc(θjc = probabilities_All_alpha20_beta20$counts_Obituaries, 
                                     θjo = other_probabilities_All_alpha20_beta20$counts_other_Obituaries) 

Wjc_Obituaries_alpha10_beta10 <- Wjc(θjc = probabilities_All_alpha10_beta10$counts_Obituaries, 
                                     θjo = other_probabilities_All_alpha10_beta10$counts_other_Obituaries)

Wjc_Obituaries_alpha7_beta3 <- Wjc(θjc = probabilities_All_alpha7_beta3$counts_Obituaries, 
                                   θjo = other_probabilities_All_alpha7_beta3$counts_other_Obituaries)

Wjc_Obituaries_alpha10_beta1 <- Wjc(θjc = probabilities_All_alpha10_beta1$counts_Obituaries, 
                                    θjo = other_probabilities_All_alpha10_beta1$counts_other_Obituaries)

Wjc_Obituaries_alpha5_beta2 <- Wjc(θjc = probabilities_All_alpha5_beta2$counts_Obituaries, 
                                   θjo = other_probabilities_All_alpha5_beta2$counts_other_Obituaries)

Wjc_Obituaries_alpha2_beta5 <- Wjc(θjc = probabilities_All_alpha2_beta5$counts_Obituaries, 
                                   θjo = other_probabilities_All_alpha2_beta5$counts_other_Obituaries) 

# Sports Section:
Wjc_Sports_alpha20_beta20 <- Wjc(θjc = probabilities_All_alpha20_beta20$counts_Sports, 
                                 θjo = other_probabilities_All_alpha20_beta20$counts_other_Sports)

Wjc_Sports_alpha10_beta10 <- Wjc(θjc = probabilities_All_alpha10_beta10$counts_Sports, 
                                 θjo = other_probabilities_All_alpha10_beta10$counts_other_Sports)

Wjc_Sports_alpha7_beta3 <- Wjc(θjc = probabilities_All_alpha7_beta3$counts_Sports, 
                               θjo = other_probabilities_All_alpha7_beta3$counts_other_Sports)

Wjc_Sports_alpha10_beta1 <- Wjc(θjc = probabilities_All_alpha10_beta1$counts_Sports, 
                                θjo = other_probabilities_All_alpha10_beta1$counts_other_Sports)

Wjc_Sports_alpha5_beta2 <- Wjc(θjc = probabilities_All_alpha5_beta2$counts_Sports, 
                               θjo = other_probabilities_All_alpha5_beta2$counts_other_Sports)

Wjc_Sports_alpha2_beta5 <- Wjc(θjc = probabilities_All_alpha2_beta5$counts_Sports, 
                               θjo = other_probabilities_All_alpha2_beta5$counts_other_Sports) 

# World Section:
Wjc_World_alpha20_beta20 <- Wjc(θjc = probabilities_All_alpha20_beta20$counts_World, 
                                θjo = other_probabilities_All_alpha20_beta20$counts_other_World)

Wjc_World_alpha10_beta10 <- Wjc(θjc = probabilities_All_alpha10_beta10$counts_World, 
                                θjo = other_probabilities_All_alpha10_beta10$counts_other_World)

Wjc_World_alpha7_beta3 <- Wjc(θjc = probabilities_All_alpha7_beta3$counts_World, 
                              θjo = other_probabilities_All_alpha7_beta3$counts_other_World)

Wjc_World_alpha10_beta1 <- Wjc(θjc = probabilities_All_alpha10_beta1$counts_World, 
                               θjo = other_probabilities_All_alpha10_beta1$counts_other_World)

Wjc_World_alpha5_beta2 <- Wjc(θjc = probabilities_All_alpha5_beta2$counts_World, 
                              θjo = other_probabilities_All_alpha5_beta2$counts_other_World)

Wjc_World_alpha2_beta5 <- Wjc(θjc = probabilities_All_alpha2_beta5$counts_World, 
                              θjo = other_probabilities_All_alpha2_beta5$counts_other_World) 


################
# Calculate Woc
################
Woc <- function(θjc, θjo, θc)
{ Woc <- sum ( log( (1 - θjc) / (1 - θjo)) ) + sum( log( θc / (1 - θc)) ) }

# Use above function to calculate Woc for each section and for varing alphas and betas

# Arts Section:
Woc_Arts_alpha20_beta20 <- Woc(θjc = probabilities_All_alpha20_beta20$counts_Arts, 
                               θjo = other_probabilities_All_alpha20_beta20$counts_other_Arts, 
                               θc  = prior_All[1])  
Woc_Arts_alpha20_beta20 # [1] -61.47863

Woc_Arts_alpha10_beta10 <- Woc(θjc = probabilities_All_alpha10_beta10$counts_Arts, 
                               θjo = other_probabilities_All_alpha10_beta10$counts_other_Arts,
                               θc  = prior_All[1])  
Woc_Arts_alpha10_beta10 # [1] -29.3348

Woc_Arts_alpha7_beta3 <- Woc(θjc = probabilities_All_alpha7_beta3$counts_Arts, 
                             θjo = other_probabilities_All_alpha7_beta3$counts_other_Arts,
                             θc  = prior_All[1])  
Woc_Arts_alpha7_beta3 # [1] -19.6111

Woc_Arts_alpha10_beta1 <- Woc(θjc = probabilities_All_alpha10_beta1$counts_Arts, 
                              θjo = other_probabilities_All_alpha10_beta1$counts_other_Arts,
                              θc  = prior_All[1])  
Woc_Arts_alpha10_beta1 # [1] -29.79307

Woc_Arts_alpha5_beta2 <- Woc(θjc = probabilities_All_alpha5_beta2$counts_Arts, 
                             θjo = other_probabilities_All_alpha5_beta2$counts_other_Arts,
                             θc  = prior_All[1])  
Woc_Arts_alpha5_beta2 # [1] -12.90102

Woc_Arts_alpha2_beta5 <- Woc(θjc = probabilities_All_alpha2_beta5$counts_Arts, 
                             θjo = other_probabilities_All_alpha2_beta5$counts_other_Arts,
                             θc  = prior_All[1])  
Woc_Arts_alpha2_beta5 # [1] -2.693192

# Business Section:
Woc_Business_alpha20_beta20 <- Woc(θjc = probabilities_All_alpha20_beta20$counts_Business, 
                                   θjo = other_probabilities_All_alpha20_beta20$counts_other_Business, 
                                   θc  = prior_All[2])  
Woc_Business_alpha20_beta20 # [1] -64.12003

Woc_Business_alpha10_beta10 <- Woc(θjc = probabilities_All_alpha10_beta10$counts_Business, 
                                   θjo = other_probabilities_All_alpha10_beta10$counts_other_Business,
                                   θc  = prior_All[2])  
Woc_Business_alpha10_beta10 # [1] -31.99928

Woc_Business_alpha7_beta3 <- Woc(θjc = probabilities_All_alpha7_beta3$counts_Business, 
                                 θjo = other_probabilities_All_alpha7_beta3$counts_other_Business,
                                 θc  = prior_All[2])  
Woc_Business_alpha7_beta3 # [1] -22.29202

Woc_Business_alpha10_beta1 <- Woc(θjc = probabilities_All_alpha10_beta1$counts_Business, 
                                  θjo = other_probabilities_All_alpha10_beta1$counts_other_Business,
                                  θc  = prior_All[2])  
Woc_Business_alpha10_beta1 # [1] -32.47873

Woc_Business_alpha5_beta2 <- Woc(θjc = probabilities_All_alpha5_beta2$counts_Business, 
                                 θjo = other_probabilities_All_alpha5_beta2$counts_other_Business,
                                 θc  = prior_All[2])  
Woc_Business_alpha5_beta2 # [1] -15.58431

Woc_Business_alpha2_beta5 <- Woc(θjc = probabilities_All_alpha2_beta5$counts_Business, 
                                 θjo = other_probabilities_All_alpha2_beta5$counts_other_Business,
                                 θc  = prior_All[2])  
Woc_Business_alpha2_beta5 # [1] -5.369394

# Obituaries Section:
Woc_Obituaries_alpha20_beta20 <- Woc(θjc = probabilities_All_alpha20_beta20$counts_Obituaries, 
                                     θjo = other_probabilities_All_alpha20_beta20$counts_other_Obituaries, 
                                     θc  = prior_All[3])  
Woc_Obituaries_alpha20_beta20 # [1] -66.76738

Woc_Obituaries_alpha10_beta10 <- Woc(θjc = probabilities_All_alpha10_beta10$counts_Obituaries, 
                                     θjo = other_probabilities_All_alpha10_beta10$counts_other_Obituaries,
                                     θc  = prior_All[3])  
Woc_Obituaries_alpha10_beta10 # [1] -34.99315

Woc_Obituaries_alpha7_beta3 <- Woc(θjc = probabilities_All_alpha7_beta3$counts_Obituaries, 
                                   θjo = other_probabilities_All_alpha7_beta3$counts_other_Obituaries,
                                   θc  = prior_All[3])  
Woc_Obituaries_alpha7_beta3 # [1] -25.63859

Woc_Obituaries_alpha10_beta1 <- Woc(θjc = probabilities_All_alpha10_beta1$counts_Obituaries, 
                                    θjo = other_probabilities_All_alpha10_beta1$counts_other_Obituaries,
                                    θc  = prior_All[3])  
Woc_Obituaries_alpha10_beta1 # [1] -35.95591

Woc_Obituaries_alpha5_beta2 <- Woc(θjc = probabilities_All_alpha5_beta2$counts_Obituaries, 
                                   θjo = other_probabilities_All_alpha5_beta2$counts_other_Obituaries,
                                   θc  = prior_All[3])  
Woc_Obituaries_alpha5_beta2 # [1] -18.99396

Woc_Obituaries_alpha2_beta5 <- Woc(θjc = probabilities_All_alpha2_beta5$counts_Obituaries, 
                                   θjo = other_probabilities_All_alpha2_beta5$counts_other_Obituaries,
                                   θc  = prior_All[3])  
Woc_Obituaries_alpha2_beta5 # [1] -8.601104

# Sports Section:
Woc_Sports_alpha20_beta20 <- Woc(θjc = probabilities_All_alpha20_beta20$counts_Sports, 
                                 θjo = other_probabilities_All_alpha20_beta20$counts_other_Sports, 
                                 θc  = prior_All[4])  
Woc_Sports_alpha20_beta20 # [1] -63.90938

Woc_Sports_alpha10_beta10 <- Woc(θjc = probabilities_All_alpha10_beta10$counts_Sports, 
                                 θjo = other_probabilities_All_alpha10_beta10$counts_other_Sports,
                                 θc  = prior_All[4])  
Woc_Sports_alpha10_beta10 # [1] -31.78759

Woc_Sports_alpha7_beta3 <- Woc(θjc = probabilities_All_alpha7_beta3$counts_Sports, 
                               θjo = other_probabilities_All_alpha7_beta3$counts_other_Sports,
                               θc  = prior_All[4])  
Woc_Sports_alpha7_beta3 # [1] -22.07961

Woc_Sports_alpha10_beta1 <- Woc(θjc = probabilities_All_alpha10_beta1$counts_Sports, 
                                θjo = other_probabilities_All_alpha10_beta1$counts_other_Sports,
                                θc  = prior_All[4])  
Woc_Sports_alpha10_beta1 # [1] -32.26611

Woc_Sports_alpha5_beta2 <- Woc(θjc = probabilities_All_alpha5_beta2$counts_Sports, 
                               θjo = other_probabilities_All_alpha5_beta2$counts_other_Sports,
                               θc  = prior_All[4])  
Woc_Sports_alpha5_beta2 # [1] -15.37179

Woc_Sports_alpha2_beta5 <- Woc(θjc = probabilities_All_alpha2_beta5$counts_Sports, 
                               θjo = other_probabilities_All_alpha2_beta5$counts_other_Sports,
                               θc  = prior_All[4])  
Woc_Sports_alpha2_beta5 # [1] -5.15785

# World Section:
Woc_World_alpha20_beta20 <- Woc(θjc = probabilities_All_alpha20_beta20$counts_World, 
                                θjo = other_probabilities_All_alpha20_beta20$counts_other_World, 
                                θc  = prior_All[5])  
Woc_World_alpha20_beta20 # [1] -64.05165

Woc_World_alpha10_beta10 <- Woc(θjc = probabilities_All_alpha10_beta10$counts_World, 
                                θjo = other_probabilities_All_alpha10_beta10$counts_other_World,
                                θc  = prior_All[5])  
Woc_World_alpha10_beta10 # [1] -31.9037

Woc_World_alpha7_beta3 <- Woc(θjc = probabilities_All_alpha7_beta3$counts_World, 
                              θjo = other_probabilities_All_alpha7_beta3$counts_other_World,
                              θc  = prior_All[5])  
Woc_World_alpha7_beta3 # [1] -22.2233

Woc_World_alpha10_beta1 <- Woc(θjc = probabilities_All_alpha10_beta1$counts_World, 
                               θjo = other_probabilities_All_alpha10_beta1$counts_other_World,
                               θc  = prior_All[5])  
Woc_World_alpha10_beta1 # [1] -32.40997

Woc_World_alpha5_beta2 <- Woc(θjc = probabilities_All_alpha5_beta2$counts_World, 
                              θjo = other_probabilities_All_alpha5_beta2$counts_other_World,
                              θc  = prior_All[5])  
Woc_World_alpha5_beta2 # [1] -15.51557

Woc_World_alpha2_beta5 <- Woc(θjc = probabilities_All_alpha2_beta5$counts_World, 
                              θjo = other_probabilities_All_alpha2_beta5$counts_other_World,
                              θc  = prior_All[5])  
Woc_World_alpha2_beta5 # [1] -5.300711


################
# Calculate Xj
################
Xj <- as.matrix(test)


#########################################
# Calculate the log odds for each section
#########################################
Log_Odds <- function(Wjc, Xj, Woc) { Log_Odds <- Xj %*% Wjc + Woc }

# Use above function to calculate Woc for each section and for varing alphas and betas

### Arts Section:
Log_Odds_Arts_alpha20_beta20 <- Log_Odds(Wjc = Wjc_Arts_alpha20_beta20, 
                                         Xj = Xj, 
                                         Woc  = Woc_Arts_alpha20_beta20)  
head(Log_Odds_Arts_alpha20_beta20, 3) 
#         [1] 
# 1 -42.28760
# 2 -38.78545
# 4 -46.46225

Log_Odds_Arts_alpha10_beta10 <- Log_Odds(Wjc = Wjc_Arts_alpha10_beta10, 
                                         Xj = Xj,
                                         Woc  = Woc_Arts_alpha10_beta10)  
head(Log_Odds_Arts_alpha10_beta10, 3) 
#          [1] 
# 1 -14.812557
# 2  -8.361192
# 4 -15.701618

Log_Odds_Arts_alpha7_beta3 <- Log_Odds(Wjc = Wjc_Arts_alpha7_beta3, 
                                        Xj = Xj,
                                        Woc  = Woc_Arts_alpha7_beta3)  
head(Log_Odds_Arts_alpha7_beta3, 3)
#         [1] 
# 1 -7.7169106
# 2  0.5028811
# 4 -6.7643311

Log_Odds_Arts_alpha10_beta1 <- Log_Odds(Wjc = Wjc_Arts_alpha10_beta1, 
                                        Xj = Xj,
                                        Woc  = Woc_Arts_alpha10_beta1)  
head(Log_Odds_Arts_alpha10_beta1, 3)
#          [1] 
# 1 -15.114241
# 2  -8.673758
# 4 -16.070987

Log_Odds_Arts_alpha5_beta2 <- Log_Odds(Wjc = Wjc_Arts_alpha5_beta2, 
                                         Xj = Xj,
                                         Woc  = Woc_Arts_alpha5_beta2)  
head(Log_Odds_Arts_alpha5_beta2, 3)
#         [1] 
# 1 -3.7283630
# 2  6.3264319
# 4 -0.8855925

Log_Odds_Arts_alpha2_beta5 <- Log_Odds(Wjc = Wjc_Arts_alpha2_beta5, 
                                       Xj = Xj,
                                       Woc  = Woc_Arts_alpha2_beta5)  
head(Log_Odds_Arts_alpha2_beta5, 3)
#         [1] 
# 1 -1.987216
# 2 14.157746
# 4  6.979815


### Business Section:
Log_Odds_Business_alpha20_beta20 <- Log_Odds(Wjc = Wjc_Business_alpha20_beta20, 
                                             Xj = Xj, 
                                             Woc  = Woc_Business_alpha20_beta20)  
head(Log_Odds_Business_alpha20_beta20, 3) 
#         [1] 
# 1 -43.75214
# 2 -52.22021
# 4 -55.46249

Log_Odds_Business_alpha10_beta10 <- Log_Odds(Wjc = Wjc_Business_alpha10_beta10, 
                                             Xj = Xj,
                                             Woc  = Woc_Business_alpha10_beta10)  
head(Log_Odds_Business_alpha10_beta10, 3) 
#          [1] 
# 1 -15.54247
# 2 -25.24515
# 4 -27.14133

Log_Odds_Business_alpha7_beta3 <- Log_Odds(Wjc = Wjc_Business_alpha7_beta3, 
                                           Xj = Xj,
                                           Woc  = Woc_Business_alpha7_beta3)  
head(Log_Odds_Business_alpha7_beta3, 3)
#         [1] 
# 1  -7.997036
# 2 -18.164542
# 4 -19.589025

Log_Odds_Business_alpha10_beta1 <- Log_Odds(Wjc = Wjc_Business_alpha10_beta1, 
                                            Xj = Xj,
                                            Woc  = Woc_Business_alpha10_beta1)  
head(Log_Odds_Business_alpha10_beta1, 3)
#          [1] 
# 1 -15.86539
# 2 -25.58190
# 4 -27.53342

Log_Odds_Business_alpha5_beta2 <- Log_Odds(Wjc = Wjc_Business_alpha5_beta2, 
                                           Xj = Xj,
                                           Woc  = Woc_Business_alpha5_beta2)  
head(Log_Odds_Business_alpha5_beta2, 3)
#          [1] 
# 1  -3.518407
# 2 -13.970039
# 4 -15.068058

Log_Odds_Business_alpha2_beta5 <- Log_Odds(Wjc = Wjc_Business_alpha2_beta5, 
                                           Xj = Xj,
                                           Woc  = Woc_Business_alpha2_beta5)  
head(Log_Odds_Business_alpha2_beta5, 3)
#         [1] 
# 1  -0.04693461
# 2 -10.37539407
# 4 -11.38056937


### Obituaries Section:
Log_Odds_Obituaries_alpha20_beta20 <- Log_Odds(Wjc = Wjc_Obituaries_alpha20_beta20, 
                                               Xj = Xj, 
                                               Woc  = Woc_Obituaries_alpha20_beta20)  
head(Log_Odds_Obituaries_alpha20_beta20, 3) 
#         [1] 
# 1 -50.29625
# 2 -56.05524
# 4 -53.90272

Log_Odds_Obituaries_alpha10_beta10 <- Log_Odds(Wjc = Wjc_Obituaries_alpha10_beta10, 
                                               Xj = Xj,
                                               Woc  = Woc_Obituaries_alpha10_beta10)  
head(Log_Odds_Obituaries_alpha10_beta10, 3) 
#          [1] 
# 1 -23.90921
# 2 -29.93099
# 4 -24.58043

Log_Odds_Obituaries_alpha7_beta3 <- Log_Odds(Wjc = Wjc_Obituaries_alpha7_beta3, 
                                             Xj = Xj,
                                             Woc  = Woc_Obituaries_alpha7_beta3)  
head(Log_Odds_Obituaries_alpha7_beta3, 3)
#         [1] 
# 1 -17.53736
# 2 -23.45668
# 4 -16.73460

Log_Odds_Obituaries_alpha10_beta1 <- Log_Odds(Wjc = Wjc_Obituaries_alpha10_beta1, 
                                              Xj = Xj,
                                              Woc  = Woc_Obituaries_alpha10_beta1)  
head(Log_Odds_Obituaries_alpha10_beta1, 3)
#          [1] 
# 1 -24.71631
# 2 -30.75093
# 4 -25.45481

Log_Odds_Obituaries_alpha5_beta2 <- Log_Odds(Wjc = Wjc_Obituaries_alpha5_beta2, 
                                             Xj = Xj,
                                             Woc  = Woc_Obituaries_alpha5_beta2)  
head(Log_Odds_Obituaries_alpha5_beta2, 3)
#          [1] 
# 1 -13.93752
# 2 -19.56216
# 4 -11.72913

Log_Odds_Obituaries_alpha2_beta5 <- Log_Odds(Wjc = Wjc_Obituaries_alpha2_beta5, 
                                             Xj = Xj,
                                             Woc  = Woc_Obituaries_alpha2_beta5)  
head(Log_Odds_Obituaries_alpha2_beta5, 3)
#         [1] 
# 1 -12.99763
# 2 -16.58881
# 4  -6.67919


### Sports Section:
Log_Odds_Sports_alpha20_beta20 <- Log_Odds(Wjc = Wjc_Sports_alpha20_beta20, 
                                           Xj = Xj, 
                                           Woc  = Woc_Sports_alpha20_beta20)  
head(Log_Odds_Sports_alpha20_beta20, 3) 
#         [1] 
# 1 -49.48533
# 2 -56.90308
# 4 -54.64486

Log_Odds_Sports_alpha10_beta10 <- Log_Odds(Wjc = Wjc_Sports_alpha10_beta10, 
                                           Xj = Xj,
                                           Woc  = Woc_Sports_alpha10_beta10)  
head(Log_Odds_Sports_alpha10_beta10, 3) 
#          [1] 
# 1 -23.87624
# 2 -32.28006
# 4 -26.06340

Log_Odds_Sports_alpha7_beta3 <- Log_Odds(Wjc = Wjc_Sports_alpha7_beta3, 
                                         Xj = Xj,
                                         Woc  = Woc_Sports_alpha7_beta3)  
head(Log_Odds_Sports_alpha7_beta3, 3)
#         [1] 
# 1 -17.90603
# 2 -26.63319
# 4 -18.37353

Log_Odds_Sports_alpha10_beta1 <- Log_Odds(Wjc = Wjc_Sports_alpha10_beta1, 
                                          Xj = Xj,
                                          Woc  = Woc_Sports_alpha10_beta1)  
head(Log_Odds_Sports_alpha10_beta1, 3)
#          [1] 
# 1 -24.19942
# 2 -32.61664
# 4 -26.45447

Log_Odds_Sports_alpha5_beta2 <- Log_Odds(Wjc = Wjc_Sports_alpha5_beta2, 
                                         Xj = Xj,
                                         Woc  = Woc_Sports_alpha5_beta2)  
head(Log_Odds_Sports_alpha5_beta2, 3)
#          [1] 
# 1 -15.10012
# 2 -23.94282
# 4 -13.71994

Log_Odds_Sports_alpha2_beta5 <- Log_Odds(Wjc = Wjc_Sports_alpha2_beta5, 
                                         Xj = Xj,
                                         Woc  = Woc_Sports_alpha2_beta5)  
head(Log_Odds_Sports_alpha2_beta5, 3)
#         [1] 
# 1 -17.898054
# 2 -25.670412
# 4  -9.586453


### World Section:
Log_Odds_World_alpha20_beta20 <- Log_Odds(Wjc = Wjc_World_alpha20_beta20, 
                                          Xj = Xj, 
                                          Woc  = Woc_World_alpha20_beta20)  
head(Log_Odds_World_alpha20_beta20, 3) 
#         [1] 
# 1 -45.91955
# 2 -52.73063
# 4 -55.44615

Log_Odds_World_alpha10_beta10 <- Log_Odds(Wjc = Wjc_World_alpha10_beta10, 
                                          Xj = Xj,
                                          Woc  = Woc_World_alpha10_beta10)  
head(Log_Odds_World_alpha10_beta10, 3) 
#          [1] 
# 1 -18.59439
# 2 -26.46558
# 4 -27.90835

Log_Odds_World_alpha7_beta3 <- Log_Odds(Wjc = Wjc_World_alpha7_beta3, 
                                        Xj = Xj,
                                        Woc  = Woc_World_alpha7_beta3)  
head(Log_Odds_World_alpha7_beta3, 3)
#         [1] 
# 1 -11.55520
# 2 -19.85470
# 4 -21.07316

Log_Odds_World_alpha10_beta1 <- Log_Odds(Wjc = Wjc_World_alpha10_beta1, 
                                         Xj = Xj,
                                         Woc  = Woc_World_alpha10_beta1)  
head(Log_Odds_World_alpha10_beta1, 3)
#          [1] 
# 1 -18.91752
# 2 -26.80173
# 4 -28.30009

Log_Odds_World_alpha5_beta2 <- Log_Odds(Wjc = Wjc_World_alpha5_beta2, 
                                        Xj = Xj,
                                        Woc  = Woc_World_alpha5_beta2)  
head(Log_Odds_World_alpha5_beta2, 3)
#          [1] 
# 1  -7.610448
# 2 -16.164637
# 4 -17.499311

Log_Odds_World_alpha2_beta5 <- Log_Odds(Wjc = Wjc_World_alpha2_beta5, 
                                        Xj = Xj,
                                        Woc  = Woc_World_alpha2_beta5)  
head(Log_Odds_World_alpha2_beta5, 3)
#         [1] 
# 1  -6.399451
# 2 -14.205916
# 4 -18.644224


########################################################
# Create dataframes with probabilities for each section
# with varing alphas and betas
########################################################
test_prob_alpha20_beta20 <- data.frame(Log_Odds_Arts_alpha20_beta20,
                                       Log_Odds_Business_alpha20_beta20,
                                       Log_Odds_Obituaries_alpha20_beta20,
                                       Log_Odds_Sports_alpha20_beta20,
                                       Log_Odds_World_alpha20_beta20)
names(test_prob_alpha20_beta20) <- c("Arts","Business","Obituaries","Sports","World")
head(test_prob_alpha20_beta20, 3)
#        Arts  Business Obituaries    Sports     World
# 1 -42.28760 -43.75214  -50.29625 -49.48533 -45.91955
# 2 -38.78545 -52.22021  -56.05524 -56.90308 -52.73063
# 4 -46.46225 -55.46249  -53.90272 -54.64486 -55.44615
  
test_prob_alpha10_beta10 <- data.frame(Log_Odds_Arts_alpha10_beta10,
                                       Log_Odds_Business_alpha10_beta10,
                                       Log_Odds_Obituaries_alpha10_beta10,
                                       Log_Odds_Sports_alpha10_beta10,
                                       Log_Odds_World_alpha10_beta10)
names(test_prob_alpha10_beta10) <- c("Arts","Business","Obituaries","Sports","World")
head(test_prob_alpha10_beta10, 3)
#        Arts  Business Obituaries    Sports     World
# 1 -14.812557 -15.54247  -23.90921 -23.87624 -18.59439
# 2  -8.361192 -25.24515  -29.93099 -32.28006 -26.46558
# 4 -15.701618 -27.14133  -24.58043 -26.06340 -27.90835

test_prob_alpha7_beta3 <- data.frame(Log_Odds_Arts_alpha7_beta3,
                                     Log_Odds_Business_alpha7_beta3,
                                     Log_Odds_Obituaries_alpha7_beta3,
                                     Log_Odds_Sports_alpha7_beta3,
                                     Log_Odds_World_alpha7_beta3)
names(test_prob_alpha7_beta3) <- c("Arts","Business","Obituaries","Sports","World")
head(test_prob_alpha7_beta3, 3)
#        Arts  Business Obituaries    Sports     World
# 1 -7.7169106  -7.997036  -17.53736 -17.90603 -11.55520
# 2  0.5028811 -18.164542  -23.45668 -26.63319 -19.85470
# 4 -6.7643311 -19.589025  -16.73460 -18.37353 -21.07316

test_prob_alpha10_beta1 <- data.frame(Log_Odds_Arts_alpha10_beta1,
                                      Log_Odds_Business_alpha10_beta1,
                                      Log_Odds_Obituaries_alpha10_beta1,
                                      Log_Odds_Sports_alpha10_beta1,
                                      Log_Odds_World_alpha10_beta1)
names(test_prob_alpha10_beta1) <- c("Arts","Business","Obituaries","Sports","World")
head(test_prob_alpha10_beta1, 3)
#        Arts  Business Obituaries    Sports     World
# 1 -15.114241 -15.86539  -24.71631 -24.19942 -18.91752
# 2  -8.673758 -25.58190  -30.75093 -32.61664 -26.80173
# 4 -16.070987 -27.53342  -25.45481 -26.45447 -28.30009

test_prob_alpha5_beta2 <- data.frame(Log_Odds_Arts_alpha5_beta2,
                                     Log_Odds_Business_alpha5_beta2,
                                     Log_Odds_Obituaries_alpha5_beta2,
                                     Log_Odds_Sports_alpha5_beta2,
                                     Log_Odds_World_alpha5_beta2)
names(test_prob_alpha5_beta2) <- c("Arts","Business","Obituaries","Sports","World")
head(test_prob_alpha5_beta2, 3)
#        Arts  Business Obituaries    Sports     World
# 1 -3.7283630  -3.518407  -13.93752 -15.10012  -7.610448
# 2  6.3264319 -13.970039  -19.56216 -23.94282 -16.164637
# 4 -0.8855925 -15.068058  -11.72913 -13.71994 -17.499311

test_prob_alpha2_beta5 <- data.frame(Log_Odds_Arts_alpha2_beta5,
                                     Log_Odds_Business_alpha2_beta5,
                                     Log_Odds_Obituaries_alpha2_beta5,
                                     Log_Odds_Sports_alpha2_beta5,
                                     Log_Odds_World_alpha2_beta5)
names(test_prob_alpha2_beta5) <- c("Arts","Business","Obituaries","Sports","World")
head(test_prob_alpha2_beta5, 3)
#        Arts  Business Obituaries    Sports     World
# 1 -1.987216  -0.04693461  -12.99763 -17.898054  -6.399451
# 2 14.157746 -10.37539407  -16.58881 -25.670412 -14.205916
# 4  6.979815 -11.38056937   -6.67919  -9.586453 -18.644224


##########################################################
# Create Confusion Matrix for each pair of alpha and betas
##########################################################

require(caret)
levels(test_ActualSections) <- c("Arts", "Business", "Obituaries", "Sports", "World")

# Confusion Matrix for Alpha = 20 and Beta = 20
test_PredictedSections_alpha20_beta20 <- names(test_prob_alpha20_beta20)[apply(X=test_prob_alpha20_beta20, MARGIN=1, FUN=which.max)]
test_BothSections_alpha20_beta20 <- data.frame(test_PredictedSections_alpha20_beta20, test_ActualSections)
names(test_BothSections_alpha20_beta20) <- c("Predicted","Actual")
ConfusionMatrix_alpha20_beta20 <- as.table(confusionMatrix(table(test_BothSections_alpha20_beta20)))
ConfusionMatrix_alpha20_beta20
#             Actual
# Predicted    Arts Business Obituaries Sports World
#   Arts        877      168          7     33    88
#   Business     16      719          1      8    29
#   Obituaries   39       16        981     19    17
#   Sports       31       27          4    922    11
#   World        37       70          7     18   855

# Overall Statistics
# Accuracy : 0.8708        
# 95% CI : (0.8612, 0.88)
# No Information Rate : 0.2           
# P-Value [Acc > NIR] : < 2.2e-16     
# Kappa : 0.8385        
# Mcnemar's Test P-Value : < 2.2e-16     

# Statistics by Class:
#                      Class: Arts Class: Business Class: Obituaries Class: Sports Class: World
# Sensitivity               0.8770          0.7190            0.9810        0.9220       0.8550
# Specificity               0.9260          0.9865            0.9772        0.9818       0.9670
# Pos Pred Value            0.7477          0.9301            0.9151        0.9266       0.8663
# Neg Pred Value            0.9679          0.9335            0.9952        0.9805       0.9639
# Prevalence                0.2000          0.2000            0.2000        0.2000       0.2000
# Detection Rate            0.1754          0.1438            0.1962        0.1844       0.1710
# Detection Prevalence      0.2346          0.1546            0.2144        0.1990       0.1974


# Confusion Matrix for Alpha = 10 and Beta = 10
test_PredictedSections_alpha10_beta10 <- names(test_prob_alpha10_beta10)[apply(X=test_prob_alpha10_beta10, MARGIN=1, FUN=which.max)]
test_BothSections_alpha10_beta10 <- data.frame(test_PredictedSections_alpha10_beta10, test_ActualSections)
names(test_BothSections_alpha10_beta10) <- c("Predicted","Actual")
ConfusionMatrix_alpha10_beta10 <- as.table(confusionMatrix(table(test_BothSections_alpha10_beta10)))
ConfusionMatrix_alpha10_beta10
#             Actual
# Predicted    Arts Business Obituaries Sports World
# Arts        862      132          7     20    74
# Business     22      758          1      8    27
# Obituaries   40       16        974     17    11
# Sports       30       29          5    936    11
# World        46       65         13     19   877

# Overall Statistics
# Accuracy : 0.8814          
# 95% CI : (0.8721, 0.8902)
# No Information Rate : 0.2             
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.8518          
# Mcnemar's Test P-Value : < 2.2e-16       

# Statistics by Class:
#                      Class: Arts Class: Business Class: Obituaries Class: Sports Class: World
# Sensitivity               0.8620          0.7580            0.9740        0.9360       0.8770
# Specificity               0.9417          0.9855            0.9790        0.9812       0.9643
# Pos Pred Value            0.7872          0.9289            0.9206        0.9258       0.8598
# Neg Pred Value            0.9647          0.9422            0.9934        0.9840       0.9691
# Prevalence                0.2000          0.2000            0.2000        0.2000       0.2000
# Detection Rate            0.1724          0.1516            0.1948        0.1872       0.1754
# Detection Prevalence      0.2190          0.1632            0.2116        0.2022       0.2040


# Confusion Matrix for Alpha = 7 and Beta = 3
test_PredictedSections_alpha7_beta3 <- names(test_prob_alpha7_beta3)[apply(X=test_prob_alpha7_beta3, MARGIN=1, FUN=which.max)]
test_BothSections_alpha7_beta3 <- data.frame(test_PredictedSections_alpha7_beta3, test_ActualSections)
names(test_BothSections_alpha7_beta3) <- c("Predicted","Actual")
ConfusionMatrix_alpha7_beta3 <- as.table(confusionMatrix(table(test_BothSections_alpha7_beta3)))
ConfusionMatrix_alpha7_beta3
#             Actual
# Predicted    Arts Business Obituaries Sports World
# Arts        860      126          6     19    72
# Business     24      767          2      8    29
# Obituaries   37       17        970     17    11
# Sports       31       28          6    938    10
# World        48       62         16     18   878

# Overall Statistics
# Accuracy : 0.8826          
# 95% CI : (0.8734, 0.8914)
# No Information Rate : 0.2             
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.8532          
# Mcnemar's Test P-Value : < 2.2e-16       

# Statistics by Class:
#                      Class: Arts Class: Business Class: Obituaries Class: Sports Class: World
# Sensitivity               0.8600          0.7670            0.9700        0.9380       0.8780
# Specificity               0.9443          0.9842            0.9795        0.9812       0.9640
# Pos Pred Value            0.7941          0.9241            0.9221        0.9260       0.8591
# Neg Pred Value            0.9643          0.9441            0.9924        0.9844       0.9693
# Prevalence                0.2000          0.2000            0.2000        0.2000       0.2000
# Detection Rate            0.1720          0.1534            0.1940        0.1876       0.1756
# Detection Prevalence      0.2166          0.1660            0.2104        0.2026       0.2044


# Confusion Matrix for Alpha = 10 and Beta = 1
test_PredictedSections_alpha10_beta1 <- names(test_prob_alpha10_beta1)[apply(X=test_prob_alpha10_beta1, MARGIN=1, FUN=which.max)]
test_BothSections_alpha10_beta1 <- data.frame(test_PredictedSections_alpha10_beta1, test_ActualSections)
names(test_BothSections_alpha10_beta1) <- c("Predicted","Actual")
ConfusionMatrix_alpha10_beta1 <- as.table(confusionMatrix(table(test_BothSections_alpha10_beta1)))
ConfusionMatrix_alpha10_beta1
#             Actual
# Predicted    Arts Business Obituaries Sports World
# Arts        863      132          7     20    74
# Business     22      758          1      8    27
# Obituaries   39       16        974     17    11
# Sports       30       29          5    936    11
# World        46       65         13     19   877

# Overall Statistics
# Accuracy : 0.8816          
# 95% CI : (0.8723, 0.8904)
# No Information Rate : 0.2             
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.852           
# Mcnemar's Test P-Value : < 2.2e-16       

# Statistics by Class:
#                      Class: Arts Class: Business Class: Obituaries Class: Sports Class: World
# Sensitivity               0.8630          0.7580            0.9740        0.9360       0.8770
# Specificity               0.9417          0.9855            0.9792        0.9812       0.9643
# Pos Pred Value            0.7874          0.9289            0.9215        0.9258       0.8598
# Neg Pred Value            0.9649          0.9422            0.9934        0.9840       0.9691
# Prevalence                0.2000          0.2000            0.2000        0.2000       0.2000
# Detection Rate            0.1726          0.1516            0.1948        0.1872       0.1754
# Detection Prevalence      0.2192          0.1632            0.2114        0.2022       0.2040


# Confusion Matrix for Alpha = 5 and Beta = 2
test_PredictedSections_alpha5_beta2 <- names(test_prob_alpha5_beta2)[apply(X=test_prob_alpha5_beta2, MARGIN=1, FUN=which.max)]
test_BothSections_alpha5_beta2 <- data.frame(test_PredictedSections_alpha5_beta2, test_ActualSections)
names(test_BothSections_alpha5_beta2) <- c("Predicted","Actual")
ConfusionMatrix_alpha5_beta2 <- as.table(confusionMatrix(table(test_BothSections_alpha5_beta2)))
ConfusionMatrix_alpha5_beta2
#             Actual
# Predicted    Arts Business Obituaries Sports World
# Arts        852      120          6     17    68
# Business     29      782          2      7    27
# Obituaries   40       16        969     17    11
# Sports       32       27          6    942    10
# World        47       55         17     17   884

# Overall Statistics
# Accuracy : 0.8858          
# 95% CI : (0.8767, 0.8945)
# No Information Rate : 0.2             
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.8572          
# Mcnemar's Test P-Value : < 2.2e-16       

# Statistics by Class:
#                      Class: Arts Class: Business Class: Obituaries Class: Sports Class: World
# Sensitivity               0.8520          0.7820            0.9690        0.9420       0.8840
# Specificity               0.9473          0.9838            0.9790        0.9812       0.9660
# Pos Pred Value            0.8015          0.9233            0.9202        0.9263       0.8667
# Neg Pred Value            0.9624          0.9475            0.9921        0.9854       0.9709
# Prevalence                0.2000          0.2000            0.2000        0.2000       0.2000
# Detection Rate            0.1704          0.1564            0.1938        0.1884       0.1768
# Detection Prevalence      0.2126          0.1694            0.2106        0.2034       0.2040


# Confusion Matrix for Alpha = 2 and Beta = 5
test_PredictedSections_alpha2_beta5 <- names(test_prob_alpha2_beta5)[apply(X=test_prob_alpha2_beta5, MARGIN=1, FUN=which.max)]
test_BothSections_alpha2_beta5 <- data.frame(test_PredictedSections_alpha2_beta5, test_ActualSections)
names(test_BothSections_alpha2_beta5) <- c("Predicted","Actual")
ConfusionMatrix_alpha2_beta5 <- as.table(confusionMatrix(table(test_BothSections_alpha2_beta5)))
ConfusionMatrix_alpha2_beta5
#             Actual
# Predicted    Arts Business Obituaries Sports World
# Arts        848       98          5     14    64
# Business     38      809          6     11    37
# Obituaries   40       18        957     16    10
# Sports       29       24         13    943     9
# World        45       51         19     16   880

# Overall Statistics
# Accuracy : 0.8874         
# 95% CI : (0.8783, 0.896)
# No Information Rate : 0.2            
# P-Value [Acc > NIR] : < 2.2e-16      
# Kappa : 0.8592         
# Mcnemar's Test P-Value : 4.274e-13      

# Statistics by Class:
#                      Class: Arts Class: Business Class: Obituaries Class: Sports Class: World
# Sensitivity               0.8480          0.8090            0.9570        0.9430       0.8800
# Specificity               0.9547          0.9770            0.9790        0.9812       0.9673
# Pos Pred Value            0.8241          0.8979            0.9193        0.9263       0.8704
# Neg Pred Value            0.9617          0.9534            0.9891        0.9857       0.9699
# Prevalence                0.2000          0.2000            0.2000        0.2000       0.2000
# Detection Rate            0.1696          0.1618            0.1914        0.1886       0.1760
# Detection Prevalence      0.2058          0.1802            0.2082        0.2036       0.2022


##########################################
# Report the top 10 most informative words
##########################################


### Alpha = 20 and Beta = 20:
# Sort the probabilities and keep only the top ten
Arts_sort_alpha20_beta20       <- head(probabilities_All_alpha20_beta20[order(-probabilities_All_alpha20_beta20$counts_Arts), ], 10)
Business_sort_alpha20_beta20   <- head(probabilities_All_alpha20_beta20[order(-probabilities_All_alpha20_beta20$counts_Business), ], 10)
Obituaries_sort_alpha20_beta20 <- head(probabilities_All_alpha20_beta20[order(-probabilities_All_alpha20_beta20$counts_Obituaries), ], 10)
Sport_sort_alpha20_beta20      <- head(probabilities_All_alpha20_beta20[order(-probabilities_All_alpha20_beta20$counts_Sports), ], 10)
World_sort_alpha20_beta20      <- head(probabilities_All_alpha20_beta20[order(-probabilities_All_alpha20_beta20$counts_World), ], 10)
# Obtain the top ten names with the highest probability
Top_Words_alpha20_beta20 <- data.frame(row.names(Arts_sort_alpha20_beta20),
                                       row.names(Business_sort_alpha20_beta20),
                                       row.names(Obituaries_sort_alpha20_beta20),
                                       row.names(Sport_sort_alpha20_beta20),
                                       row.names(World_sort_alpha20_beta20))
names(Top_Words_alpha20_beta20) <- c("Arts Top Words", "Business Top Words", "Obituaries Top Words",
                                     "Sports Top Words", "World Top Words")
Top_Words_alpha20_beta20
#    Arts Top Words Business Top Words Obituaries Top Words Sports Top Words World Top Words
# 1            york            company                 died             game       president
# 2            book          yesterday                 home           season       officials
# 3           years            percent                death            night      government
# 4             art               year                 said          victory          united
# 5        reported            billion               cancer             team        american
# 6            week              years                lived        yesterday        military
# 7           books            million        complications            coach            said
# 8         theater             market            manhattan           sports        minister
# 9           music            federal                  son         football          people
# 10          night               week               family            world          police

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=Top_Words_alpha20_beta20, file="Ten_Most_Informative_Words.csv", sep="\t", row.names=FALSE, col.names=TRUE)

### Alpha = 10 and Beta = 10:
# Sort the probabilities and keep only the top ten
Arts_sort_alpha10_beta10       <- head(probabilities_All_alpha10_beta10[order(-probabilities_All_alpha10_beta10$counts_Arts), ], 10)
Business_sort_alpha10_beta10   <- head(probabilities_All_alpha10_beta10[order(-probabilities_All_alpha10_beta10$counts_Business), ], 10)
Obituaries_sort_alpha10_beta10 <- head(probabilities_All_alpha10_beta10[order(-probabilities_All_alpha10_beta10$counts_Obituaries), ], 10)
Sport_sort_alpha10_beta10      <- head(probabilities_All_alpha10_beta10[order(-probabilities_All_alpha10_beta10$counts_Sports), ], 10)
World_sort_alpha10_beta10      <- head(probabilities_All_alpha10_beta10[order(-probabilities_All_alpha10_beta10$counts_World), ], 10)
# Obtain the top ten names with the highest probability
Top_Words_alpha10_beta10 <- data.frame(row.names(Arts_sort_alpha10_beta10),
                                       row.names(Business_sort_alpha10_beta10),
                                       row.names(Obituaries_sort_alpha10_beta10),
                                       row.names(Sport_sort_alpha10_beta10),
                                       row.names(World_sort_alpha10_beta10))
names(Top_Words_alpha10_beta10) <- c("Arts Top Words", "Business Top Words", "Obituaries Top Words",
                                     "Sports Top Words", "World Top Words")
Top_Words_alpha10_beta10
#    Arts Top Words Business Top Words Obituaries Top Words Sports Top Words World Top Words
# 1            york            company                 died             game       president
# 2            book          yesterday                 home           season       officials
# 3           years            percent                death            night      government
# 4             art               year                 said          victory          united
# 5        reported            billion               cancer             team        american
# 6            week              years                lived        yesterday        military
# 7           books            million        complications            coach            said
# 8         theater             market            manhattan           sports        minister
# 9           music            federal                  son         football          people
# 10          night               week               family            world          police


### Alpha = 7 and Beta = 3:
# Sort the probabilities and keep only the top ten
Arts_sort_alpha7_beta3       <- head(probabilities_All_alpha7_beta3[order(-probabilities_All_alpha7_beta3$counts_Arts), ], 10)
Business_sort_alpha7_beta3   <- head(probabilities_All_alpha7_beta3[order(-probabilities_All_alpha7_beta3$counts_Business), ], 10)
Obituaries_sort_alpha7_beta3 <- head(probabilities_All_alpha7_beta3[order(-probabilities_All_alpha7_beta3$counts_Obituaries), ], 10)
Sport_sort_alpha7_beta3      <- head(probabilities_All_alpha7_beta3[order(-probabilities_All_alpha7_beta3$counts_Sports), ], 10)
World_sort_alpha7_beta3      <- head(probabilities_All_alpha7_beta3[order(-probabilities_All_alpha7_beta3$counts_World), ], 10)
# Obtain the top ten names with the highest probability
Top_Words_alpha7_beta3 <- data.frame(row.names(Arts_sort_alpha7_beta3),
                                     row.names(Business_sort_alpha7_beta3),
                                     row.names(Obituaries_sort_alpha7_beta3),
                                     row.names(Sport_sort_alpha7_beta3),
                                     row.names(World_sort_alpha7_beta3))
names(Top_Words_alpha7_beta3) <- c("Arts Top Words", "Business Top Words", "Obituaries Top Words",
                                     "Sports Top Words", "World Top Words")
Top_Words_alpha7_beta3
#    Arts Top Words Business Top Words Obituaries Top Words Sports Top Words World Top Words
# 1            york            company                 died             game       president
# 2            book          yesterday                 home           season       officials
# 3           years            percent                death            night      government
# 4             art               year                 said          victory          united
# 5        reported            billion               cancer             team        american
# 6            week              years                lived        yesterday        military
# 7           books            million        complications            coach            said
# 8         theater             market            manhattan           sports        minister
# 9           music            federal                  son         football          people
# 10          night               week               family            world          police


### Alpha = 10 and Beta = 1:
# Sort the probabilities and keep only the top ten
Arts_sort_alpha10_beta1       <- head(probabilities_All_alpha10_beta1[order(-probabilities_All_alpha10_beta1$counts_Arts), ], 10)
Business_sort_alpha10_beta1   <- head(probabilities_All_alpha10_beta1[order(-probabilities_All_alpha10_beta1$counts_Business), ], 10)
Obituaries_sort_alpha10_beta1 <- head(probabilities_All_alpha10_beta1[order(-probabilities_All_alpha10_beta1$counts_Obituaries), ], 10)
Sport_sort_alpha10_beta1      <- head(probabilities_All_alpha10_beta1[order(-probabilities_All_alpha10_beta1$counts_Sports), ], 10)
World_sort_alpha10_beta1      <- head(probabilities_All_alpha10_beta1[order(-probabilities_All_alpha10_beta1$counts_World), ], 10)
# Obtain the top ten names with the highest probability
Top_Words_alpha10_beta1 <- data.frame(row.names(Arts_sort_alpha10_beta1),
                                      row.names(Business_sort_alpha10_beta1),
                                      row.names(Obituaries_sort_alpha10_beta1),
                                      row.names(Sport_sort_alpha10_beta1),
                                      row.names(World_sort_alpha10_beta1))
names(Top_Words_alpha10_beta1) <- c("Arts Top Words", "Business Top Words", "Obituaries Top Words",
                                   "Sports Top Words", "World Top Words")
Top_Words_alpha10_beta1
#    Arts Top Words Business Top Words Obituaries Top Words Sports Top Words World Top Words
# 1            york            company                 died             game       president
# 2            book          yesterday                 home           season       officials
# 3           years            percent                death            night      government
# 4             art               year                 said          victory          united
# 5        reported            billion               cancer             team        american
# 6            week              years                lived        yesterday        military
# 7           books            million        complications            coach            said
# 8         theater             market            manhattan           sports        minister
# 9           music            federal                  son         football          people
# 10          night               week               family            world          police


### Alpha = 5 and Beta = 2:
# Sort the probabilities and keep only the top ten
Arts_sort_alpha5_beta2       <- head(probabilities_All_alpha5_beta2[order(-probabilities_All_alpha5_beta2$counts_Arts), ], 10)
Business_sort_alpha5_beta2   <- head(probabilities_All_alpha5_beta2[order(-probabilities_All_alpha5_beta2$counts_Business), ], 10)
Obituaries_sort_alpha5_beta2 <- head(probabilities_All_alpha5_beta2[order(-probabilities_All_alpha5_beta2$counts_Obituaries), ], 10)
Sport_sort_alpha5_beta2      <- head(probabilities_All_alpha5_beta2[order(-probabilities_All_alpha5_beta2$counts_Sports), ], 10)
World_sort_alpha5_beta2      <- head(probabilities_All_alpha5_beta2[order(-probabilities_All_alpha5_beta2$counts_World), ], 10)
# Obtain the top ten names with the highest probability
Top_Words_alpha5_beta2 <- data.frame(row.names(Arts_sort_alpha5_beta2),
                                     row.names(Business_sort_alpha5_beta2),
                                     row.names(Obituaries_sort_alpha5_beta2),
                                     row.names(Sport_sort_alpha5_beta2),
                                     row.names(World_sort_alpha5_beta2))
names(Top_Words_alpha5_beta2) <- c("Arts Top Words", "Business Top Words", "Obituaries Top Words",
                                    "Sports Top Words", "World Top Words")
Top_Words_alpha5_beta2
#    Arts Top Words Business Top Words Obituaries Top Words Sports Top Words World Top Words
# 1            york            company                 died             game       president
# 2            book          yesterday                 home           season       officials
# 3           years            percent                death            night      government
# 4             art               year                 said          victory          united
# 5        reported            billion               cancer             team        american
# 6            week              years                lived        yesterday        military
# 7           books            million        complications            coach            said
# 8         theater             market            manhattan           sports        minister
# 9           music            federal                  son         football          people
# 10          night               week               family            world          police


### Alpha = 2 and Beta = 5:
# Sort the probabilities and keep only the top ten
Arts_sort_alpha2_beta5       <- head(probabilities_All_alpha2_beta5[order(-probabilities_All_alpha2_beta5$counts_Arts), ], 10)
Business_sort_alpha2_beta5   <- head(probabilities_All_alpha2_beta5[order(-probabilities_All_alpha2_beta5$counts_Business), ], 10)
Obituaries_sort_alpha2_beta5 <- head(probabilities_All_alpha2_beta5[order(-probabilities_All_alpha2_beta5$counts_Obituaries), ], 10)
Sport_sort_alpha2_beta5      <- head(probabilities_All_alpha2_beta5[order(-probabilities_All_alpha2_beta5$counts_Sports), ], 10)
World_sort_alpha2_beta5      <- head(probabilities_All_alpha2_beta5[order(-probabilities_All_alpha2_beta5$counts_World), ], 10)
# Obtain the top ten names with the highest probability
Top_Words_alpha2_beta5 <- data.frame(row.names(Arts_sort_alpha2_beta5),
                                     row.names(Business_sort_alpha2_beta5),
                                     row.names(Obituaries_sort_alpha2_beta5),
                                     row.names(Sport_sort_alpha2_beta5),
                                     row.names(World_sort_alpha2_beta5))
names(Top_Words_alpha2_beta5) <- c("Arts Top Words", "Business Top Words", "Obituaries Top Words",
                                   "Sports Top Words", "World Top Words")
Top_Words_alpha2_beta5
#    Arts Top Words Business Top Words Obituaries Top Words Sports Top Words World Top Words
# 1            york            company                 died             game       president
# 2            book          yesterday                 home           season       officials
# 3           years            percent                death            night      government
# 4             art               year                 said          victory          united
# 5        reported            billion               cancer             team        american
# 6            week              years                lived        yesterday        military
# 7           books            million        complications            coach            said
# 8         theater             market            manhattan           sports        minister
# 9           music            federal                  son         football          people
# 10          night               week               family            world          police



##############################################
# Top 10 “most difficult to classify” articles
##############################################

# Calculate the variance of probabilities
test_variance_alpha20_beta20 <- apply(X=test_prob_alpha20_beta20, MARGIN=1, FUN=var)
test_variance_alpha20_beta20 <- data.frame(test_variance_alpha20_beta20, test_BothSections_alpha20_beta20$Predicted)
names(test_variance_alpha20_beta20) <- c("Variance", "PredictedSection")

# Order the variances to obtain the lowest ten
top_variance_alpha20_beta20 <- head(test_variance_alpha20_beta20[order(test_variance_alpha20_beta20$Variance), ], 10)
top_variance_alpha20_beta20
#       Variance PredictedSection
# 9316 0.6247438           Sports
# 3546 0.8262763             Arts
# 5432 1.2166662             Arts
# 2187 1.2351013         Business
# 6548 1.3271198           Sports
# 6805 1.3960409             Arts
# 7664 1.4409677             Arts
# 1635 1.4943367             Arts
# 6100 1.5398813             Arts
# 9447 1.5593955           Sports

# Obtain indeces of the ten articles with the lowest variance
top_variance_alpha20_beta20_index <- as.numeric(rownames(top_variance_alpha20_beta20))
top_variance_alpha20_beta20_index
# [1] 9316 3546 5432 2187 6548 6805 7664 1635 6100 9447

# Read again the original articles in order to print out the full articles 
setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
articles_All <- read.table(file="NYTimes_API_All.csv", sep="\t", stringsAsFactors=FALSE, header=TRUE)
names(articles_All) <- c("URL","Title","Body","ActualSection")
articles_All$ActualSection <- as.factor(articles_All$ActualSection)
levels(articles_All$ActualSection) <- c("Arts","Business","Obituaries","Sports","World")

# Subset the 10000 articles by the index of 10 articles selected above (top_variance_alpha20_beta20_index)
articles_All_difficult_clasiffy <- articles_All[top_variance_alpha20_beta20_index, ]

# Create a final dataset with the difficult articles to classify and with variance and predictions
Final_Articles_Difficult_Clasiffy <- cbind(articles_All_difficult_clasiffy, top_variance_alpha20_beta20)
names(Final_Articles_Difficult_Clasiffy)
# [1] "URL" "Title" "Body" "ActualSection"  "Variance"  "PredictedSection"

Final_Articles_Difficult_Clasiffy[ , c(2,4,6,5)]
#                                                                          Title ActualSection PredictedSection  Variance
# 9316  THE SATURDAY PROFILE; The Defender of a Lesser-Known Guarantee in Russia         World           Sports 0.6247438
# 3546                         Getting a Lower Price, for Repairs You Don't Need      Business             Arts 0.8262763
# 5432               Steve Irwin, Wildlife Master, Is Killed by a Stingray at 44    Obituaries             Arts 1.2166662
# 2187 GOLDEN OPPORTUNITIES; When Shielding Money Clashes With Elders' Free Will      Business         Business 1.2351013
# 6548                                     Vick Receives 23 Months And a Lecture        Sports           Sports 1.3271198
# 6805                     SPORTS OF THE TIMES; A Righteous Recipe for Longevity        Sports             Arts 1.3960409
# 7664                              After Charmed Season, Cavaliers Must Regroup        Sports             Arts 1.4409677
# 1635                                                  CRITICS' CHOICE; New CDs          Arts             Arts 1.4943367
# 6100              Pine Bark Beetle Infestation Is Causing a Rocky Mountain Low        Sports             Arts 1.5398813
# 9447     tMembers of New Group in Britain Aim to Offset Their Own Carbon Outpu         World           Sports 1.5593955

Final_Articles_Difficult_Clasiffy[ , 3]
# [1] "THE men who attacked Ivan Y. Pavlov waited beside his car outside his home. They knocked him over from behind, stomped him and kicked him in the head. None of them spoke. They stole nothing. As Mr. Pavlov lay curled defensively on the street, they trotted away. Then they tried to run him over with their car. Mr. Pavlov rolled clear, he said. The"   
# [2] "In his study of auto-repair shops, Henry Schneider varied what he told the mechanics in one important way. At half of the 40 Connecticut garages where he brought a Subaru station wagon for an inspection, he said that he and his (fictional) wife had just moved to town. At the other half, he said they were about to move to Chicago. And it made a"     
# [3] "Steve Irwin, the khaki-clad wildlife stalker who won global fame with his televised death-defying crocodile stunts and whose booming voice made ''Crikey!'' in a ripe Australian accent an international catchword, was killed by a stingray yesterday while filming a documentary at the Great Barrier Reef off Australia's northeast coast. He was 44."      
# [4] "Eight years ago, when Robert J. Pyle was 73 years old, he had about $500,000 in the bank and owned a house in Northern California worth about $650,000. He was looking forward to a comfortable retirement. Today, at 81, he has lost everything. Mr. Pyle, a retired aerospace engineer, now lives in his stepdaughter's tiny, mountainside home in a room"   
# [5] "Michael Vick, the former Atlanta Falcons star who has traded his No. 7 jersey for a black-and-white-striped jail uniform, stood expressionless in a federal courtroom Monday and apologized to the court and his family for his involvement in a dogfighting ring. But for the judge about to decide Vick's fate, those words were not enough. ''I think you"  
# [6] "With bullets whizzing over his head in the Battle of the Bulge, Daniel Bukantz dug his foxhole and hunkered down because, as he freely admits, ''I was scared.'' At that moment, he had no guarantee he would ever get home, much less become a four-time Olympic fencer and five-time Olympic judge, or that he would live to be 90, which he will reach"     
# [7] "In his new commercial, LeBron James makes the highly counterintuitive, almost comical claim, ''You don't want to be LeBron James.'' There are, it seems, some burdens to being rich, young, famous and responsible for the hoop dreams of your home state. But James did not need a marketing department to sell this message. The proof was in the chorus of" 
# [8] "CELINE DION ''Taking Chances'' (Columbia) It has now been a decade since Celine Dion first shared the musical electrocardiogram that has come to define her career. And in retrospect ''My Heart Will Go On'' seems like the end of an era. Since then she has managed only one more Top 10 hit in the United States: ''That's the Way It Is,'' from 1999. Her"
# [9] "We do not have to own land because, collectively with our fellow Americans, we possess millions of acres. Much of it is in the romantic West, where deer, elk and antelope can be harvested. For me this year, the quarry was mule deer. From my home in southern Illinois, it is a two-day drive, uphill, to Laramie, Wyo., at the base of the Rockies. Well" 
#[10] "Jacqueline Sheedy has turned the former coal barge where she lives into a shrine to energy efficiency: she reads by candlelight in midwinter, converts the waste from her toilet into fertilizer, and hauls fresh water home on a trailer attached to her bicycle. Now Ms. Sheedy has set herself a new goal: to stop burning coal for heat and instead use"   

setwd("~/Desktop/INTRODUCTION TO DATA SCIENCE/Homework/Homework 4")
write.table(x=Final_Articles_Difficult_Clasiffy, file="Ten_Most_Difficult_To_Clasiffy_Articles.csv", 
            sep="\t", row.names=FALSE, col.names=TRUE)

