setwd("E:/Schoolwork/CSU Global/MIS500 Foundations of Data Analysis/MIS500 Foundations of Data Analysis Portfolio Project")

library(R.utils)
library(caret)

millionsongraw <- read.csv("millionsong.csv")

summary(millionsongraw)

#viewing missing values
sapply(millionsongraw,function(x) sum(is.na(x)))

#removing null values for song.hotttnesss
millionsong <- millionsongraw[complete.cases(millionsongraw$song.hotttnesss),]

#removing songs where hotttnesss = 0 (there is no value)
millionsong <-subset(millionsong, song.hotttnesss !=0)

rm(millionsongraw)

#TEMPO T-TEST

#5 number summary

#removing songs where tempo = 0
millionsong <- subset(millionsong, tempo != 0)

summary(millionsong$tempo)
tempovars <- c("song.hotttnesss","tempo")

slowsample <- subset(millionsong, tempo <= 99.87)
slowsample <- slowsample[tempovars]

fastsample <- subset(millionsong, tempo >=146.46)
fastsample <- fastsample[tempovars]

tempotest <- t.test(slowsample$song.hotttnesss, fastsample$song.hotttnesss, paired=FALSE)
tempotest

#DURATION T-TEST
durationvars <- c("song.hotttnesss", "duration")

#summary of duration
summary(millionsong$duration)
max(millionsong$duration) 
#max is 1686.752 seconds

#standard deviation of duration 
sd(millionsong$duration)
#SD = 102.7575

#short sample is within Q1
shortsample <- subset(millionsong, duration <184.85)
shortsample <- shortsample[durationvars]

#the long sample's upper limit is within 2 standard deviations of the mean
longsample <- longsample <- subset(millionsong, duration < 446.9256 & duration > 279.86)
longsample <- longsample[durationvars]

#duration t-test
durationtest <- t.test(shortsample$song.hotttnesss, longsample$song.hotttnesss, paired=FALSE)
durationtest


#MULTIPLE LINEAR REGRESSION
songmodel <- lm(formula= song.hotttnesss ~ tempo + duration + key + loudness + mode + time_signature + end_of_fade_in, data=millionsong)
summary(songmodel)

#writing csv file of cleaned dataset for tableau use
#write.csv(millionsong, "millionsongcleaned.csv")