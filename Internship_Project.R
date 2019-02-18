
rm(list=ls())

install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("udpipe")
install.packages("tm")
install.packages("reshape2")

library(ROAuth)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(udpipe)
library(tm)
library(wordcloud)
library(reshape2)

setwd("C:/Users/Victoria Sharam/Desktop/getTV Twitter Data")

files<-list.files("C:/Users/Victoria Sharam/Desktop/getTV Twitter Data", full.names = TRUE)
getTV_Tweets <- NULL
for(i in 1:22)
{ getTV_Tweets<- rbind(getTV_Tweets,read.csv(files[i]))
}

# Change variable types
getTV_Tweets$time=as.POSIXct(getTV_Tweets$time, format="%Y-%m-%d %H:%M")
#getTV_Tweets$id=as.factor(getTV_Tweets$id)

# Convert to PDT
time_PDT <- with_tz(getTV_Tweets$time, "America/Los_Angeles")
getTV_Tweets$time <- time_PDT

# Add Day of Week
getTV_Tweets$Date <- getTV_Tweets$time
getTV_Tweets$Date=as.Date(getTV_Tweets$Date, "%m/%d/%Y")
getTV_Tweets$DayOfWeek <- as.factor(wday(getTV_Tweets$Date, label = TRUE))

# Add Time: Hour
hour_time <- hour(ymd_hms(getTV_Tweets$time))
getTV_Tweets$hour <- hour_time

# Add Time: Minute
minute_time <- minute(ymd_hms(getTV_Tweets$time))
getTV_Tweets$minute <- minute_time

# Add Month
month <- month(ymd_hms(getTV_Tweets$time))
getTV_Tweets$month <- month

# Add Time
created_split <- colsplit(getTV_Tweets$time, " ", c("Date", "Time"))
getTV_Tweets$time <- created_split$Time

# Add Time: Hour + Minute
getTV_Tweets$hour_min <- substr(getTV_Tweets$time, 0, 5)

# Add Time + Day of Week
getTV_Tweets$hour_day <- paste(getTV_Tweets$DayOfWeek, getTV_Tweets$hour)

##################################################################################################

# UNIVARIATE ANALYSES

# IMPRESSIONS
table(getTV_Tweets$impressions)
ggplot(getTV_Tweets,aes(x="Impressions", y=impressions))+geom_boxplot()
ggplot(getTV_Tweets,aes(x=impressions))+geom_histogram(binwidth = 30)
summary(getTV_Tweets$impressions)

#######################################################################################################

# ENGAGEMENT
table(getTV_Tweets$engagements)
ggplot(getTV_Tweets, aes(x="engagements", y=engagements))+geom_boxplot()
ggplot(getTV_Tweets,aes(x=engagements))+geom_histogram(binwidth = 20)
summary(getTV_Tweets$engagements)

#######################################################################################################

# ENGAGEMENT RATE
table(getTV_Tweets$engagement.rate)
ggplot(getTV_Tweets,aes(x='Engagement Rate',y=engagement.rate))+geom_boxplot()
summary(getTV_Tweets$engagement.rate)

#######################################################################################################

# RETWEETS
table(getTV_Tweets$retweets)
ggplot(getTV_Tweets, aes(x="Number of RTs", y=retweetCount))+geom_boxplot()
ggplot(getTV_Tweets,aes(x=retweets))+geom_histogram(binwidth = 5)
summary(getTV_Tweets$retweets)

#######################################################################################################

# FAVORITES
table(getTV_Tweets$likes)
ggplot(getTV_Tweets, aes(x="Number of Likes", y=likes))+geom_boxplot()
ggplot(getTV_Tweets,aes(x=likes))+geom_histogram(binwidth = 5)
summary(getTV_Tweets$likes)

#######################################################################################################

# REPLIES TO OUR TWEETS
table(getTV_Tweets$replies)
ggplot(getTV_Tweets, aes(x="Number of Replies", y=replies))+geom_boxplot()
ggplot(getTV_Tweets,aes(x=replies))+geom_histogram(binwidth = 5)
summary(getTV_Tweets$replies)

#######################################################################################################

# USER PROFILE CLICKS
table(getTV_Tweets$user.profile.clicks)
ggplot(getTV_Tweets,aes(x="User Profile Clicks",y=user.profile.clicks))+geom_boxplot()
ggplot(getTV_Tweets, aes(x=replies))+geom_histogram(binwidth = 5)
summary(getTV_Tweets$user.profile.clicks)

#######################################################################################################

# URL CLICKS
table(getTV_Tweets$url.clicks)
ggplot(getTV_Tweets,aes(x="URL Clicks",y=url.clicks))+geom_boxplot()
summary(getTV_Tweets$url.clicks)

#######################################################################################################

# HASHTAG CLICKS
table(getTV_Tweets$hashtag.clicks)
ggplot(getTV_Tweets,aes(x="Hashtag Clicks",y=hashtag.clicks))+geom_boxplot()
summary(getTV_Tweets$hashtag.clicks)

#######################################################################################################

# DETAIL EXPANDS
table(getTV_Tweets$detail.expands)
ggplot(getTV_Tweets,aes(x="Detail Expands",y=detail.expands))+geom_boxplot()
ggplot(getTV_Tweets, aes(x=detail.expands))+geom_histogram(binwidth = 5)
summary(getTV_Tweets$detail.expands)

#######################################################################################################

# PERMALINK CLICKS
table(getTV_Tweets$permalink.clicks)
ggplot(getTV_Tweets,aes(x="Permalink Clicks",y=permalink.clicks))+geom_boxplot()
summary(getTV_Tweets$permalink.clicks)

#######################################################################################################

# FOLLOWS
table(getTV_Tweets$follows)
ggplot(getTV_Tweets,aes(x="Follows",y=follows))+geom_boxplot()
summary(getTV_Tweets$follows)

#######################################################################################################

# MEDIA ENGAGEMENTS
table(getTV_Tweets$media.engagements)
ggplot(getTV_Tweets,aes(x="Media Engagements",y=media.engagements))+geom_boxplot()
ggplot(getTV_Tweets, aes(x=media.engagements))+geom_histogram(binwidth = 5)
summary(getTV_Tweets$media.engagements)

#######################################################################################################

# TIMING

# Date
table(getTV_Tweets$Date)
summary(getTV_Tweets$Date)

# Day of Week
sort(table(getTV_Tweets$DayOfWeek))

# Hour
sort(table(getTV_Tweets$hour))

# Minute
sort(table(getTV_Tweets$minute))

# Hour_Min
sort(table(getTV_Tweets$hour_min))

# Hour_Day
sort(table(getTV_Tweets$hour_day))

# Month
sort(table(getTV_Tweets$month))

#######################################################################################################
#######################################################################################################
# BIVARIATE ANALYSES

#######################################################################################################

# Engagement-Replies
ggplot(getTV_Tweets, aes(x=engagements, y=replies))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$engagements,getTV_Tweets$replies)

# Impressions-Replies
ggplot(getTV_Tweets, aes(x=impressions, y=replies))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$impressions,getTV_Tweets$replies)

#######################################################################################################

# Engagement-Likes
ggplot(getTV_Tweets, aes(x=engagements, y=likes))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$engagements,getTV_Tweets$likes)

# Impressions-Likes
ggplot(getTV_Tweets, aes(x=impressions, y=likes))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$impressions,getTV_Tweets$likes)

#######################################################################################################

# Engagement-Retweet
ggplot(getTV_Tweets, aes(x=engagements, y=retweets))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$engagements,getTV_Tweets$retweets)

# Impressions-Retweet
ggplot(getTV_Tweets, aes(x=impressions, y=retweets))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$impressions,getTV_Tweets$retweets)

#######################################################################################################

cor(getTV_Tweets$engagements,getTV_Tweets$user.profile.clicks)
cor(getTV_Tweets$engagements,getTV_Tweets$url.clicks)
cor(getTV_Tweets$engagements,getTV_Tweets$hashtag.clicks)
cor(getTV_Tweets$engagements,getTV_Tweets$detail.expands)
#cor(getTV_Tweets$engagements,getTV_Tweets$permalink.clicks)
cor(getTV_Tweets$engagements,getTV_Tweets$follows)
cor(getTV_Tweets$engagements,getTV_Tweets$media.engagements)

cor(getTV_Tweets$impressions,getTV_Tweets$user.profile.clicks)
cor(getTV_Tweets$impressions,getTV_Tweets$url.clicks)
cor(getTV_Tweets$impressions,getTV_Tweets$hashtag.clicks)
cor(getTV_Tweets$impressions,getTV_Tweets$detail.expands)
# cor(getTV_Tweets$impressions,getTV_Tweets$permalink.clicks)
cor(getTV_Tweets$impressions,getTV_Tweets$follows)
cor(getTV_Tweets$impressions,getTV_Tweets$media.engagements)

#######################################################################################################

# Engagement-Day of Week
aggregate(formula=engagements~DayOfWeek,data=getTV_Tweets,FUN=mean)
fit21<-aov(engagements~DayOfWeek,data=getTV_Tweets)
summary(fit21)

# Impressions-Day of Week
aggregate(formula=impressions~DayOfWeek,data=getTV_Tweets,FUN=mean)
fit22<-aov(impressions~DayOfWeek,data=getTV_Tweets)
summary(fit22)

#######################################################################################################

# Engagement-Hour
aggregate(formula=engagements~hour,data = getTV_Tweets, FUN=mean)
fit24 <- aov(engagements~hour, data= getTV_Tweets)
summary(fit24)

# Impressions-Hour
aggregate(formula=impressions~hour, data=getTV_Tweets, FUN=mean)
fit25 <- aov(impressions~hour, data=getTV_Tweets)
summary(fit25)

#######################################################################################################

# Engagement-Minute
aggregate(formula=engagements~minute,data = getTV_Tweets, FUN=mean)
fit27 <- aov(engagements~minute, data= getTV_Tweets)
summary(fit27)

# Impressions-Minute
aggregate(formula=impressions~minute, data=getTV_Tweets, FUN=mean)
fit28 <- aov(impressions~minute, data=getTV_Tweets)
summary(fit28)

#######################################################################################################

# Engagement-Time
aggregate(formula=engagements~hour_min, data=getTV_Tweets, FUN=mean)
fit30 <- aov(engagements~hour_min, data=getTV_Tweets)
summary(fit30)

# Impressions-Time
aggregate(formula=impressions~hour_min, data=getTV_Tweets, FUN=mean)
fit31 <- aov(impressions~hour_min, data=getTV_Tweets)
summary(fit31)

#######################################################################################################

# Native: Engagement-Time/Day
aggregate(formula=engagements~hour_day, data=getTV_Tweets, FUN=mean)
fit30 <- aov(engagements~hour_day, data=getTV_Tweets)
summary(fit30)

# Impressions-Time/Day
aggregate(formula=impressions~hour_day, data=getTV_Tweets, FUN=mean)
fit31 <- aov(impressions~hour_day, data=getTV_Tweets)
summary(fit31)

# Engagement-Month
aggregate(formula=engagements~month, data=getTV_Tweets, FUN=mean)
fit31 <- aov(engagements~month, data=getTV_Tweets)
summary(fit31)

# Impressions-Month
aggregate(formula=impressions~month, data=getTV_Tweets, FUN=mean)
fit31 <- aov(impressions~month, data=getTV_Tweets)
summary(fit31)

#######################################################################################################
# MISC BIVARIATES CONT-CONT

ggplot(getTV_Tweets, aes(x=retweets, y=media.engagements))+geom_point()+stat_smooth(method='lm') 
cor(getTV_Tweets$impressions,getTV_Tweets$engagements)

#######################################################################################################
