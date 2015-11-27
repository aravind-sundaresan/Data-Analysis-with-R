getwd()

setwd('/home/apollo/R')
getwd()


statesInfo <- read.csv('stateData.csv')

subset(statesInfo,state.region == 1)

reddit <- read.csv('reddit.csv')

table(reddit$employment.status)

levels(reddit$age.range)

library(ggplot2)
qplot(data = reddit,x = age.range)

reddit$age.range <- ordered(reddit$age.range,c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"))


pf <- read.delim('pseudo_facebook.tsv')
names(pf)

library(ggplot2)
qplot(data = pf,x = dob_day)+
  scale_x_discrete(breaks = 1:31)+
  facet_wrap(~dob_month,ncol = 3)

qplot(data = pf,x = friend_count,xlim = c(0,1000), binwidth = 25)

qplot(data = pf,x = friend_count,binwidth = 25)+
  scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender,ncol = 2)

qplot(data = subset(pf,!is.na(gender)),x = friend_count,binwidth = 25)+
  scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender,ncol = 2)

qplot(data = subset(na.omit(pf)),x = friend_count,binwidth = 25)+
  scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender,ncol = 2)

table(pf$gender)
by(pf$friend_count,pf$gender,summary)

qplot(x = tenure/365,data = pf,binwidth = 0.25,
      color = I('black'),fill = I('blue'),
      xlab = 'Number of years on Facebook',
      ylab = 'Number of users')+
  scale_x_continuous(breaks = seq(0,7,1),limits = c(0,7))

qplot(x = age,data = pf,binwidth = 1,
      xlab = 'age of the users',
      ylab = 'Number of users',
      color = I('black'),fill = I('orange'))+
  scale_x_continuous(breaks = seq(10,100,5),limits = c(10,100))


graph1 <- qplot(x = friend_count,data = pf)
graph2 <- qplot(x = friend_count,data = pf)+
  scale_x_log10()
graph3 <- qplot(x = friend_count,data = pf)+
  scale_x_sqrt()
grid.arrange(graph1,graph2,graph3,ncol = 1)

qplot(data = subset(pf,!is.na(gender)),x = friend_count,binwidth = 5,geom = 'freqpoly',
              color = gender)+
  scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))


qplot(data = subset(pf,!is.na(gender)),x = friend_count,y = ..count../sum(..count..),
      binwidth = 5,geom = 'freqpoly',color = gender,
      xlab = 'Friend Count',
      ylab = 'Proportion of users with that friend count')+
  scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))

qplot(data = subset(pf,!is.na(gender)),x = www_likes,geom = 'freqpoly',color = gender)+
 scale_x_log10()

by(pf$www_likes,pf$gender,sum)

qplot(x = gender,y = friend_count,data = subset(pf,!is.na(gender)),
      geom = 'boxplot')+
  coord_cartesian(ylim = c(0,250))

by(pf$friendships_initiated,pf$gender,summary)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0,1,0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
  
summary(pf$mobile_check_in)

pos <- (sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in))*100

qplot(x = age,y = friend_count,data =pf)
ggplot(aes(x = age,y = friend_count),data = pf)+
  geom_point(alpha = 1/20)+
  xlim(13,90)

ggplot(aes(x = age,y = friend_count),data = pf)+
  geom_jitter(alpha = 1/20)+
  xlim(13,90)

ggplot(aes(x = age,y = friend_count),data = pf)+
  geom_point(alpha = 1/20)+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

names(pf)
ggplot(aes(x = age,y = friendships_initiated),data = pf)+
  geom_point(alpha = 1/10,position = position_jitter(h = 0),
             color = 'orange')+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

age_group <- group_by(pf,age)
pf.fc_by_age <- summarise(age_group,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
pf.fc_by_age <- arrange(pf.fc_by_age,age)
head(pf.fc_by_age,20)

ggplot(aes(x = age,y = friend_count_mean),data = pf.fc_by_age)+
  geom_line()+
  xlim(13,90)

ggplot(aes(x = age,y = friendships_initiated),data = pf)+
  geom_point(alpha = 1/10,position = position_jitter(h = 0),
             color = 'orange')+
  coord_cartesian(xlim = c(13,70),ylim = c(0,1000))+
  geom_line(stat = 'summary',fun.y = mean)+
  geom_line(stat = 'summary',fun.y = quantile,probs = 0.1,linetype = 2,color = 'blue')+
  geom_line(stat = 'summary',fun.y = quantile,probs = 0.9,linetype = 3,color = 'blue')+
  geom_line(stat = 'summary',fun.y = quantile,probs = 0.5)
  
cor.test(pf$age,pf$friend_count,method = "pearson")
with(pf,cor.test(age,friend_count,method = "pearson"))
with(subset(pf,age<=70),cor.test(age,friend_count,method = "pearson"))

ggplot(aes(x = www_likes_received,y = likes_received),data = pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))+
  geom_smooth(method = "lm",color = "red")
cor.test(pf$www_likes_received,pf$likes_received)

ggplot(aes(y = Mitchell$Temp,x = Mitchell$Month),data = Mitchell)+
  geom_point()
cor.test(Mitchell$Month,Mitchell$Temp)

ggplot(aes(y = Mitchell$Temp,x = Mitchell$Month),data = Mitchell)+
  geom_point()+
  scale_x_discrete(breaks = seq(0,203,12))

ggplot(aes(x = age,y = friend_count_mean),data = pf.fc_by_age)+
  geom_line()

pf$age_with_months <- pf$age + (pf$dob_month/12)
age_with_months_group <- group_by(pf,age_with_months)
pf.fc_by_age_months <- summarise(age_with_months_group,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())

ggplot(aes(x = age_with_months,y = friend_count_mean),data = subset(pf.fc_by_age_months,age_with_months<71))+
  geom_line()

ggplot(aes(x = age_with_months,y = friend_count_mean),data = subset(pf.fc_by_age_months,age_with_months<71))+
  geom_line()+
  geom_smooth()

age_gender_group = group_by(subset(pf,!is.na(gender)),age,gender)

pf.fc_by_age_gender <- summarise(age_gender_group,
                                 mean_friend_count = mean(as.numeric(friend_count)),
                                 median_friend_count = median(as.numeric(friend_count)),
                                 n = n())
ungroup(age)
arrange(pf.fc_by_age_gender,age)
head(pf.fc_by_age_gender,20)

ggplot(aes(x = age,y = median_friend_count),data = pf.fc_by_age_gender)+
  geom_line(aes(color = gender))

library(reshape2)
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide,20)

ggplot(aes(x = age,y = female/male),data = pf.fc_by_age_gender.wide)+
  geom_line()+
  geom_hline(yintercept = 1,alpha = 0.3,linetype = 2)

pf$year_joined <- floor(2014-pf$tenure/365)
summary(pf$year_joined)

pf$year_joined.bucket <- cut(pf$year_joined,c(2004,2009,2011,2012,2014))

ggplot(aes(x = age,y = friend_count),
       data = subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = median)

ggplot(aes(x = age,y = friend_count),
       data = subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean)+
  geom_line(stat = 'summary',fun.y = mean,linetype = 2)

with(subset(pf,tenure >= 1),summary(friend_count/tenure))

ggplot(aes(x = tenure,y = friendships_initiated/tenure),
       data = subset(pf,tenure >= 1))+
  geom_line(aes(color = year_joined.bucket),stat = 'summary',fun.y = mean)

ggplot(aes(x = tenure,y = friendships_initiated/tenure),
       data = subset(pf,tenure >= 1))+
  geom_smooth(aes(color = year_joined.bucket))

yo <- read.csv('yogurt.csv')
str(yo)
yo$id = factor(yo$id)


qplot(data = yo,x = price,binwidth = 10)

yo <- transform(yo,all.purchases = strawberry+blueberry+pina.colada+plain+mixed.berry)
ggplot(aes(y = price,x = time),data = yo)+
  geom_jitter(alpha = 1/10)

set.seed(1800)
sample.ids <- sample(levels(yo$id),16)

ggplot(aes(x = time,y = price),data = subset(yo,id %in% sample.ids))+
  geom_line()+
  facet_wrap( ~id )+
  geom_point(aes(size = all.purchases,color = 'orange'),pch = 1)
  
cor.test(pf$friendships_initiated,pf$friend_count)
cor.test(pf$age,pf$mobile_likes)

data("diamonds")

ggplot(aes(x = carat,y = price),data = diamonds)+
  scale_x_continuous(lim = c(0,quantile(diamonds$carat,0.99)))+
  scale_y_continuous(lim = c(0,quantile(diamonds$price,0.99)))+
  geom_point(color = I('orange'))+
  stat_smooth(method = 'lm')

plot1 <- qplot(data = diamonds,x = price,binwidth = 100) + 
  ggtitle('Price')

plot2 <- qplot(data = diamonds,x = price,binwidth = 0.01) +
  scale_x_log10()+
  ggtitle('Price (log10)')

grid.arrange(plot1,plot2,ncol = 2)


cuberoot_trans = function() trans_new('cuberoot',
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(aes(x = carat,y = price),data = diamonds)+
  geom_point()+
  scale_x_continuous(trans = cuberoot_trans())