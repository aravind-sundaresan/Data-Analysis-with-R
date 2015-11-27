library(ggplot2)

data("diamonds")
summary(diamonds)

View(diamonds)
names(diamonds)

levels(diamonds$carat)
diamonds$color
diamonds$carat
diamonds$cut
diamonds$clarity
diamonds$depth
diamonds$table
diamonds$price
diamonds$x
diamonds$y
diamonds$z

library(ggplot2)
qplot(x = price,data = diamonds)
summary(diamonds$price < 250)

qplot(x = price,data = diamonds,binwidth = 100)+
  scale_x_continuous(limits = c(0,5000),breaks = seq(0,5000,500))
ggsave('histogram1.png')

qplot(x = price,data = diamonds,binwidth = 500)+
  scale_x_continuous(limits = c(0,5000),breaks = seq(0,5000,500))+
  facet_wrap(~cut,ncol = 5)

qplot(x = price,data = diamonds)+
  facet_wrap(~cut,ncol = 5)

by(diamonds$price,diamonds$cut,min)
View(diamonds)

qplot(x = price, data = diamonds,binwidth = 10) + 
  facet_wrap(~cut)+
  scale_x_log10()

qplot(y = price, x = color,data = subset(na.omit(diamonds)),
      geom = 'boxplot')+
  coord_cartesian(ylim = c(0,10000))

summary(diamonds$carat)
by(diamonds$price,diamonds$color,summary)


qplot(y = price/carat, x = color,data = subset(na.omit(diamonds)),
      geom = 'boxplot')

qplot(x = carat,data = subset(diamonds,!is.na(carat)),
      geom = 'freqpoly',binwidth = 0.01)+
  scale_x_continuous(limits = c(0,3),breaks = seq(0,3,0.1))

