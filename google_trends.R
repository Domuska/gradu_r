setwd("C:/Gradu/gradu_r/google_trends_data")
library(ggplot2)

trend_popular <- read.csv("trends_worldwide_2012-2016_3.csv")
trend_iterated <- read.csv("trends_worldwide_no_iteration_2012-2016_3.csv")
trend_iterated$Week <- as.numeric(as.character(trend_iterated$Week))
trend_iterated$Week <- as.Date(trend_iterated$Week, format = "%d.%m.%y")

random_data <- read.csv("random_data.csv")

#betterDates <- as.Date(dates,
#format = "%m/%d/%y")

#popular tools
qplot(x = Week, data = trend_popular)

ggplot(data = trend_popular, aes(x = Week, y = ))




#esimerkki SO:sta
#http://stackoverflow.com/questions/14860078/plot-multiple-lines-data-series-each-with-unique-color-in-r
set.seed(45)
df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                 variable=rep(paste0("category", 1:9), each=5))
# plot
ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable))

ggplot(data = trend_iterated, aes(x = Week, y = frequency)) + geom_line(aes(colour=toolName))
ggplot(data = random_data, aes(x = Week, y = frequency)) + geom_line(aes(colour = toolName))
