setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/wikipedia")
setwd("C:/Gradu/android_testing_results/combined_results/wikipedia")
library(ggplot2)
library(gridExtra)

#read webview wikipedia frames, ignore rows with failures

#WIFI NETWORK
appium_frame_w <- read.csv("appium_wifi_webview.csv")
appium_frame_w <- appium_frame_w[!(appium_frame_w$failures != 0),]

espresso_frame_w <- read.csv("espresso_wifi_webview.csv")
espresso_frame_w <- espresso_frame_w[!(espresso_frame_w$failures != 0),]


#5G NETWORK
appium_frame_w_5g <- read.csv("appium_5g_webview.csv")
appium_frame_w_5g <- appium_frame_w_5g[!(appium_frame_w_5g$failures != 0),]

espresso_frame_w_5g <- read.csv("espresso_5g_webview.csv")
espresso_frame_w_5g <- espresso_frame_w_5g[!(espresso_frame_w_5g$failures != 0),]


#4G NETWORK
appium_frame_w_4g <- read.csv("appium_4g_webview.csv")
appium_frame_w_4g <- appium_frame_w_4g[!(appium_frame_w_4g$failures != 0),]

espresso_frame_w_4g <- read.csv("espresso_4g_webview.csv")
espresso_frame_w_4g <- espresso_frame_w_4g[!(espresso_frame_w_4g$failures != 0),]


#boxplots

#add tool names to the frames (Appium Wiki Native Wifi etc)
#wifi
appium_frame_w$toolname <- "AWWW"
espresso_frame_w$toolname <- "EWWW"


#5g
appium_frame_w_5g$toolname <- "AWN5"
espresso_frame_w_5g$toolname <- "EWW5"


#4g
appium_frame_w_4g$toolname <- "AWN4"
espresso_frame_w_4g$toolname <- "EWW4"

#combined frames
combined_frame_wikipedia_w <- rbind(appium_frame_w, espresso_frame_w)
combined_frame_wikipedia_w_5g <- rbind(appium_frame_w_5g, espresso_frame_w_5g)
combined_frame_wikipedia_w_4g <- rbind(appium_frame_w_4g, espresso_frame_w_4g)

#all networks combined
combined_frame_all_wetworks <- rbind(combined_frame_wikipedia_w, combined_frame_wikipedia_w_5g, combined_frame_wikipedia_w_4g)


#save .png to directory below
setwd("C:/Gradu/gradu_r/pictures")
setwd("C:/users/Tomi/R/gradu_r/pictures")

#boxplot from all apps frames combined

########## CHANGE NAME OF THE FILE ##########
png(filename="wikipedia_webview_allnetworks_2_boxplot.png", width = 1060)

#the scale_x_discrete and limits can be used to order of entries in x axis
plot = ggplot(combined_frame_all_wetworks, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Configuration") + 
  ylab("Test set run time in seconds")
#scale_x_discrete(limits = c("AWNW", "EWNW", "TWNW", "UWNW", "AWN5", "EWN5", "TWN5", "UWN5", "AWN4", "EWN4", "TWN4", "UWN4"))

#write out just the plot, no modifications, entries grouped by name
plot

#modify the plot, order x-axis values in certain order, use this when all network types are compared
plot + scale_x_discrete(limits = c("AWWW", "EWWW", "AWW5", "EWW5", "AWW4", "EWW4"))

dev.off()

