


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
appium_frame_w_5g$runTime_seconds <- as.numeric(as.character(appium_frame_w_5g$runTime_seconds))
#have to convert above to numeric since it's not read properly. R please.


espresso_frame_w_5g <- read.csv("espresso_5g_webview.csv")
espresso_frame_w_5g <- espresso_frame_w_5g[!(espresso_frame_w_5g$failures != 0),]


#4G NETWORK
appium_frame_w_4g <- read.csv("appium_4g_webview.csv")
appium_frame_w_4g <- appium_frame_w_4g[!(appium_frame_w_4g$failures != 0),]

espresso_frame_w_4g <- read.csv("espresso_4g_webview.csv", sep=";")
espresso_frame_w_4g <- espresso_frame_w_4g[!(espresso_frame_w_4g$failures != 0),]


#boxplots

#add tool names to the frames (Appium Wiki Native Wifi etc)
#wifi
appium_frame_w$toolname <- "AWWW"
espresso_frame_w$toolname <- "EWWW"

#5g
appium_frame_w_5g$toolname <- "AWW5"
espresso_frame_w_5g$toolname <- "EWW5"

#4g
appium_frame_w_4g$toolname <- "AWW4"
espresso_frame_w_4g$toolname <- "EWW4"

#combined frames
combined_frame_wikipedia_w <- rbind(appium_frame_w, espresso_frame_w)
combined_frame_wikipedia_w_5g <- rbind(appium_frame_w_5g, espresso_frame_w_5g)
combined_frame_wikipedia_w_4g <- rbind(appium_frame_w_4g, espresso_frame_w_4g)

#all networks combined
combined_frame_all_networks <- rbind(combined_frame_wikipedia_w, combined_frame_wikipedia_w_5g, combined_frame_wikipedia_w_4g)


#save .png to directory below
setwd("C:/Gradu/gradu_r/pictures")
setwd("C:/users/Tomi/R/gradu_r/pictures")

#boxplot from all apps frames combined

########## CHANGE NAME OF THE FILE ##########
png(filename="wikipedia_webview_allnetworks_2_boxplot.png")

#the scale_x_discrete and limits can be used to order of entries in x axis
plot = ggplot(combined_frame_all_networks, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Configuration") + 
  ylab("Test set run time in seconds")


#write out just the plot, no modifications, entries grouped by name
plot

#modify the plot, order x-axis values in certain order, use this when all network types are compared
plot + scale_x_discrete(limits = c("AWWW", "EWWW", "AWW5", "EWW5", "AWW4", "EWW4"))

dev.off()


#means, medians, standard deviations

mean(appium_frame_w$runTime_seconds)
mean(appium_frame_w_4g$runTime_seconds)
mean(appium_frame_w_5g$runTime_seconds)

mean(espresso_frame_w$runTime_seconds)
mean(espresso_frame_w_4g$runTime_seconds)
mean(espresso_frame_w_5g$runTime_seconds)

median(appium_frame_w$runTime_seconds)
median(appium_frame_w_4g$runTime_seconds)
median(appium_frame_w_5g$runTime_seconds)

median(espresso_frame_w$runTime_seconds)
median(espresso_frame_w_4g$runTime_seconds)
median(espresso_frame_w_5g$runTime_seconds)

sd(appium_frame_w$runTime_seconds)
sd(appium_frame_w_4g$runTime_seconds)
sd(appium_frame_w_5g$runTime_seconds)

sd(espresso_frame_w$runTime_seconds)
sd(espresso_frame_w_4g$runTime_seconds)
sd(espresso_frame_w_5g$runTime_seconds)



#cohen's D for run times
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Gradu/gradu_r/effsize_0.6.4")

#työkalujen vertailua
cohen.d(appium_frame_w$runTime_seconds, espresso_frame_w$runTime_seconds)

#verkkojen vertailua
cohen.d(appium_frame_w_4g$runTime_seconds, appium_frame_w$runTime_seconds)
cohen.d(appium_frame_w_5g$runTime_seconds, appium_frame_w$runTime_seconds)

cohen.d(espresso_frame_w$runTime_seconds, espresso_frame_w_4g$runTime_seconds)
cohen.d(espresso_frame_w_5g$runTime_seconds, espresso_frame_w_4g$runTime_seconds)




#failure calculations

#function takes in a frame that has all rows recorded, will get rows with failures in them and
#calculate percentage of failed test runs
print_fail_percentage <- function(test_frame){
  #get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
  frame_failures = nrow(test_frame[test_frame$failures > 0,])
  fail_percentage = frame_failures / nrow(test_frame) * 100
  fail_percentage
}


#wifi failures
appium_frame_w_f <- read.csv("appium_wifi_webview.csv")
espresso_frame_w_f <- read.csv("espresso_wifi_webview.csv")

print_fail_percentage(appium_frame_w_f)
print_fail_percentage(espresso_frame_w_f)

#5G failures
appium_frame_w_5g_f <- read.csv("appium_5g_webview.csv")
espresso_frame_w_5g_f <- read.csv("espresso_5g_webview.csv")

print_fail_percentage(appium_frame_w_5g_f)
print_fail_percentage(espresso_frame_w_5g_f)


#4G failures
appium_frame_n_4g_f <- read.csv("appium_4g_native.csv")
espresso_frame_n_4g_f <- read.csv("espresso_4g_native.csv")

print_fail_percentage(appium_frame_n_4g_f)
print_fail_percentage(espresso_frame_n_4g_f)









