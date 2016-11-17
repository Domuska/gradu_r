#scripts for comparing the results of the native and webview wikipedia test runs, using wifi network
#variables are named as appium_xxx_n, or appium_xxx_w, n meaning for native wikipedia results and w for webview results

setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/wikipedia")
setwd("C:/Gradu/android_testing_results/combined_results/wikipedia")
library(ggplot2)
library(gridExtra)

#read native wikipedia frames, ignore rows with failures

#WIFI NETWORK
appium_frame_n <- read.csv("appium_wifi_native.csv")
appium_frame_n <- appium_frame_n[!(appium_frame_n$failures != 0),]

espresso_frame_n <- read.csv("espresso_wifi_native.csv")
espresso_frame_n <- espresso_frame_n[!(espresso_frame_n$failures != 0),]

uiautomator_frame_n <- read.csv("uiautomator_wifi_native.csv")
uiautomator_frame_n <- uiautomator_frame_n[!(uiautomator_frame_n$failures != 0),]

tau_frame_n <- read.csv("tau_wifi_native.csv")


#5G NETWORK
appium_frame_n_5g <- read.csv("appium_5g_native.csv")
appium_frame_n_5g <- appium_frame_n_5g[!(appium_frame_n_5g$failures != 0),]

espresso_frame_n_5g <- read.csv("espresso_5g_native.csv")
espresso_frame_n_5g <- espresso_frame_n_5g[!(espresso_frame_n_5g$failures != 0),]

uiautomator_frame_n_5g <- read.csv("uiautomator_5g_native.csv")
uiautomator_frame_n_5g <- uiautomator_frame_n_5g[!(uiautomator_frame_n_5g$failures != 0),]

tau_frame_n_5g <- read.csv("tau_5g_native.csv")


#4G NETWORK
appium_frame_n_4g <- read.csv("appium_4g_native.csv")
appium_frame_n_4g <- appium_frame_n_4g[!(appium_frame_n_4g$failures != 0),]

espresso_frame_n_4g <- read.csv("espresso_4g_native.csv")
espresso_frame_n_4g <- espresso_frame_n_4g[!(espresso_frame_n_4g$failures != 0),]

uiautomator_frame_n_4g <- read.csv("uiautomator_4g_native.csv")
uiautomator_frame_n_4g <- uiautomator_frame_n_4g[!(uiautomator_frame_n_4g$failures != 0),]

tau_frame_n_4g <- read.csv("tau_4g_native.csv")

#some geom bars of the results
qplot(x = runTime_seconds, data = appium_frame_n, binwidth = 5)
qplot(x = runTime_seconds, data = espresso_frame_n, binwidth = 1)
t.test(appium_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)

#uiautomator
qplot(x = runTime_seconds, data = uiautomator_frame_n_4g, binwidth = 10)
qplot(x = runTime_seconds, data = uiautomator_frame_n_5g, binwidth = 10)
t.test(uiautomator_frame_n_4g$runTime_seconds, uiautomator_frame_n_5g$runTime_seconds)

#espresso
qplot(x = runTime_seconds, data = espresso_frame_n, binwidth = 1)
qplot(x = runTime_seconds, data = espresso_frame_n_5g, binwidth = 1)
t.test(espresso_frame_n$runTime_seconds, espresso_frame_n_5g$runTime_seconds)

#tau, interesting plot on 4g
qplot(x = runTime_seconds, data = tau_frame_n, binwidth = 5)
qplot(x = runTime_seconds, data = tau_frame_n_4g, binwidth = 5)
t.test(tau_frame_n_5g$runTime_seconds, tau_frame_n$runTime_seconds)

#boxplots

#add tool names to the frames (Appium Wiki Native Wifi etc)
#wifi
appium_frame_n$toolname <- "AWNW"
espresso_frame_n$toolname <- "EWNW"
uiautomator_frame_n$toolname <- "UWNW"
tau_frame_n$toolname <- "TWNW"

#5g
appium_frame_n_5g$toolname <- "AWN5"
espresso_frame_n_5g$toolname <- "EWN5"
uiautomator_frame_n_5g$toolname <- "UWN5"
tau_frame_n_5g$toolname <- "TWN5"

#4g
appium_frame_n_4g$toolname <- "AWN4"
espresso_frame_n_4g$toolname <- "EWN4"
uiautomator_frame_n_4g$toolname <- "UWN4"
tau_frame_n_4g$toolname <- "TWN4"


combined_frame_wikipedia_n <- rbind(appium_frame_n, espresso_frame_n, uiautomator_frame_n, tau_frame_n)

combined_frame_wikipedia_n_5g <- rbind(appium_frame_n_5g, espresso_frame_n_5g, uiautomator_frame_n_5g, tau_frame_n_5g)

combined_frame_wikipedia_n_4g <- rbind(appium_frame_n_4g, espresso_frame_n_4g, uiautomator_frame_n_4g, tau_frame_n_4g)

#all 3 apps frames combined
combined_frame_all_apps <- rbind(combined_frame_amaze, combined_frame_wikipedia_n, combined_frame_wikipedia_w, combined_frame_notes)


#all network configurations combined
combined_frame_all_networks <- rbind(combined_frame_wikipedia_n, combined_frame_wikipedia_n_5g, combined_frame_wikipedia_n_4g)
combined_frame_wifi_4g_networks <- rbind(combined_frame_wikipedia_n, combined_frame_wikipedia_n_4g)



#save .png to directory below
setwd("C:/Gradu/gradu_r/pictures")
setwd("C:/users/Tomi/R/gradu_r/pictures")

#boxplot from all apps frames combined

########## CHANGE NAME OF THE FILE ##########
png(filename="wikipedia_native_wifi_4g_boxplot.png")

#the scale_x_discrete and limits can be used to order of entries in x axis
plot = ggplot(combined_frame_wifi_4g_networks, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Configuration") + 
  ylab("Test set run time in seconds")
  #scale_x_discrete(limits = c("AWNW", "EWNW", "TWNW", "UWNW", "AWN5", "EWN5", "TWN5", "UWN5", "AWN4", "EWN4", "TWN4", "UWN4"))

#write out just the plot, no modifications, entries grouped by name
plot

#modify the plot, order x-axis values in certain order, use this when all network types are compared
plot + scale_x_discrete(limits = c("AWNW", "EWNW", "TWNW", "UWNW", "AWN5", "EWN5", "TWN5", "UWN5", "AWN4", "EWN4", "TWN4", "UWN4"))

dev.off()



#means & medians & standard deviations for the run times, dont draw plot in R since it is much more work than with Excel

#WIFI means & medians
mean_appium_n <- mean(appium_frame_n$runTime_seconds)
mean_appium_n <- as.numeric(as.character(mean_appium_n))

mean_espresso_n <- mean(espresso_frame_n$runTime_seconds)
mean_espresso_n <- as.numeric(as.character(mean_espresso_n))

mean_uiautomator_n <- mean(uiautomator_frame_n$runTime_seconds)
mean_uiautomator_n <- as.numeric(as.character(mean_uiautomator_n))

mean_tau_n <- mean(tau_frame_n$runTime_seconds)
mean_tau_n <- as.numeric(as.character(mean_tau_n))

mean_appium_n
mean_espresso_n
mean_tau_n
mean_uiautomator_n

appium_wifi_median <- median(appium_frame_n$runTime_seconds)
espresso_wifi_median <- median(espresso_frame_n$runTime_seconds)
tau_wifi_median <- median(tau_frame_n$runTime_seconds)
uiautomator_wifi_median <- median(uiautomator_frame_n$runTime_seconds)

appium_wifi_median
espresso_wifi_median
tau_wifi_median
uiautomator_wifi_median

#calculate percentages for median tool execution speed
appium_wifi_median / espresso_wifi_median * 100
tau_wifi_median / espresso_wifi_median * 100
uiautomator_wifi_median / espresso_wifi_median * 100


sd(appium_frame_n$runTime_seconds)
sd(espresso_frame_n$runTime_seconds)
sd(tau_frame_n$runTime_seconds)
sd(uiautomator_frame_n$runTime_seconds)

#5G means & medians
mean_appium_n_5g <- mean(appium_frame_n_5g$runTime_seconds)
mean_espresso_n_5g <- mean(espresso_frame_n_5g$runTime_seconds)
mean_uiautomator_n_5g <- mean(uiautomator_frame_n_5g$runTime_seconds)
mean_tau_n_5g <- mean(tau_frame_n_5g$runTime_seconds)

mean_appium_n_5g
mean_espresso_n_5g
mean_tau_n_5g
mean_uiautomator_n_5g

median(appium_frame_n_5g$runTime_seconds)  
median(espresso_frame_n_5g$runTime_seconds)  
median(tau_frame_n_5g$runTime_seconds)  
median(uiautomator_frame_n_5g$runTime_seconds)

sd(appium_frame_n_5g$runTime_seconds)  
sd(espresso_frame_n_5g$runTime_seconds)  
sd(tau_frame_n_5g$runTime_seconds)  
sd(uiautomator_frame_n_5g$runTime_seconds)


#4G means & medians
mean(appium_frame_n_4g$runTime_seconds)
mean(espresso_frame_n_4g$runTime_seconds)
mean(tau_frame_n_4g$runTime_seconds)
mean(uiautomator_frame_n_4g$runTime_seconds)

appium_4g_median <- median(appium_frame_n_4g$runTime_seconds)
espresso_4g_median <- median(espresso_frame_n_4g$runTime_seconds)
tau_4g_median <- median(tau_frame_n_4g$runTime_seconds)
uiautomator_4g_median <- median(uiautomator_frame_n_4g$runTime_seconds)

appium_4g_median
espresso_4g_median
tau_4g_median
uiautomator_4g_median

sd(appium_frame_n_4g$runTime_seconds)
sd(espresso_frame_n_4g$runTime_seconds)
sd(tau_frame_n_4g$runTime_seconds)
sd(uiautomator_frame_n_4g$runTime_seconds)

#percentage differences
appium_4g_median / espresso_4g_median * 100
tau_4g_median / espresso_4g_median * 100
uiautomator_4g_median / espresso_4g_median * 100


#other interesting means and medians

#compare medians of all network types against each other
median(combined_frame_wikipedia_n$runTime_seconds)
median(combined_frame_wikipedia_n_5g$runTime_seconds)
median(combined_frame_wikipedia_n_4g$runTime_seconds)

mean(combined_frame_wikipedia_n$runTime_seconds)
mean(combined_frame_wikipedia_n_5g$runTime_seconds)
mean(combined_frame_wikipedia_n_4g$runTime_seconds)



#cohen's D for run times
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Gradu/gradu_r/effsize_0.6.4")


#calculate the cohen's d compared to the fastest tool
#wifi
cohen.d(appium_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)
cohen.d(espresso_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)
cohen.d(uiautomator_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)
cohen.d(tau_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)

#5g
cohen.d(appium_frame_n_5g$runTime_seconds, espresso_frame_n_5g$runTime_seconds)
cohen.d(espresso_frame_n_5g$runTime_seconds, espresso_frame_n_5g$runTime_seconds)
cohen.d(uiautomator_frame_n_5g$runTime_seconds, espresso_frame_n_5g$runTime_seconds)
cohen.d(tau_frame_n_5g$runTime_seconds, espresso_frame_n_5g$runTime_seconds)


#4g
cohen.d(appium_frame_n_4g$runTime_seconds, espresso_frame_n_4g$runTime_seconds)
cohen.d(espresso_frame_n_4g$runTime_seconds, espresso_frame_n_4g$runTime_seconds)
cohen.d(uiautomator_frame_n_4g$runTime_seconds, espresso_frame_n_4g$runTime_seconds)
cohen.d(tau_frame_n_4g$runTime_seconds, espresso_frame_n_4g$runTime_seconds)

#networks vs eachother
cohen.d(uiautomator_frame_n_4g$runTime_seconds, uiautomator_frame_n_5g$runTime_seconds)



#calculate failures

#function takes in a frame that has all rows recorded, will get rows with failures in them and
#calculate percentage of failed test runs
print_fail_percentage <- function(test_frame){
  #get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
  frame_failures = nrow(test_frame[test_frame$failures > 0,])
  fail_percentage = frame_failures / nrow(test_frame) * 100
  fail_percentage
}

#WIFI

#uiautomator_frame_n <- uiautomator_frame_n[!(uiautomator_frame_n$failures != 0),]

#first the frames with failures
########appium failure
appium_frame_n_f <- read.csv("appium_wifi_native.csv")
sum(appium_frame_n_f$failures > 0)
nrow(appium_frame_n_f)
print_fail_percentage(appium_frame_n_f)

#########espresso failure
espresso_frame_n_f <- read.csv("espresso_wifi_native.csv")
sum(espresso_frame_n_f$failures > 0)
nrow(espresso_frame_n_f)
print_fail_percentage(espresso_frame_n_f)

########uiautomator failure
uiautomator_frame_n_f <- read.csv("uiautomator_wifi_native.csv")
sum(uiautomator_frame_n_f$failures > 0)
nrow(uiautomator_frame_n_f)
print_fail_percentage(uiautomator_frame_n_f)

########tau failure
#tau frame does not have rows with failed tests because how the tests were run
tau_failures_n <- read.csv("tau_wifi_native_failures.csv")
tau_failures_n
nrow(tau_frame_n) + tau_failures_n
tau_failures_n / (nrow(tau_frame_n) + tau_failures_n) * 100

#total number of failures
espresso_failures_n
appium_failures_n
uiautomator_failures_n
tau_failures_n



#5G failures
appium_frame_n_5g_f <- read.csv("appium_5g_native.csv")
espresso_frame_n_5g_f <- read.csv("espresso_5g_native.csv")
uiautomator_frame_n_5g_f <- read.csv("uiautomator_5g_native.csv")
tau_failures_n_5g <- read.csv("tau_5g_native_failures.csv")

print_fail_percentage(appium_frame_n_5g_f)
print_fail_percentage(espresso_frame_n_5g_f)
print_fail_percentage(uiautomator_frame_n_5g_f)
tau_failures_n_5g / nrow(tau_frame_n_5g) * 100



#4G failures
appium_frame_n_4g_f <- read.csv("appium_4g_native.csv")
espresso_frame_n_4g_f <- read.csv("espresso_4g_native.csv")
uiautomator_frame_n_4g_f <- read.csv("uiautomator_4g_native.csv")
tau_failures_n_4g <- read.csv("tau_4g_native_failures.csv")

print_fail_percentage(appium_frame_n_4g_f)
print_fail_percentage(espresso_frame_n_4g_f)
print_fail_percentage(uiautomator_frame_n_4g_f)
tau_failures_n_4g / nrow(tau_frame_n_4g) * 100
