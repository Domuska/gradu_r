#scripts for comparing the results of the native and webview wikipedia test runs, using wifi network
#variables are named as appium_xxx_n, or appium_xxx_w, n meaning for native wikipedia results and w for webview results

setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/wikipedia")
setwd("C:/Gradu/android_testing_results/combined_results/wikipedia")
library(ggplot2)
library(gridExtra)8

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
combined_frame_all_apps <- rbind(combined_frame_amaze, combined_frame_wikipedia_n, notes_combined)


#all network configurations combined
combined_frame_all_networks <- rbind(combined_frame_wikipedia_n, combined_frame_wikipedia_n_5g, combined_frame_wikipedia_n_4g)
                                     , levels = "AWN4", "EWN4", "TWN4", "UWN4")

#save .png to directory below
setwd("C:/Gradu/gradu_r/pictures")
setwd("C:/R/gradu_r/pictures")
png(filename="wikipedia_boxplot_native_wifi.png")

#boxplot from the results, change the combined frame below as wished
ggplot(combined_frame_all_networks, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Name of the tool") + 
  ylab("Test set run time in seconds")

dev.off()


#boxplot from all apps frames combined
png(filename="all_apps_boxplot_native_wifi.png", width = 1060)

ggplot(combined_frame_all_apps, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Name of the tool") + 
  ylab("Test set run time in seconds")

dev.off()



#means & medians for the run times, dont draw plot in R since it is much more work than with Excel

#WIFI
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

median(appium_frame_n$runTime_seconds)
median(espresso_frame_n$runTime_seconds)
median(tau_frame_n$runTime_seconds)
median(uiautomator_frame_n$runTime_seconds)

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


#4G means & medians
mean(appium_frame_n_4g$runTime_seconds)
mean(espresso_frame_n_4g$runTime_seconds)
mean(tau_frame_n_4g$runTime_seconds)
mean(uiautomator_frame_n_4g$runTime_seconds)

median(appium_frame_n_4g$runTime_seconds)
median(espresso_frame_n_4g$runTime_seconds)
median(tau_frame_n_4g$runTime_seconds)
median(uiautomator_frame_n_4g$runTime_seconds)

#cohen's D for run time
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Gradu/gradu_r/effsize_0.6.4")


#calculate the cohen's d compared to the fastest tool
#wifi
cohen.d(appium_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)
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


#calculate failures

print_fail_percentage <- function(test_frame){
  #get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
  frame_failures = nrow(test_frame[test_frame$failures > 0,])
  fail_percentage = frame_failures / nrow(test_frame) * 100
  fail_percentage
}

#WIFI

#first the frames with failures
appium_frame_n_f <- read.csv("appium_wifi_native.csv")
print_fail_percentage(appium_frame_n_f)

espresso_frame_n_f <- read.csv("espresso_wifi_native.csv")
print_fail_percentage(espresso_frame_n_f)

uiautomator_frame_n_f <- read.csv("uiautomator_wifi_native.csv")
print_fail_percentage(uiautomator_frame_n_f)

#tau failures are just a raw value in a csv
tau_failures_n <- read.csv("tau_wifi_native_failures.csv")
tau_failures_n / nrow(tau_frame_n) * 100

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
