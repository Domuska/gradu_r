#scripts for comparing the results of the native and webview wikipedia test runs, using wifi network
#variables are named as appium_xxx_n, or appium_xxx_w, n meaning for native wikipedia results and w for webview results

setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/wikipedia")
setwd("C:/Gradu/android_testing_results/combined_results/wikipedia")
library(ggplot2)
library(gridExtra)

#read native wikipedia frames, ignore rows with failures
appium_frame_n <- read.csv("appium_wifi_native.csv")
appium_frame_n <- appium_frame_n[!(appium_frame_n$failures != 0),]

espresso_frame_n <- read.csv("espresso_wifi_native.csv")
espresso_frame_n <- espresso_frame_n[!(espresso_frame_n$failures != 0),]

uiautomator_frame_n <- read.csv("uiautomator_wifi_native.csv")
uiautomator_frame_n <- uiautomator_frame_n[!(uiautomator_frame_n$failures != 0),]

tau_frame_n <- read.csv("tau_wifi_native.csv")



#boxplots

#add tool names to the frames
appium_frame_n$toolname <- "Appium Wikipedia"
espresso_frame_n$toolname <- "Espresso Wikipedia"
uiautomator_frame_n$toolname <- "UiAutomator Wikipedia"
tau_frame_n$toolname <- "Tau Wikipedia"

combined_frame_wikipedia_n <- rbind(appium_frame_n, espresso_frame_n, uiautomator_frame_n, tau_frame_n)

#all 3 apps frames combined
combined_frame_all <- rbind(combined_frame_amaze, combined_frame_wikipedia_n, notes_combined)

#save .png to directory below
setwd("C:/Gradu/gradu_r/pictures")
setwd("C:/R/gradu_r/pictures")
png(filename="wikipedia_boxplot_native_wifi.png")

ggplot(combined_frame, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Name of the tool") + 
  ylab("Test set run time in seconds")

dev.off()


#boxplot from all apps frames combined
png(filename="all_apps_boxplot_native_wifi.png", width = 1060)

ggplot(combined_frame_all, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Name of the tool") + 
  ylab("Test set run time in seconds")

dev.off()



#means for the run times, dont draw plot in R since it it much more work than with Excel

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



#cohen's D for run time
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Gradu/gradu_r/effsize_0.6.4")


#calculate the cohen's d compared to the fastest tool
cohen.d(appium_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)
cohen.d(uiautomator_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)
cohen.d(tau_frame_n$runTime_seconds, espresso_frame_n$runTime_seconds)




#calculate failures
#first the frames with failures
appium_frame_n_f <- read.csv("appium_wifi_native.csv")
#get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
appium_failures_n = nrow(appium_frame_n_f[appium_frame_n_f$failures > 0,])


espresso_frame_n_f <- read.csv("espresso_wifi_native.csv")
#get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
espresso_failures_n = nrow(espresso_frame_n_f[espresso_frame_n_f$failures > 0,])

uiautomator_frame_n_f <- read.csv("uiautomator_wifi_native.csv")
#get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
uiautomator_failures_n = nrow(uiautomator_frame_n_f[uiautomator_frame_n_f$failures > 0,])

#tau failures are just a raw valuein a csv
tau_failures_n <- read.csv("tau_failures.csv")
  
espresso_failures_n
appium_failures_n
uiautomator_failures_n
tau_failures_n

fail_percentage_espresso_n = espresso_failures_n / nrow(espresso_frame_n_f) * 100
fail_percentage_appium_n = appium_failures_n / nrow(appium_frame_n_f) * 100
fail_percentage_uiautomator_n = uiautomator_failures_n / nrow(uiautomator_frame_n_f) * 100
fail_percentage_tau_n = tau_failures_n / nrow(tau_frame_n) * 100

fail_percentage_espresso_n
fail_percentage_appium_n
fail_percentage_uiautomator_n
fail_percentage_tau_n



