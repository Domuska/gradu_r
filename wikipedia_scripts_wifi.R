#scripts for comparing the results of the native and webview wikipedia test runs, using wifi network

setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/wikipedia")
setwd("C:/Gradu/android_testing_results/combined_results/wikipedia")
library(ggplot2)
library(gridExtra)

#read frames, ignore rows with failures
appium_frame_n <- read.csv("appium_wifi_native.csv")
appium_frame_n <- appium_frame_n[!(appium_frame_n$failures != 0),]

espresso_frame_n <- read.csv("espresso_wifi_native.csv")
espresso_frame_n <- espresso_frame_n[!(espresso_frame_n$failures != 0),]

uiautomator_frame_n <- read.csv("uiautomator_wifi_native.csv")
uiautomator_frame_n <- uiautomator_frame_n[!(uiautomator_frame_n$failures != 0),]

tau_frame <- read.csv("tau_wifi_native.csv")

