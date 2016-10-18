setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/amaze")
setwd("C:/Gradu/android_testing_results/combined_results/amaze")
library(ggplot2)
library(gridExtra)

#read frames, ignore rows with failures
appium_frame <- read.csv("appium_amaze.csv")
appium_frame <- appium_frame[!(appium_frame$failures != 0),]

espresso_frame <- read.csv("espresso_amaze.csv")
espresso_frame <- espresso_frame[!(espresso_frame$failures != 0),]

robotium_frame <- read.csv("robotium_amaze.csv")
robotium_frame <- robotium_frame[!(robotium_frame$failures != 0),]

uiautomator_frame <- read.csv("uiautomator_amaze.csv")
uiautomator_frame <- uiautomator_frame[!(uiautomator_frame$failures != 0),]

tau_frame <- read.csv("tau_amaze.csv")



#bar plots of individual results, combined with grid.arrange

appium_plot <- qplot(x = runTime_seconds, data = appium_frame, binwidth = 1, xlab = "Appium") +
  scale_x_continuous(limits = c(95, 415))

espresso_plot <- qplot(x = runTime_seconds, data = espresso_frame, binwidth = 1, xlab = "Espresso") +
  scale_x_continuous(limits = c(95, 415))

robotium_plot <- qplot(x = runTime_seconds, data = robotium_frame, binwidth = 1, xlab = "Robotium") +
  scale_x_continuous(limits = c(95, 415))

uiautomator_plot <- qplot(x = runTime_seconds, data = uiautomator_frame, binwidth = 1, xlab = "UiAutomator") + 
  scale_x_continuous(limits = c(95, 415))

tau_plot <- qplot(x = runTime_seconds, data = tau_frame, binwidth = 1, xlab = "Tau") + 
  scale_x_continuous(limits = c(95, 415))

grid.arrange(appium_plot, espresso_plot, robotium_plot, uiautomator_plot, tau_plot, ncol = 1)


#boxplots

#add tool names to the frames
appium_frame$toolname <- "Appium"
espresso_frame$toolname <- "Espresso"
robotium_frame$toolname <- "Robotium"
uiautomator_frame$toolname <- "UiAutomator"
tau_frame$toolname <- "Tau"

combined_frame <- rbind(appium_frame, espresso_frame, robotium_frame, uiautomator_frame, tau_frame)

ggplot(combined_frame, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot()



#means for the run times

mean_appium <- mean(appium_frame$runTime_seconds)
mean_appium <- as.numeric(as.character(mean_appium))

mean_espresso <- mean(espresso_frame$runTime_seconds)
mean_espresso <- as.numeric(as.character(mean_espresso))

mean_robotium <- mean(robotium_frame$runTime_seconds)
mean_robotium <- as.numeric(as.character(mean_robotium))

mean_uiautomator <- mean(uiautomator_frame$runTime_seconds)
mean_uiautomator <- as.numeric(as.character(mean_uiautomator))

mean_tau <- mean(tau_frame$runTime_seconds)
mean_tau <- as.numeric(as.character(mean_tau))


means = data.frame(toolname = character(), time = numeric(), stringsAsFactors = FALSE)
means[1,] <- c("Appium", mean_appium)
means = rbind(means, c("Espresso", mean_espresso))
means = rbind(means, c("Robotium", mean_robotium))
means = rbind(means, c("UiAutomator", mean_uiautomator))
means = rbind(means, c("Tau", mean_tau))

#plot from means, fill by tool name, y-axis does not look good, draw this with Excel?
ggplot(means, aes(x = toolname, y = time, fill = toolname)) + geom_bar(stat = "identity", width = .5) +
  scale_y_discrete("Run Time")
#scale_x_continuous(breaks = seq(10, 80, 5), limits = c(10,80))

ggplot(means, aes(x = toolname, y = time)) + geom_point(stat = "identity")


#cohen's D
library(effsize, lib.loc = "C:/Gradu/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")

#calculate the cohen's d compared to the fastest tool
cohen.d(appium_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(robotium_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(uiautomator_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(tau_frame$runTime_seconds, espresso_frame$runTime_seconds)

#other interesting cohen's d values
cohen.d(robotium_frame$runTime_seconds, tau_frame$runTime_seconds)
