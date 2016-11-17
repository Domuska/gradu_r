

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
appium_frame$toolname <- "AA"
espresso_frame$toolname <- "EA"
robotium_frame$toolname <- "RA"
uiautomator_frame$toolname <- "UA"
tau_frame$toolname <- "TA"

combined_frame_amaze <- rbind(appium_frame, espresso_frame, robotium_frame, uiautomator_frame, tau_frame)

#save .png to directory below
setwd("C:/Gradu/gradu_r/pictures")
setwd("C:/users/Tomi/R/gradu_r/pictures")
png(filename="amaze_boxplot.png")

ggplot(combined_frame_amaze, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot() + 
  xlab("Name of the tool") + 
  ylab("Test set run time in seconds")

dev.off()



#means, medians, standard deviations for the run times

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

mean_appium
mean_espresso
mean_tau
mean_robotium
mean_uiautomator


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


#medians
appium_median <-median(appium_frame$runTime_seconds)
espresso_median <- median(espresso_frame$runTime_seconds)
robotium_median <- median(robotium_frame$runTime_seconds)
tau_median <- median(tau_frame$runTime_seconds)
uiautomator_median <- median(uiautomator_frame$runTime_seconds)

appium_median
espresso_median
robotium_median
tau_median
uiautomator_median

#percentages
appium_median / espresso_median * 100
robotium_median / espresso_median * 100
tau_median / espresso_median * 100
uiautomator_median / espresso_median * 100


#standard deviations
sd(appium_frame$runTime_seconds)
sd(espresso_frame$runTime_seconds)
sd(robotium_frame$runTime_seconds)
sd(tau_frame$runTime_seconds)
sd(uiautomator_frame$runTime_seconds)



#cohen's D for run time
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Gradu/gradu_r/effsize_0.6.4")


#calculate the cohen's d compared to the fastest tool
cohen.d(appium_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(espresso_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(robotium_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(uiautomator_frame$runTime_seconds, espresso_frame$runTime_seconds)
cohen.d(tau_frame$runTime_seconds, espresso_frame$runTime_seconds)

#other interesting cohen's d values
cohen.d(robotium_frame$runTime_seconds, tau_frame$runTime_seconds)


#calculate failures
print_fail_percentage <- function(test_frame){
  #get number of rows in the frame that has more than 0 failures (a new frame is created inside parantheses)
  frame_failures = nrow(test_frame[test_frame$failures > 0,])
  fail_percentage = frame_failures / nrow(test_frame) * 100
  fail_percentage
}


#########appium failure
appium_frame_f <- read.csv("appium_amaze.csv")
sum(appium_frame_f$failures > 0)
nrow(appium_frame_f)
print_fail_percentage(appium_frame_f)

#########espresso failure
espresso_frame_f <- read.csv("espresso_amaze.csv")
sum(espresso_frame_f$failures > 0)
nrow(espresso_frame_f)
print_fail_percentage(espresso_frame_f)

#########robotium failure
robotium_frame_f <- read.csv("robotium_amaze.csv")
sum(robotium_frame_f$failures > 0)
nrow(robotium_frame_f)
print_fail_percentage(robotium_frame_f)

#########uiautomator failure
uiautomator_frame_f <- read.csv("uiautomator_amaze.csv")
sum(uiautomator_frame_f$failures > 0)
nrow(uiautomator_frame_f)
print_fail_percentage(uiautomator_frame_f)

#########tau failure
tau_failures_n <- read.csv("tau_amaze_failures.csv")
tau_failures_n
nrow(tau_frame) + tau_failures_n
tau_failures_n / (nrow(tau_frame) + tau_failures_n) * 100











#wilcoxon tests, if they are wished at some point
library(MASS)
wilcox.test(appium_frame$runTime_seconds, espresso_frame$runTime_seconds)
wilcox.test(appium_frame$runTime_seconds, tau_frame$runTime_seconds)
wilcox.test(espresso_frame$runTime_seconds, robotium_frame$runTime_seconds)

