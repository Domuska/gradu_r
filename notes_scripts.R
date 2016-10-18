setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/notes")
library(ggplot2)
library(gridExtra)

#munch munch, clean rows with failures from the results
notes_appium <- read.csv("appium_notes.csv")
notes_appium <- notes_appium[!(notes_appium$failures != 0),]
notes_appium$toolname <- "Appium"

notes_espresso <- read.csv("espresso_notes.csv")
notes_espresso <- notes_espresso[!(notes_espresso$failures != 0),]
notes_espresso$toolname <- "Espresso"

notes_robotium <- read.csv("robotium_notes.csv")
notes_robotium <- notes_robotium[!(notes_robotium$failures != 0),]
notes_robotium$toolname <- "Robotium"

notes_uiautomator <- read.csv("uiautomator_notes.csv")
notes_uiautomator <- notes_uiautomator[!(notes_uiautomator$failures != 0),]
notes_uiautomator$toolname <- "UiAutomator"

notes_tau <- read.csv("tau_notes.csv")
notes_tau <- notes_tau[!(notes_tau$failures != 0),]
notes_tau$toolname <- "Tau"

#data together
notes_combined <- rbind(notes_appium, notes_espresso, 
                        notes_robotium, notes_uiautomator, notes_tau)


#bar plots of the individual results
appium_runtime <- qplot(x = runTime_seconds, data = subset(notes_appium, failures == 0),
                        binwidth = 1, xlab = "Appium") + 
  scale_x_continuous(limits = c(72, 560))
espresso_runtime <- qplot(x = runTime_seconds, data = notes_espresso, binwidth = 1, xlab = "Espresso") +
  scale_x_continuous(limits = c(72, 560))
robotium_runtime <- qplot(x = runTime_seconds, data = subset(notes_robotium, failures == 0),
                          binwidth = 1, xlab = "Robotium") +
  scale_x_continuous(limits = c(72, 560))
tau_runtime <- qplot(x = runTime_seconds, data = notes_tau, binwidth = 1, xlab = "Tau") +
  scale_x_continuous(limits = c(72, 560))
uiautomator_runtime <- qplot(x = runTime_seconds, data = notes_uiautomator, binwidth = 1, xlab = "uiautomator") +
  scale_x_continuous(limits = c(72, 560))

grid.arrange(appium_runtime, espresso_runtime, robotium_runtime, tau_runtime, uiautomator_runtime, ncol = 1)

#boxplot from all the tools
qplot(x = toolname, y = runTime_seconds, data = notes_combined, geom = "boxplot",
      xlab = "Name of the tool",
      ylab = "Total test run time (seconds)")
#same with ggplot
ggplot(notes_combined, aes(x = toolname, y = runTime_seconds)) +
  geom_boxplot()

#ei kovin hyödyllinen...
ggplot(notes_espresso, aes(x = runTime_seconds)) + geom_histogram()


#means for the run times

mean_espresso <- mean(notes_espresso$runTime_seconds)
mean_espresso <- as.numeric(as.character(mean_espresso))

mean_appium <- mean(notes_appium$runTime_seconds)
mean_appium <- as.numeric(as.character(mean_appium))

mean_robotium <- mean(notes_robotium$runTime_seconds)
mean_robotium <- as.numeric(as.character(mean_robotium))

mean_uiautomator <- mean(notes_uiautomator$runTime_seconds)
mean_uiautomator <- as.numeric(as.character(mean_uiautomator))

mean_tau <- mean(notes_tau$runTime_seconds)
mean_tau <- as.numeric(as.character(mean_tau))


#tehdään uusi dataframe
means = data.frame(toolname = character(), time = numeric(), stringsAsFactors = FALSE)
#lisätään eka rivi, pitää tehä eri tavalla
#http://stackoverflow.com/questions/12614397/how-to-add-rows-to-empty-data-frames-with-header-in-r
means[1, ] <- c("Espresso", mean_espresso)
#lisätään frameen muut rivit
means = rbind(means, c("Appium", mean_appium))
means = rbind(means, c("Robotium", mean_robotium))
means = rbind(means, c("UiAutomator", mean_uiautomator))
means = rbind(means, c("Tau", mean_tau))
#convert time column to numeric
means[, 2] <- as.data.frame(sapply(means[, 2], as.numeric))
#means$time <- as.numeric(as.character(means$time))
#means[,'time'] <- as.numeric(as.character(means[,'time']))

#histogrammi meaneista
ggplot(means, aes(x = toolname, y = time)) + geom_bar(stat = "identity")

qplot(x = toolname, y = time, data = means) + geom_histogram(binwidth = 1)


#cohen's d

#install effsize for cohen's d etc, change path to where you have the git repo
install.packages("effsize", lib = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
library(effsize, lib.loc = "C:/Users/Tomi/R/gradu_r/effsize_0.6.4")
#https://cran.r-project.org/web/packages/effsize/effsize.pdf

#appium_espresso <- rbind(notes_appium, notes_espresso)
appium_espresso <- data.frame(notes_appium$toolname, notes_appium$runTime_seconds)
#appium_espresso <- cbind(notes_espresso$toolname, notes_espresso$runTime_seconds)
appium_espresso <- rbind(appium_espresso, notes_espresso[1,])

appium_espresso <- read.csv("appium_espresso.csv")

#en tiiä mitä tällä yritetään, ei toimi
cohen.d(d = mean(appium_espresso$runTime_seconds), f = appium_espresso$runTime_seconds)
#cohenin d:n saa varmaan tällä, ei oikeesti:
cohen.d(f = notes_espresso$runTime_seconds, d = notes_appium$runTime_seconds)



#tehty kattomalla dokumentaation esimerkkiä
d = (c(notes_appium$runTime_seconds, notes_espresso$runTime_seconds))
f = rep(c("Appium","Espresso"), each=50)

#REAL STUFFS ARE IN HERE calculate cohen's d
cohen.d(notes_appium$runTime_seconds, notes_espresso$runTime_seconds)
cohen.d(notes_tau$runTime_seconds, notes_robotium$runTime_seconds)
cohen.d(notes_uiautomator$runTime_seconds, notes_tau$runTime_seconds)
cohen.d(notes_robotium$runTime_seconds, notes_espresso$runTime_seconds)
cohen.d(notes_tau$runTime_seconds, notes_robotium$runTime_seconds)
#data and factor
cohen.d(d,f)



