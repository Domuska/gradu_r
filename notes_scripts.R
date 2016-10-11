setwd("C:/Users/Tomi/testAutomation/measurements/combined_results/notes")
library(ggplot2)
library(gridExtra)


#munch munch
notes_appium <- read.csv("appium_notes.csv")
notes_appium$toolname <- "Appium"

notes_espresso <- read.csv("espresso_notes.csv")
notes_espresso$toolname <- "Espresso"

notes_robotium <- read.csv("robotium_notes.csv")
notes_robotium$toolname <- "Robotium"

notes_uiautomator <- read.csv("uiautomator_notes.csv")
notes_uiautomator$toolname <- "UiAutomator"

notes_tau <- read.csv("tau_notes.csv")
notes_tau$toolname <- "Tau"

#data together
notes_combined <- rbind(notes_appium, notes_espresso, 
                        notes_robotium, notes_uiautomator, notes_tau)

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
#koitetaan muokata numeroiksi muuttuja (voivat olla faktoreita), mutta tämä ei ilmeisesti auta
#numeric_espresso <- mean(notes_espresso$runTime_seconds)
#numeric_espresso <- as.numeric(as.character(numeric_espresso))
#mean_espresso <- numeric_espresso
#mean_espresso <- mean(as.numeric(levels(notes_espresso$runTime_seconds)))

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
means[1, ] <- c("Espresso", 78.7586)
#lisätään frameen muut rivit
means = rbind(means, c("Appium", 506.3224))
means = rbind(means, c("Robotium", 172.906))
means = rbind(means, c("UiAutomator", 299.916))
means = rbind(means, c("Tau", 296.1730769))
#convert time column to numeric
means[, 2] <- as.data.frame(sapply(means[, 2], as.numeric))
means$time <- as.numeric(as.character(means$time))
means[,'time'] <- as.numeric(as.character(means[,'time']))

#histogrammi meaneista, jostain syystä espresso on väärässä paikassa
ggplot(means, aes(x = toolname, y = time)) + geom_bar(stat = "identity")

qplot(x = toolname, y = time, data = means) + geom_histogram(binwidth = 1)

