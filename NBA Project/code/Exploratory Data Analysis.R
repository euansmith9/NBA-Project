setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)

data <- read.csv("finaldata.csv", stringsAsFactors = TRUE)

data$gmDate <- as.Date(as.character(data$gmDate), format = "%Y-%m-%d")

colnames(data)

ggplot(data, aes(x=teamPTS)) +
  geom_histogram(aes(y=after_stat(density)), alpha = 0.8, binwidth=1) +
  geom_density(alpha = 0.5, adjust=1.5, aes(y=after_stat(density))) +
  labs(title="Distribution of Team Points since 1983/84 Season",
       x="Points Scored", y="Density")

summary(data$teamPTS)
var(data$teamPTS)
summary(data$team3PM)
summary(data$team3PA)

ggplot(data, aes(x=teamFGM)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 1.8) +
  labs(title="Distribution of Field Goals Made since 1983/84", x="Field Goals Made", y="Frequency")

ggplot(data, aes(x=decade, y=teamPTS, group=decade)) +
  geom_boxplot() +
  labs(title="NBA Scoring by Decade", x="Decade", y="Points Scored") +
  theme_minimal()

dataElo <- data %>%
  group_by(year) %>%
  summarise(Avg3PA = mean(data$team3PA),
            AvgElo = mean(data$teamEloPre))

data80s <- subset(data, decade == "1980s")
data90s <- subset(data, decade == "1990s")
data00s <- subset(data, decade == "2000s")
data10s <- subset(data, decade == "2010s")
data20s <- subset(data, decade == "2020s")
data15a <- subset(data10s, year >= 2015)
data15b <- subset(data10s, year < 2015)
data16 <- subset(data, year == "2016")
dataGSW <- subset(data10s, teamAbbr == "GSW")

summary(data80s$team3PM)
summary(data90s$team3PM)
summary(data00s$team3PM)
summary(data10s$team3PM)
summary(data15a$team3PM)
summary(data15b$team3PM)

data3PA <- data %>%
  group_by(year) %>%
  summarise(Avg3PA = mean(team3PA))

dataPTS <- data %>%
  group_by(year) %>%
  summarise(AvgPTS = mean(teamPTS))

ggplot(data3PA, aes(x=year, y=Avg3PA)) +
  geom_point() +
  geom_smooth(se = FALSE, method="loess", color="blue") +  
  labs(title="NBA 3-Point Shooting Trend Over the Years", x="Year", y="Average 3-Point Shots Attempted per Game") +
  scale_x_continuous(breaks=seq(min(1980), max(2020), by=5)) +
  theme_minimal()


ggplot(dataPTS, aes(x=year, y=AvgPTS)) +
  geom_point() +
  geom_smooth(se = FALSE, method="loess", color="blue") +  
  labs(title="NBA Points Scoring Trend Over the Years", x="Year", y="Average Points per Game") +
  scale_x_continuous(breaks=seq(min(1980), max(2020), by=5)) +
  theme_minimal()

data3PM <- data %>%
  group_by(year) %>%
  summarise(Avg3PM = mean(team3PM))

ggplot(data3PM, aes(x=year, y=Avg3PM)) +
  geom_point() +
  geom_smooth(se = FALSE, method="loess", color="blue") +  
  labs(title="NBA 3-Point Shooting Trend Over the Years", x="Year", y="Average 3-Point Shots Made per Game") +
  scale_x_continuous(breaks=seq(min(1980), max(2020), by=5)) +
  theme_minimal()

data3P. <- data %>%
  group_by(year) %>%
  summarise(Avg3P. = mean(team3P.))

ggplot(data3P., aes(x=year, y=Avg3P.)) +
  geom_point() +
  geom_smooth(se = FALSE, method="loess", color="blue") +  
  labs(title="NBA 3-Point Shooting Trend Over the Years", x="Year", y="Average 3-Point Percentage") +
  scale_x_continuous(breaks=seq(min(1980), max(2020), by=5)) +
  theme_minimal()

data10s3PA <- data10s %>%
  group_by(year) %>%
  summarise(Avg3PA = mean(team3PA))

ggplot(data10s3PA, aes(x=year, y=Avg3PA)) +
  geom_point() +
  geom_smooth(se = FALSE, color="blue") +  
  labs(title="NBA 3-Point Shooting Trend Over the Years", x="Year", y="Average 3-Point Shots Attempted per Game") +
  scale_x_continuous(breaks=seq(min(2010), max(2020), by=1)) +
  theme_minimal()

?geom_point
?scale_x_continuous

ggplot(data, aes(x=decade, y=team3PA, group=decade)) +
  geom_boxplot() +
  labs(title="NBA 3 Points Attempted by Decade", x="Decade", y="3 Points Attempted") +
  theme_minimal()

ggplot(data, aes(x = teamPTS, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Points by Outcome since 1983/84 Season",
       x = "Points", y = "Density") +
  theme_minimal()

ggplot(data, aes(x = teamFGM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Field Goals Made by Outcome since 1983/84 season",
       x = "Field Goals Made", y = "Density") +
  theme_minimal()

ggplot(data, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome since 1983/84 Season",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p1 <- ggplot(data80s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 1980s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p2 <- ggplot(data90s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 1990s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p3 <- ggplot(data00s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 2000s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p4 <- ggplot(data10s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 2010s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p5 <- ggplot(data20s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 2010s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

(p1 | p2 | p3) / (p4 | p5)

ggplot(data, aes(x = team3PM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Made by Outcome since 1983/84 Season",
       x = "3 Point Field Goals Made", y = "Density") +
  theme_minimal()

p1 <- ggplot(data80s, aes(x = team3PM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Made by Outcome in the 1980s",
       x = "3 Point Field Goals Made", y = "Density") +
  xlim(c(0,30)) +
  theme_minimal()

p2 <- ggplot(data90s, aes(x = team3PM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Made by Outcome in the 1990s",
       x = "3 Point Field Goals Made", y = "Density") +
  xlim(c(0,30)) +
  theme_minimal()

p3 <- ggplot(data00s, aes(x = team3PM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Made by Outcome in the 2000s",
       x = "3 Point Field Goals Made", y = "Density") +
  xlim(c(0,30)) +
  theme_minimal()

p4 <- ggplot(data10s, aes(x = team3PM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Made by Outcome in the 2010s",
       x = "3 Point Field Goals Made", y = "Density") +
  xlim(c(0,30)) +
  theme_minimal()

p5 <- ggplot(data20s, aes(x = team3PM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Made by Outcome in the 2020s",
       x = "3 Point Field Goals Made", y = "Density") +
  xlim(c(0,30)) +
  theme_minimal()

p1/p2/p3/p4/p5


p1 <- ggplot(data, aes(x = teamTRB, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Total Rebounds by Outcome since 1983/84 Season",
       x = "Total Rebounds", y = "Density") +
  theme_minimal()

p2 <- ggplot(data, aes(x = teamORB, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Offensive Rebounds by Outcome since 1983/84 Season",
       x = "Offensive Rebounds", y = "Density") +
  theme_minimal()

p3 <- ggplot(data, aes(x = teamDRB, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Defensive Rebounds by Outcome since 1983/84 Season",
       x = "Defensive Rebounds", y = "Density") +
  theme_minimal()

p1/p2/p3

ggplot(data, aes(x = teamSTL, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Steals by Outcome since 1983/84 Season",
       x = "Steals", y = "Density") +
  theme_minimal()

ggplot(data, aes(x = teamBLK, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Blocks by Outcome since 1983/84 Season",
       x = "Blocks", y = "Density") +
  theme_minimal()

ggplot(data, aes(x = teamFTM, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of Free Throws Made by Outcome since 1983/84 Season",
       x = "Free Throws Made", y = "Density") +
  theme_minimal()


ggplot(data, aes(x = decade)) + 
  geom_bar(aes(y = team2PM, fill = "2PM"), stat = 'identity', position = 'dodge') +
  geom_bar(aes(y = team3PM, fill = "3PM"), stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c("2PM" = "blue", "3PM" = "red")) +
  theme_minimal() +
  labs(title = "Comparison by Decade: 2pm vs 3pm", x = "Decade", y = "Value")

summary_data <- data %>%
  group_by(decade) %>%
  summarise(Avg2PM = mean(team2PM, na.rm = TRUE),
            Avg3PM = mean(team3PM, na.rm = TRUE)) %>%
  gather(key = "Type", value = "AverageShots", Avg2PM, Avg3PM)

ggplot(summary_data, aes(x = decade, y = AverageShots, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Avg2PM" = "blue", "Avg3PM" = "red")) +
  labs(title = "Average 2PM and 3PM Shots by Decade",
       x = "Decade",
       y = "Average Shots",
       fill = "Type of Shot")

summary_data2 <- data %>%
  group_by(decade) %>%
  summarise(`Avg2P%` = mean(team2P., na.rm = TRUE),
            `Avg3P%` = mean(team3P., na.rm = TRUE)) %>%
  gather(key = "Type", value = "AverageShots", `Avg2P%`, `Avg3P%`)

ggplot(summary_data2, aes(x = decade, y = AverageShots, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Avg2P%" = "blue", "Avg3P%" = "red")) +
  labs(title = "Average 2P% and 3P% Shots by Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type of Shot")

p1 <- ggplot(data80s, aes(x = teamEloPre)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 1980s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p1

p2 <- ggplot(data90s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 1990s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p3 <- ggplot(data00s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 2000s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p4 <- ggplot(data10s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 2010s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

p5 <- ggplot(data20s, aes(x = team3PA, fill = teamRslt)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  labs(title = "Distribution of 3 Point Field Goals Attempted by Outcome in the 2010s",
       x = "3 Point Field Goals Attempted", y = "Density") +
  theme_minimal()

eloplot <- data10s %>%
  filter(teamAbbr %in% c('GSW', 'ATL', "DET"))

ggplot(eloplot) +
  geom_line(aes(y = teamEloPre, x = gmDate, group = teamAbbr, color = teamAbbr)) +
  labs(title = "Elo Ratings during 2010s",
       x = "Elo Rating", y = "Density") +
  theme_minimal()

yearly_averages <- data %>%
  group_by(decade, year) %>%
  summarise(Avg3PM = mean(team3PM, na.rm = TRUE),
            AvgEloPre = mean(teamEloPre, na.rm = TRUE))


ggplot(yearly_averages, aes(x = Avg3PM, y = AvgEloPre, color = decade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Average 3PM vs. Elo Rating by Year",
       x = "Average 3-Point Field Goals Made",
       y = "Average Elo Rating") +
  theme_minimal()

cor(data10s$teamEloPre, data10s$team3PM)
cor(data80s$teamEloPre, data80s$team3PM)
cor(data90s$teamEloPre, data90s$team3PM)
cor(data00s$teamEloPre, data00s$team3PM)



