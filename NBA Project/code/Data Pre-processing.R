setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)
library(MASS)
library(GGally)

#  Read in data

data <- read.csv("1949-2020_officialBoxScore.csv")

data$gmDate <- as.Date(data$gmDate, format="%Y-%m-%d")

data <- arrange(data, gmDate)

# Read in 2020/21 data

data2 <- read.csv("2020-21_officialBoxScore.csv")

data2$gmDate <- as.Date(data2$gmDate, format="%Y-%m-%d")

data <- bind_rows(data, data2) # Combine dataframes

data <- arrange(data, gmDate)

data <- data %>%
  filter(gmDate > as.Date("1983-08-31")) # Filter from 1983 season onwards

data <- data %>%
  filter(seasonType == "Regular") # Filter by regular season

data <- data %>% distinct() # Remove any duplicate rows

newnames <- list(
  CHH = "CHA", 
  NJN = "BRK", 
  NOH = "NOP", 
  NOK = "NOP", 
  SEA = "OKC", 
  VAN = "MEM", 
  CHO = "CHA", 
  KCK = "SAC",
  SDC = "LAC",
  WSB = "WAS"  
) # Mapping abbreviation changes amongst franchise

namefunc <- function(abbr, map) {
  if (abbr %in% names(map)) {
    return(map[[abbr]])
  } else {
    return(abbr)
  }
} # Function to apply mapping

data <- data %>%
  mutate(teamAbbr = sapply(teamAbbr, function(abbr) namefunc(abbr, newnames)),
         opptAbbr = sapply(opptAbbr, function(abbr) namefunc(abbr, newnames))) # Applying Mapping

# Creating game id for each game

data$game_id <- ifelse(data$opptLoc == "Home", 
                       paste0(format(data$gmDate, "%Y%m%d"), data$opptAbbr), 
                       paste0(format(data$gmDate, "%Y%m%d"), data$teamAbbr))

# Creating year variable

data$year <- as.integer(substr(data$season, 1, 4)) + 1

# Creating decade variable

data$decade <- cut(data$year,
                   breaks = c(1980, 1990, 2000, 2010, 2020, 2030),
                   labels = c("1980s", "1990s", "2000s", "2010s", "2020s"), right = FALSE)

# Creating weights for more recent games to be weighted more

data$weights <- ifelse(data$year == 2021, 16,
                       ifelse(data$year == 2020, 8,
                              ifelse(data$year == 2019, 4,
                                     ifelse(data$year >= 2010 & data$year < 2019, 2, 1))))

# Creating team form variables

data <- data %>%
  mutate(win = ifelse(teamRslt == "Win", 1, 0)) %>%
  group_by(teamAbbr, year) %>%
  mutate(teamForm = lag(rollapply(win, 10, FUN = mean, partial = TRUE, align = 'right'))) %>%
  ungroup()

data <- data %>%
  mutate(win = ifelse(opptRslt == "Win", 1, 0)) %>%
  group_by(opptAbbr, year) %>%
  mutate(opptForm = lag(rollapply(win, 10, FUN = mean, partial = TRUE, align = 'right'))) %>%
  ungroup()

# Writing as csv file

write.csv(data, "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data/Feature Data.csv", row.names = FALSE)

# Reading in nba elo data

data1 <- read.csv("nba_elo.csv", stringsAsFactors = TRUE)

# Stadardising column names

colnames(data1)[1] <- "gmDate"
colnames(data1)[2] <- "year"
colnames(data1)[4] <- "seasonType"
colnames(data1)[5] <- "opptAbbr"
colnames(data1)[6] <- "teamAbbr"
colnames(data1)[7] <- "opptEloPre"
colnames(data1)[8] <- "teamEloPre"
colnames(data1)[11] <- "opptEloPost"
colnames(data1)[12] <- "teamEloPost"

data1$gmDate <- as.Date(data1$gmDate, format="%d/%m/%Y")

# Filtering Elo data

data1 <- data1 %>%
  filter(gmDate > as.Date("1983-08-31"))

data1 <- data1 %>%
  filter(gmDate < as.Date("2021-8-31"))

data1$decade <- cut(data1$year,
                    breaks = c(1980, 1990, 2000, 2010, 2020, 2030),
                    labels = c("1980s", "1990s", "2000s", "2010s", "2020s"), right = FALSE)


data1 <- data1 %>%
  filter(!seasonType %in% c("t", "q", "s", "c", "f", "p"))

data1$seasonType <- "Regular"

write.csv(data1, "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data/elo_data.csv", row.names = FALSE)

data <- read.csv("Feature Data.csv")

data1 <- read.csv("elo_data.csv")

newnames2 <- list(
  CHH = "CHA", 
  NJN = "BRK", 
  NOH = "NOP", 
  NOK = "NOP", 
  SEA = "OKC", 
  VAN = "MEM", 
  CHO = "CHA", 
  KCK = "SAC",
  SDC = "LAC",
  WSB = "WAS"  
) # Mapping abbreaviations

namefunc <- function(abbr, map) {
  if (abbr %in% names(map)) {
    return(map[[abbr]])
  } else {
    return(abbr)
  }
}

data1 <- data1 %>%
  mutate(teamAbbr = sapply(teamAbbr, function(abbr) namefunc(abbr, newnames2)),
         opptAbbr = sapply(opptAbbr, function(abbr) namefunc(abbr, newnames2)))

data1$gmDate <- as.Date(data1$gmDate, format="%Y-%m-%d")

data1$game_id <- paste0(format(data1$gmDate, "%Y%m%d"), data1$opptAbbr) # Game id for elo data frame

data <- data %>% distinct(data$game_id, .keep_all = TRUE)

common_games <- intersect(data$game_id, data1$game_id) # Checking coomon games in both datasets

data_common <- data[data$game_id %in% common_games, ]
data1_common <- data1[data1$game_id %in% common_games, ]

# Checking exclusive games in each dataset (due to replays/ neutral court)

exclusive_data <- setdiff(data$game_id, data1$game_id)
exclusive_data1 <- setdiff(data1$game_id, data$game_id)
exclusive_data
exclusive_data1

data <- filter(data, !(game_id %in% exclusive_data))
data1 <- filter(data1, !(game_id %in% exclusive_data1))

table(data$teamAbbr)
table(data1$teamAbbr)

data1 <- dplyr::select(data1, game_id, teamEloPre, opptEloPre)

# Merging datasets

data <- inner_join(data, data1, by = "game_id")

# Feature engineering

data <- data %>%
  group_by(teamAbbr, year) %>%
  mutate(
    teamAvgAST = lag(rollapply(teamAST, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvg3PM = lag(rollapply(team3PM, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvg2PM = lag(rollapply(team2PM, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvgTO = lag(rollapply(teamTO, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvgBLK = lag(rollapply(teamBLK, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvgSTL = lag(rollapply(teamSTL, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvgPF = lag(rollapply(teamPF, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvgFTM = lag(rollapply(teamFTM, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    teamAvgTRB = lag(rollapply(teamTRB, width = 10, FUN = mean, partial = TRUE, align = 'right'))
  ) %>%
  ungroup()

data <- data %>%
  group_by(opptAbbr, year) %>%
  mutate(
    opptAvgAST = lag(rollapply(opptAST, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvg3PM = lag(rollapply(oppt3PM, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvg2PM = lag(rollapply(oppt2PM, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvgTO = lag(rollapply(opptTO, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvgBLK = lag(rollapply(opptBLK, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvgSTL = lag(rollapply(opptSTL, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvgPF = lag(rollapply(opptPF, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvgFTM = lag(rollapply(opptFTM, width = 10, FUN = mean, partial = TRUE, align = 'right')),
    opptAvgTRB = lag(rollapply(opptTRB, width = 10, FUN = mean, partial = TRUE, align = 'right'))
  )

data <- data %>%
  group_by(opptAbbr, year) %>%
  mutate(opptAvgDRTG = lag(rollapply(opptDrtg, width = 10, FUN = mean, partial = TRUE, align = 'right')))

data <- data %>%
  group_by(teamAbbr, year) %>%
  mutate(teamAvgDRTG = lag(rollapply(teamDrtg, width = 10, FUN = mean, partial = TRUE, align = 'right')))

# Creating observations from both perspectives

opdata <- data %>%
  rename_with(~ str_replace(., "^team", "temp"), starts_with("team")) %>%
  rename_with(~ str_replace(., "^oppt", "team"), starts_with("oppt")) %>%
  rename_with(~ str_replace(., "^temp", "oppt"), starts_with("temp"))

data <- bind_rows(data, opdata)

data <- arrange(data, gmDate)

write.csv(data, "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data/finaldata.csv", row.names = FALSE)


