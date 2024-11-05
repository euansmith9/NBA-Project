setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data")

data <- read.csv("finaldata.csv")

data$teamRslt <- factor(data$teamRslt)
data$teamLoc <- factor(data$teamLoc)

data <- data %>%
  dplyr::select(year, decade, game_id, teamWins, teamLosses, gmDate, teamRslt, teamEloPre, opptEloPre, teamLoc, teamDayOff, 
                teamForm, opptForm, teamAvgAST, teamAvg3PM, teamAvg2PM, teamAvgTO,
                teamAvgBLK, teamAvgPF, teamAvgFTM, teamAvgTRB, opptAvgDRTG, teamAbbr, opptAbbr, opptDayOff, weights, year)

data$gmDate <- as.Date(data$gmDate, format = "%Y-%m-%d")

summary(data)

train_data <- data[data$gmDate < as.Date("2021-03-07"),]
test_data <- data[data$gmDate > as.Date("2021-03-07"),]

summary(train_data)

train_data <- na.omit(train_data)

summary(train_data)

train_data$EloDiff <- train_data$teamEloPre - train_data$opptEloPre
test_data$EloDiff <- test_data$teamEloPre - test_data$opptEloPre

set.seed(123)

team_initial_elo <- test_data %>%
  group_by(teamAbbr) %>%
  summarize(initial_elo = first(teamEloPre)) %>%
  ungroup()

elo_ratings <- setNames(team_initial_elo$initial_elo, team_initial_elo$teamAbbr)
elo_ratings

update_elo <- function(winner_elo, loser_elo) {
  K = 20
  Ea <- 1 / (1 + 10 ^ ((loser_elo - winner_elo) / 400))
  Eb <- 1 - Ea
  new_winner_elo <- winner_elo + K * (1 - Ea)
  new_loser_elo <- loser_elo + K * (0 - Eb)
  return(list(new_winner_elo = new_winner_elo, new_loser_elo = new_loser_elo))
}

logmodel5 <- glm(teamRslt == "Win" ~ EloDiff + teamLoc +
                   teamForm + opptForm + teamAvgAST + teamAvgTO +
                   teamAvgBLK + teamAvgPF + teamAvgFTM, 
                 family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel5)

elo_history <- data.frame(Date = as.Date(character()), teamAbbr = character(), EloRating = numeric(), Result = character())

test <- test_data %>%
  filter(teamLoc == 'Home')

summary(test)

for (i in 1:nrow(test)) {
  game <- test[i, ]
  home_team <- game$teamAbbr
  away_team <- game$opptAbbr
  home_team_elo <- elo_ratings[home_team]
  away_team_elo <- elo_ratings[away_team]
  prediction_data <- game
  prediction_data$teamEloPre <- home_team_elo
  prediction_data$opptEloPre <- away_team_elo
  
  predicted_prob <- predict(logmodel5, newdata = prediction_data, type = "response")
  simulated_result <- rbinom(1, 1, predicted_prob)
  
  if (simulated_result == 1) {
    updated_ratings <- update_elo(home_team_elo, away_team_elo)
    elo_ratings[home_team] <- updated_ratings$new_winner_elo
    elo_ratings[away_team] <- updated_ratings$new_loser_elo
    result_home_team <- "Win"
    result_away_team <- "Loss"
  } else {
    updated_ratings <- update_elo(away_team_elo, home_team_elo)
    elo_ratings[away_team] <- updated_ratings$new_winner_elo
    elo_ratings[home_team] <- updated_ratings$new_loser_elo
    result_home_team <- "Loss"
    result_away_team <- "Win"
  }
  
  elo_history <- rbind(elo_history, data.frame(Date = game$gmDate, teamAbbr = home_team, EloRating = elo_ratings[home_team], Result = result_home_team))
  elo_history <- rbind(elo_history, data.frame(Date = game$gmDate, teamAbbr = away_team, EloRating = elo_ratings[away_team], Result = result_away_team))
}

ggplot(elo_history, aes(x = Date, y = EloRating, color = teamAbbr)) +
  geom_line() +
  labs(title = "Simulated Elo Ratings Post All Star Break", x = "Date", y = "Elo Rating", color = "Team") +
  theme_minimal()

new <- train_data %>%
  filter(year == '2021')

new2 <- test_data %>%
  filter(year == '2021')

ggplot(new, aes(x = gmDate, y = teamEloPre, color = teamAbbr)) +
  geom_line() +
  labs(title = "Actual Elo Ratings Pre All Star Break", x = "Date", y = "Elo Rating", color = "Team") +
  theme_minimal()

ggplot(new2, aes(x = gmDate, y = teamEloPre, color = teamAbbr)) +
  geom_line() +
  labs(title = "Actual Elo Ratings Post All Star Break", x = "Date", y = "Elo Rating", color = "Team") +
  theme_minimal()

actual_standings <- data %>%
  group_by(teamAbbr) %>%
  filter(year == '2021') %>%
  filter(gmDate < '2021-03-07') %>%
  summarise(ActualWins = sum(teamRslt == "Win"),
            ActualLosses = sum(teamRslt == "Loss"))

actual_standingspost <- data %>%
  group_by(teamAbbr) %>%
  filter(year == '2021') %>%
  summarise(ActualWins = sum(teamRslt == "Win"),
            ActualLosses = sum(teamRslt == "Loss")) %>%
  arrange(desc(ActualWins), ActualLosses)

sim_outcomes <- elo_history %>%
  group_by(teamAbbr) %>%
  summarise(SimulatedWins = sum(Result == "Win"),
            SimulatedLosses = sum(Result == "Loss")) 

simulated_standings <- full_join(actual_standings, sim_outcomes, by = "teamAbbr") %>%
  mutate(SimTotalWins = ActualWins + SimulatedWins,
         SimTotalLosses = ActualLosses + SimulatedLosses) %>%
  arrange(desc(SimTotalWins), SimTotalLosses)

simulated_standings <- na.omit(simulated_standings)

view(simulated_standings)

sum(simulated_standings$SimTotalWins)
sum(simulated_standings$SimTotalLosses)

east_teams <- c("BRK", "MIA", "ATL", "BOS", "NYK", "PHI", "TOR", "CHI", "CLE", "DET", "IND", "MIL", "WAS", "ORL", "CHA")

actual_standingspost$Conference <- ifelse(actual_standingspost$teamAbbr %in% east_teams, "East", "West")
simulated_standings$Conference <- ifelse(simulated_standings$teamAbbr %in% east_teams, "East", "West")

simeast_standings <- simulated_standings %>%
  filter(Conference == "East") %>%
  arrange(desc(SimTotalWins), SimTotalLosses)

simwest_standings <- simulated_standings %>%
  filter(Conference == "West") %>%
  arrange(desc(SimTotalWins), SimTotalLosses)

east_standings <- actual_standingspost %>%
  filter(Conference == "East") %>%
  arrange(desc(ActualWins), ActualLosses)

west_standings <- actual_standingspost %>%
  filter(Conference == "West") %>%
  arrange(desc(ActualWins), ActualLosses)

east_standings <- cbind(east_standings, simeast_standings)
west_standings <- cbind(west_standings, simwest_standings)

write.csv(west_standings, "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data/weststandings.csv", row.names = FALSE)
write.csv(east_standings,  "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/NBA Project/data/weststandings.csv", row.names = FALSE)

elo_ratings_initial <- setNames(team_initial_elo$initial_elo, team_initial_elo$teamAbbr)
elo_ratings_initial

full_simulation <- function(test, elo_ratings_initial, logmodel5, n) {
  all_outcomes <- list()
  
  for (i in 1:n) {
    elo_ratings <- elo_ratings_initial
    outcomes <- vector("list", length = nrow(test))
    
    for (j in 1:nrow(test)) {
      game <- test[j, ]
      home_team <- game$teamAbbr
      away_team <- game$opptAbbr
      
      prediction_data <- game
      prediction_data$teamEloPre <- elo_ratings[home_team]
      prediction_data$opptEloPre <- elo_ratings[away_team]
      
      predicted_prob <- predict(logmodel5, newdata = prediction_data, type = "response")
      simulated_result <- rbinom(1, size = 1, prob = predicted_prob)
      
      outcomes[[j]] <- list(home_team = home_team, away_team = away_team,
                                home_result = ifelse(simulated_result == 1, "Win", "Loss"),
                                away_result = ifelse(simulated_result == 1, "Loss", "Win"))
    }
    
    all_outcomes[[i]] <- do.call(rbind, lapply(outcomes, data.frame))
  }
  
  return(all_outcomes)
}


set.seed(123)

n <- 1000
sim <- full_simulation(test_data, elo_ratings_initial, logmodel5, n)

combined_outcomes <- do.call(rbind, sim)

simhome_outcomes <- combined_outcomes %>%
  group_by(home_team) %>%
  summarise(SimulatedHomeWins = sum(home_result == "Win"),
            SimulatedHomeLosses = sum(home_result == "Loss")) %>%
  rename(teamAbbr = home_team)

simaway_outcomes <- combined_outcomes %>%
  group_by(away_team) %>%
  summarise(SimulatedAwayWins = sum(away_result == "Win"),
            SimulatedAwayLosses = sum(away_result == "Loss")) %>%
  rename(teamAbbr = away_team)

simcombined_outcomes <- full_join(simhome_outcomes, simaway_outcomes, by = "teamAbbr") %>%
  rowwise() %>%
  mutate(SimulatedWins = sum(c(SimulatedHomeWins, SimulatedAwayWins)),
         SimulatedLosses = sum(c(SimulatedHomeLosses, SimulatedAwayLosses))) %>%
  ungroup() %>%
  dplyr::select(teamAbbr, SimulatedWins, SimulatedLosses)

simulated_standings <- full_join(actual_standings, simcombined_outcomes, by = "teamAbbr") %>%
  mutate(ActualWins = ActualWins*1000,
         ActualLosses = ActualLosses*1000) %>%
  mutate(SimTotalWins = ActualWins + SimulatedWins,
         SimTotalLosses = ActualLosses + SimulatedLosses) %>%
  arrange(desc(SimTotalWins), SimTotalLosses)

simulated_standings <- na.omit(simulated_standings)

view(simulated_standings)

sum(simulated_standings$SimTotalWins)
sum(simulated_standings$SimTotalLosses)

simulated_team_outcomes <- simulated_outcomes %>%
  group_by(Team) %>%
  summarise(SimulatedWins = sum(Result == "Win"),
            SimulatedLosses = sum(Result == "Loss"))

team_total_winsdal <- numeric(length = length(sim))

for (i in 1:length(sim)) {
  run_outcomes <- sim[[i]]
  home_wins <- sum(run_outcomes$home_team == "DAL" & run_outcomes$home_result == "Win")
  away_wins <- sum(run_outcomes$away_team == "DAL" & run_outcomes$away_result == "Win")
  team_total_winsdal[i] <- home_wins + away_wins
}


p1 <- ggplot(data.frame(TotalWins = team_total_winsdal), aes(x = TotalWins)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1, fill = 'blue') +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of Simulated Wins for DAL",
       x = "Simulated Wins",
       y = "Density") +
  xlim(c(10, 30)) +
  theme_minimal()

team_total_winsGSW <- numeric(length = length(sim))

for (i in 1:length(sim)) {
  iteration <- sim[[i]]
  home_wins <- sum(iteration$home_team == "GSW" & iteration$home_result == "Win")
  away_wins <- sum(iteration$away_team == "GSW" & iteration$away_result == "Win")
  team_total_winsGSW[i] <- home_wins + away_wins
}


p2 <- ggplot(data.frame(TotalWins = team_total_winsGSW), aes(x = TotalWins)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1, fill = 'yellow') +
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(title = "Density of Simulated Wins for GSW",
       x = "Simulated Wins",
       y = "Density") +
  xlim(c(10, 30)) +
  theme_minimal()

team_total_winsPHO <- numeric(length = length(sim))

for (i in 1:length(sim)) {
  run_outcomes <- sim[[i]]
  home_wins <- sum(run_outcomes$home_team == "PHO" & run_outcomes$home_result == "Win")
  away_wins <- sum(run_outcomes$away_team == "PHO" & run_outcomes$away_result == "Win")
  team_total_winsPHO[i] <- home_wins + away_wins
}


p3 <- ggplot(data.frame(TotalWins = team_total_winsPHO), aes(x = TotalWins)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1, fill = 'purple') +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density of Simulated Wins for PHO",
       x = "Simulated Wins",
       y = "Density") +
  xlim(c(10, 30)) +
  theme_minimal()

p1/p2/p3







