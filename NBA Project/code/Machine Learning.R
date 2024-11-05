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

cor_matrix <- cor(train_data[, sapply(train_data, is.numeric)])
cor_matrix
corrplot::corrplot(cor_matrix, method = 'number')

logmodel1 <- glm(teamRslt == "Win" ~ teamEloPre + opptEloPre + teamLoc + teamDayOff + opptDayOff +
             teamForm + opptForm + teamAvgAST + teamAvg3PM + teamAvg2PM + teamAvgTO +
             teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB + opptAvgDRTG, 
             family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel1)

train_data$EloDiff <- train_data$teamEloPre - train_data$opptEloPre

logmodel2 <- glm(teamRslt == "Win" ~ EloDiff + teamLoc +
                   teamForm + opptForm + teamAvgAST + teamAvg3PM + teamAvgTO +
                   teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB + opptAvgDRTG, 
                 family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel2)

logmodel3 <- glm(teamRslt == "Win" ~ EloDiff + teamLoc +
                   teamForm + opptForm + teamAvgAST + teamAvgTO +
                   teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB + opptAvgDRTG, 
                 family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel3)

logmodel4 <- glm(teamRslt == "Win" ~ EloDiff + teamLoc +
                   teamForm + opptForm + teamAvgAST + teamAvgTO +
                   teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB, 
                 family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel4)

data20s$EloDiff <- data20s$teamEloPre - data20s$opptEloPre

logmodel5 <- glm(teamRslt == "Win" ~ EloDiff + teamLoc +
                   teamForm + opptForm + teamAvgAST + teamAvgTO +
                   teamAvgBLK + teamAvgPF + teamAvgFTM, 
                 family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel5)

logmodel6 <- glm(teamRslt == "Win" ~ EloDiff + teamLoc +
                   teamForm + opptForm + teamAvgAST + teamAvgTO +
                   teamAvgBLK + teamAvgFTM, 
                 family = binomial, data = train_data, weights = train_data$weights)
summary(logmodel6)

train_data$pred_train1 <- predict(logmodel1, type = "response")
train_data$pred_train2 <- predict(logmodel5, type = "response")
test_data$pred_test <- predict(logmodel5, type = "response", newdata = test_data)

ggplot(train_data) +
  geom_boxplot(aes(x=factor(teamRslt), y=pred_train1)) -> p1

ggplot(train_data) +
  geom_density(aes(x=pred_train1)) +
  facet_wrap(~teamRslt, ncol=1)-> p2

ggplot(train_data) +
  geom_boxplot(aes(x=factor(teamRslt), y=pred_train2)) -> p3

ggplot(train_data) +
  geom_density(aes(x=pred_train2)) +
  facet_wrap(~teamRslt, ncol=1)-> p4

(p1+p2)

(p3+p4)

t1 <- table(train_data$pred_train1 >= 0.5, train_data$teamRslt)
t1

t2 <- table(train_data$pred_train2 >= 0.5, train_data$teamRslt)
t2

t3 <- table(test_data$pred_test >= 0.5, test_data$teamRslt)
t3

true_positives = 27990
true_negatives = 28045
false_positives = 13056
false_negatives = 13001

accuracy = (true_positives + true_negatives) / (true_positives + true_negatives + false_positives + false_negatives)
precision = true_positives / (true_positives + false_positives)
sensitivity = true_positives / (true_positives + false_negatives)
specificity = true_negatives / (true_negatives + false_positives)
f1_score = (2 * precision * sensitivity) / (precision + sensitivity)

accuracy
precision
sensitivity
specificity
f1_score

library(tidyverse)

data_2021 <- data %>%
  filter(year == 2021)

test_data2 <- test_data %>% 
  dplyr::select(game_id, pred_test)

test_data2

data_2021 <- data_2021 %>%
  left_join(test_data2, by = "game_id", relationship = "many-to-many")

actual_outcomes <- data_2021 %>%
  filter(gmDate < as.Date("2021-03-07")) %>%
  group_by(teamAbbr) %>%
  summarise(ActualWins = sum(teamRslt == "Win"), ActualLosses = sum(teamRslt == "Loss"))

predicted_outcomes <- data_2021 %>%
  filter(gmDate > as.Date("2021-03-07") & teamLoc == "Home") %>%
  mutate(predictedRslt = ifelse(!is.na(pred_test), ifelse(pred_test >= 0.5, "Win", "Loss"), NA_character_)) %>%
  group_by(teamAbbr) %>%
  summarise(PredictedWins = sum(predictedRslt == "Win", na.rm = TRUE), 
            PredictedLosses = sum(predictedRslt == "Loss", na.rm = TRUE))

predicted_outcomes

total_outcomes <- full_join(actual_outcomes, predicted_outcomes, by = "teamAbbr") %>%
  mutate(TotalWins = ActualWins + PredictedWins, TotalLosses = ActualLosses + PredictedLosses) %>%
  arrange(desc(TotalWins), TotalLosses) %>%
  dplyr::select(teamAbbr, TotalWins, TotalLosses)

print(total_outcomes)



