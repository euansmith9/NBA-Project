# NBA-Project
Simulating NBA Game Outcomes Using Machine Learning

Predicting the outcomes of NBA games and the consequent standings of teams is a challenge
that statisticians and basketball enthusiasts have grappled with for years. The allure of the
task lies in its complexity and the dynamic nature of the sport and the National Basketball
Association (NBA) provides a rich bank of data for prediction.

The NBA has been an extensive collector of game-related data, making it an ideal candidate
for statistical analysis. The vast amount of data available allow for a nuanced exploration of
the factors that contribute to a team's victory, thereby facilitating the creation of a predictive
models.

The implementation of the three-point line in the 1979-80 season marked a significant
turning point in basketball strategy. Initially regarded as a risky and unconventional play, the
three-point shot has evolved to become a central element of basketball offense. This
evolution has been gradual, reflecting the sport's adaptability and the innovative mindset of
its participants. The analytics movement in basketball, spearheaded by individuals like Daryl
Morey, placed a spotlight on the efficiency of the three-point shot and led to a league-wide
re-evaluation of offensive strategies. The Houston Rockets, under Morey's leadership,
exemplified this shift as they often led the league in three-point attempts. Today, the NBA is
characterised by the prevalence of the three-point shot, with players such as Stephen Curry
and Klay Thompson, the famed "Splash Brothers" of the Golden State Warriors,
revolutionising the guard position along with winning 4 NBA championships together (Freitas
2021).

The Elo rating system is measure of a team's competitive calibre. Teams start with an initial
Elo rating which then evolves through their performance in games, with the rating's
sensitivity to match outcomes dictated by a carefully chosen K-factor, for the NBA balancing
the need for ratings to respond to recent performances without excessive volatility. This
system has the added advantage of incorporating the margin of victory, providing a more
nuanced understanding of each game's context.

In this report, we will be looking at a large portion of NBA history, with team statistics from
the 1983/84 season all the way up until the 2020/21 season, with the objective of employing
logistic regression to predict the outcomes of the 2020/21 NBA season. Logistic regression
has been selected for this study due to its effectiveness in analysing binary outcomes,
specifically, the wins or losses of NBA games. These outcomes are influenced by a variety of
predictor variables, reflecting the diverse and complex nature of team performance,
including Elo rating. The model aims to break down the patterns in team performances and
predict game outcomes with a high degree of accuracy.
