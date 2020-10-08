library(readr)
library(tidyverse)

#Loading Data
nfldata <- read_csv("~/Desktop/Data Science/Stat231JackDove/Homeworks/spreadspoke_scores.csv")

#WRANGLING

#Wrangling by Year
nfltotalsdata <- nfldata %>%
  group_by(schedule_season) %>%
  summarize(avg_score_total = mean(score_home + score_away), 
            avg_score_difference = mean((score_home-score_away)), 
            avg_temperature= (mean(weather_temperature)), 
            avg_spread_magnitude = mean(abs(spread_favorite)), 
            avg_overunder = mean(over_under_line),
            avg_spread_accuracy = mean(abs(abs(score_home-score_away)-abs(spread_favorite))),
            avg_overunder_accuracy = mean(abs((score_home+score_away)-over_under_line)))

#Wrangling by Points bins (and year)
nfltotalsdata_pointsbins <- nfldata %>%
  mutate(scoretotal = score_home + score_away) %>%
  mutate(scoringbin = ifelse(scoretotal > mean(scoretotal), "highscoring", "lowscoring")) %>%
  group_by(schedule_season, scoringbin) %>%
  summarize(avg_score_total = mean(score_home + score_away), 
            avg_score_difference = mean((score_home-score_away)), 
            avg_temperature= (mean(weather_temperature)), 
            avg_spread_magnitude = mean(abs(spread_favorite)), 
            avg_overunder = mean(over_under_line),
            avg_spread_accuracy = mean(abs(abs(score_home-score_away)-abs(spread_favorite))),
            avg_overunder_accuracy = mean(abs((score_home+score_away)-over_under_line)))

#In 1988, Over Under is Consistently Tracked: Year Data
post1978nfltotalsdata <- nfltotalsdata %>%
  filter(schedule_season > 1987)

#In 1988, Over Under is Consistently Tracked: Bin Data
post1978nfltotalsdata_pointsbins <- nfltotalsdata_pointsbins %>%
  filter(schedule_season > 1987)





#CHARTS

#Total Score vs. Over Under Line Chart
totalscore_vs_overunder_chart <- ggplot(data=post1978nfltotalsdata, aes(x=schedule_season)) +
                                         geom_line(aes(y=avg_score_total), color = "Red") +
                                         geom_line(aes(y=avg_overunder), color = "Blue")

#Score Difference vs. Spread Magnitude Line Chart
scorediff_vs_spreadmag_chart <- ggplot(data=post1978nfltotalsdata, aes(x=schedule_season)) +
  geom_line(aes(y=avg_score_difference), color = "Red") +
  geom_line(aes(y=avg_spread_magnitude), color = "Blue")

#Spread Accuracy by Points Bin Line Chart
spread_accuracy_by_points_bin <- ggplot(
  data= post1978nfltotalsdata_pointsbins, 
  aes(x= schedule_season, y= avg_spread_accuracy, group=scoringbin, color=scoringbin)) + geom_line()
  
#Over Under Accuracy by Points Bin Line Chart
Over_Under_Accuracy_by_points_bin <- ggplot(
  data= post1978nfltotalsdata_pointsbins, 
  aes(x= schedule_season, y= avg_overunder_accuracy, group=scoringbin, color=scoringbin)) + geom_line()






