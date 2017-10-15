# ===================================================================
# Title: Make Teams Table
# Description:
#   This script prepares the data from roster and stats, puts them 
#   'nba2017-teams.csv'
# Input(s): data file 'nba2017-roster.csv' and 'nba2017-stats.csv'
# Output(s): data file 'nba2017-teams.csv'
# Author: Jesse Gao
# Date: 10-14-2017
# ===================================================================

# download RData file into your working directory
github <- "https://github.com/ucb-stat133/stat133-fall-2017/raw/master/data/"
csv <- "nba2017-roster.csv"
download.file(url = paste0(github, csv), destfile = '../data/nba2017-roster.csv')
csv <- "nba2017-stats.csv"
download.file(url = paste0(github, csv), destfile = '../data/nba2017-stats.csv')

nbaRoster = read.csv(file = '../data/nba2017-roster.csv', stringsAsFactors = FALSE)
nbaStats = read.csv(file = '../data/nba2017-stats.csv', stringsAsFactors = FALSE)

library(dplyr)
missed_fg = nbaStats$field_goals_atts - nbaStats$field_goals_made
missed_ft = nbaStats$points1_atts - nbaStats$points1_made
points = nbaStats$points1_made + 2 * nbaStats$points2_made + 3 * nbaStats$points3_made
rebounds = nbaStats$off_rebounds + nbaStats$def_rebounds
efficiency = (points + rebounds + nbaStats$assists + nbaStats$steals + nbaStats$blocks - missed_fg - missed_ft - nbaStats$turnovers) / nbaStats$games_played
nbaStats = mutate(nbaStats, missed_fg, missed_ft, points, rebounds, efficiency)

sink(file = '../output/efficiency-summary.txt')
summary(efficiency)
sink()

merged = merge(nbaRoster, nbaStats)

#teams = merged %>%
#  select(c("team", "experience", "salary", "points3_made", "points2_made", "points1_made", "points", "off_rebounds", "def_rebounds", "assists", "steals", "blocks", "turnovers", "fouls", "efficiency"))
#teams$points3_made = teams$points3_made*3
#teams$points2_made = teams$points2_made*2
#colnames(teams) = c("team", "experience", "salary", "points3", "points2", "free_throws", "points", "off_rebounds", "def_rebounds", "assists", "steals", "blocks", "turnovers", "fouls", "efficiency")
teams = merged %>%
  group_by(team) %>%
  summarise(
    experience = round(sum(experience), 2),
    salary = round(sum(salary/1000000), 2),
    points3 = sum(points3_made),
    points2 = sum(points2_made),
    free_throws = sum(points1_made),
    points = sum(points),
    off_rebounds = sum(off_rebounds),
    def_rebounds = sum(def_rebounds),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    turnovers = sum(turnovers),
    fouls = sum(fouls),
    efficiency = sum(efficiency)
  )

sink(file = '../data/teams-summary.txt')
summary(teams)
sink()

write.csv(x = teams, file = '../data/nba2017-teams.csv')

pdf(file = '../images/teams_star_plot.pdf')
stars(teams[,-1], labels = teams$team)
dev.off()

library(ggplot2)
pdf(file = '../images/experience_salary.pdf')
ggplot(data = teams, aes(x = experience, y = salary)) + 
  geom_point() + ggtitle("Experience vs Salary")
dev.off()

