
library(tidyverse)
library(ggthemes)
library(rvest)  
library(robotstxt)
library(readxl)

#URLs: 

# Source: Spotrac Contract Values (Salary)

# NFL: https://www.spotrac.com/nfl/contracts/sort-value/limit-2000/
# NBA: https://www.spotrac.com/nba/contracts/sort-value/limit-2000/
# NHL: https://www.spotrac.com/nhl/contracts/sort-value/limit-2000/
# MLB: https://www.spotrac.com/mlb/contracts/sort-value/limit-2000/

# Source: Statmuse (Performance)
# NFL: https://www.statmuse.com/nfl/ask/how-many-time-has-each-nfl-team-made-the-playoffs-since-2012
# NBA: https://www.statmuse.com/nba/ask/how-many-time-has-each-nba-team-made-the-playoffs-since-2012
# NHL: https://www.statmuse.com/nhl/ask/how-many-time-has-each-nhl-team-made-the-playoffs-since-2012
# MLB: https://www.statmuse.com/mlb/ask/how-many-time-has-each-mlb-team-made-the-playoffs-since-2012

# Spotrac updated their website since we began this project, so this code no
# longer works! We downloaded the data beforehand into an Rdata file using 
# this script, but it will no longer work. 

# nfl <- read_html("https://www.spotrac.com/nfl/contracts/sort-value/limit-2000/")
# nba <- read_html("https://www.spotrac.com/nba/contracts/sort-value/limit-2000/")
# nhl <- read_html("https://www.spotrac.com/nhl/contracts/sort-value/limit-2000/")
# mlb <- read_html("https://www.spotrac.com/mlb/contracts/sort-value/limit-2000/")

nfl_performance <- read_excel("../data/NFLData.xlsx")
nba_performance <- read_excel("../data/NBAData.xlsx")
nhl_performance <- read_excel("../data/NHLData.xlsx")
mlb_performance <- read_excel("../data/MLBData.xlsx")

nfl_total_cap <- read_excel("../data/NFLtotalcap.xlsx")
nba_total_cap <- read_excel("../data/NBAtotalcap.xlsx")
nhl_total_cap <- read_excel("../data/NHLtotalcap.xlsx")
mlb_total_cap <- read_excel("../data/MLBtotalcap.xlsx")

clean_team_names <- function(data, team1, team2, new_team_name) {
  # Use grepl to match partial names
  data$Team <- ifelse(grepl(team1, data$Team) | grepl(team2, data$Team), new_team_name, data$Team)
  
  return(data)
}


mlb_total_cap <- clean_team_names(mlb_total_cap, "Cleveland Guardians", "Cleveland Indians", "Cleveland Guardians")
mlb_total_cap <- clean_team_names(mlb_total_cap, "Miami Marlins", "Florida Marlins", "Miami Marlins")

nba_total_cap <- clean_team_names(nba_total_cap, "Brooklyn Nets", "New Jersey Nets", "Brooklyn Nets")
nba_total_cap <- clean_team_names(nba_total_cap, "Charlotte Bobcats", "Charlotte Hornets", "Charlotte Hornets")
nba_total_cap <- clean_team_names(nba_total_cap, "New Orleans Hornets", "New Orleans Pelicans", "New Orleans Pelicans")

nhl_total_cap <- clean_team_names(nhl_total_cap, "Phoenix Coyotes", "Arizona Coyotes", "Arizona Coyotes")

nfl_total_cap <- clean_team_names(nfl_total_cap, "Washington Redskins", "Washington Football Team", "Washington Commanders")
nfl_total_cap <- clean_team_names(nfl_total_cap, "Washington Commanders", " Washington Commanders", "Washington Commanders")
nfl_total_cap <- clean_team_names(nfl_total_cap, "Las Vegas Raiders", "Oakland Raiders", "Las Vegas Raiders")
nfl_total_cap <- clean_team_names(nfl_total_cap, "San Diego Chargers", "Los Angeles Chargers", "Los Angeles Chargers")
nfl_total_cap <- clean_team_names(nfl_total_cap, "St. Louis Rams", "Los Angeles Rams", "Los Angeles Rams")



mlb_total_cap_grouped <- mlb_total_cap %>% 
  group_by(Team) %>% 
  summarize(
    Average_total_cap = mean(TotalPayroll) 
  )

nfl_total_cap_grouped <- nfl_total_cap %>% 
  group_by(Team) %>% 
  summarize(
    Average_total_cap = mean(TotalCap),
    Average_cap_space = mean(CapSpace),
    
  )

nhl_total_cap_grouped <- nhl_total_cap %>% 
  group_by(Team) %>% 
  filter(!(Team %in% c("Vegas Golden Knights", "Seattle Kraken"))) %>% 
  summarize(
    Average_total_cap = mean(TotalCap),
    Average_cap_space = mean(CapSpace)
  )

nba_total_cap_grouped <- nba_total_cap %>% 
  group_by(Team) %>% 
  summarize(
    Average_total_cap = mean(TotalCap),
    Average_cap_space = mean(CapSpace)
  )

names(mlb_total_cap_grouped)[names(mlb_total_cap_grouped) == "Team"] <- "TEAM"

mlb_dataset <- merge(mlb_total_cap_grouped, mlb_performance, by = "TEAM")

names(nba_total_cap_grouped)[names(nba_total_cap_grouped) == "Team"] <- "TEAM"

nba_dataset <- merge(nba_total_cap_grouped, nba_performance, by = "TEAM")

names(nfl_total_cap_grouped)[names(nfl_total_cap_grouped) == "Team"] <- "TEAM"

nfl_dataset <- merge(nfl_total_cap_grouped, nfl_performance, by = "TEAM")

names(nhl_total_cap_grouped)[names(nhl_total_cap_grouped) == "Team"] <- "TEAM"

nhl_dataset <- merge(nhl_total_cap_grouped, nhl_performance, by = "TEAM")

save(nfl_dataset, nhl_dataset, nba_dataset, mlb_dataset,
          file = "../data/performance.Rdata")


# scrape_data <- function(data, tag) {
#   element <- data %>% 
#     html_elements(tag) %>% 
#     html_text2()
#   
#   return(element)
# }
# 
# clean_data <- function(df) {
#   df$position <- gsub(" \\|", "", df$position)
#   
#   df <- df %>%
#     mutate(
#       rank = as.numeric(rank),
#       contract_length = as.numeric(contract_length),
#       signed_age = as.numeric(signed_age),
#       signed_age = ifelse(signed_age == 0, NA, signed_age),
#       contract_value = as.numeric(gsub("[\\$|,]", "", contract_value)),
#       aav = as.numeric(gsub("[\\$|,]", "", aav)),
#       sign_bonus = ifelse(!is.na(sign_bonus), as.numeric(gsub("[\\$|,]", "", sign_bonus)), NA)
#     )
#   
#   df <- df %>% 
#     filter(position != "|")
#   
#   return(df)
# }
# 
# rank <- scrape_data(nfl, ".noborderright")
# player_name <- scrape_data(nfl, ".team-name")
# position <- scrape_data(nfl, ".team-name+ .rank-position")
# signed_age <- scrape_data(nfl, ".rank-name+ .small")
# contract_length <- scrape_data(nfl, ".small+ td.small")
# contract_value <- scrape_data(nfl, ".result")
# aav <- scrape_data(nfl, ".result+ .right")
# sign_bonus <- scrape_data(nfl, "td:nth-child(7)")
# 
# nfl_salary <- tibble(rank=rank,
#                      name = player_name,
#                      position = position,
#                      contract_length = contract_length, 
#                      signed_age = signed_age,
#                      contract_value = contract_value, 
#                      aav = aav,
#                      sign_bonus = sign_bonus
# )
# 
# nfl_salary <- clean_data(nfl_salary)
# 
# rank <- scrape_data(nhl, ".noborderright")
# player_name <- scrape_data(nhl, ".team-name")
# position <- scrape_data(nhl, ".team-name+ .rank-position")
# signed_age <- scrape_data(nhl, ".rank-name+ .small")
# contract_length <- scrape_data(nhl, ".small+ td.small")
# contract_value <- scrape_data(nhl, ".result")
# aav <- scrape_data(nhl, ".result+ .right")
# sign_bonus <- scrape_data(nhl, "td:nth-child(7)")
# 
# nhl_salary <- tibble(rank=rank,
#                      name = player_name,
#                      position = position,
#                      contract_length = contract_length, 
#                      signed_age = signed_age,
#                      contract_value = contract_value, 
#                      aav = aav,
#                      sign_bonus = sign_bonus
# )
# 
# nhl_salary <- clean_data(nhl_salary)
# 
# rank <- scrape_data(nba, ".noborderright")
# player_name <- scrape_data(nba, ".team-name")
# position <- scrape_data(nba, ".team-name+ .rank-position")
# signed_age <- scrape_data(nba, ".rank-name+ .small")
# contract_length <- scrape_data(nba, ".small+ td.small")
# contract_value <- scrape_data(nba, ".result")
# aav <- scrape_data(nba, ".result+ .right")
# sign_bonus <- scrape_data(nba, "td:nth-child(7)")
# 
# nba_salary <- tibble(rank=rank,
#                      name = player_name,
#                      position = position,
#                      contract_length = contract_length, 
#                      signed_age = signed_age,
#                      contract_value = contract_value, 
#                      aav = aav,
#                      sign_bonus = sign_bonus
# )
# 
# nba_salary <- clean_data(nba_salary)
# 
# rank <- scrape_data(mlb, ".noborderright")
# player_name <- scrape_data(mlb, ".team-name")
# position <- scrape_data(mlb, ".rank-name+ .small")
# signed_age <- scrape_data(mlb, "td:nth-child(4)")
# contract_length <- scrape_data(mlb, ".small+ td.small")
# contract_value <- scrape_data(mlb, ".result .cap")
# aav <- scrape_data(mlb, ".result+ .right")
# sign_bonus <- scrape_data(mlb, "td:nth-child(7)")
# 
# mlb_salary <- tibble(rank=rank,
#                      name = player_name,
#                      position = position,
#                      contract_length = contract_length, 
#                      signed_age = signed_age,
#                      contract_value = contract_value, 
#                      aav = aav,
#                      sign_bonus = sign_bonus
# )
# 
# mlb_salary <- clean_data(mlb_salary)
# 
# 
# save(nfl_salary, nhl_salary, nba_salary, mlb_salary, nfl_performance,
#      nhl_performance, nba_performance, mlb_performance,
#      file = "../data/salary.Rdata")
