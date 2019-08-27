# Get a list of every pair of players who have played 22 or more 
#   head to head matches in ATP or WTA tennis:

# First, download the data from Jeff Sackmann's github repo:
# for year in {1968..2019}; do wget https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_$year.csv --no-check-certificate; done
# for year in {1968..2018}; do wget https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_$year.csv --no-check-certificate; done

# R setup:
library(dplyr)
library(data.table)
library(tidyr)
setwd("~/tennis")  # forgive me, Jenny Bryan

# read in the results files for ATP:
atp <- vector("list", length(1968:2019))
for (i in 1:length(atp)) {
  atp[[i]] <- fread(paste0("data/atp/atp_matches_", 1967 + i, ".csv"), data.table = FALSE)
}

# combine all 52 years of data into a single data frame and sort chronologically:
atp <- bind_rows(atp)
atp <- arrange(atp, tourney_date)
n <- nrow(atp)
# 169,690 matches

# count unique matchups:
atp <- atp %>%
  mutate(lower = pmin(winner_id, loser_id), 
         upper = pmax(winner_id, loser_id))

# count number of head to head matches for each pair of players
m <- atp %>% group_by(lower, upper) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame()

N <- sum(m$n >= 22)
N
# only 35 pairs of players with 22 or more head-to-head matchups in this data

# filter match-level data to just these 35 matchups
df <- inner_join(atp, filter(m, n >= 22))

# for each matchup, compute cumulative win-loss record
h2h <- vector("list", N)
for (i in 1:N) {
  h2h[[i]] <- filter(df, lower == m$lower[i], upper == m$upper[i]) %>%
    select(winner_id, winner_name, loser_id, loser_name, lower, upper)
  h2h[[i]] <- h2h[[i]] %>%
    mutate(wins = cumsum(winner_id == lower), 
           losses = cumsum(winner_id == upper), 
           player1 = ifelse(winner_id == lower, winner_name, loser_name), 
           player2 = ifelse(winner_id == lower, loser_name, winner_name))
}

# look at the 22nd match between each pair of players:
match_22_atp <- lapply(h2h, slice, 22) %>%
  bind_rows() %>%
  select(wins:player2) %>%
  mutate(W = pmax(wins, losses), 
         L = pmin(wins, losses), 
         W_player = ifelse(wins > losses, player1, player2), 
         L_player = ifelse(wins > losses, player2, player1)) %>%
  select(W:L_player) %>%
  arrange(desc(W))

match_22_atp

# read in the results files for ATP:
wta <- vector("list", length(1968:2018))
for (i in 1:length(wta)) {
  wta[[i]] <- fread(paste0("data/wta/wta_matches_", 1967 + i, ".csv"), 
                    data.table = FALSE, fill = TRUE) %>%
    select(tourney_date, winner_id, winner_name, loser_id, loser_name)
}

# combine all 51 years of data into a single data frame and sort chronologically:
wta <- bind_rows(wta)
wta <- arrange(wta, tourney_date)
n <- nrow(wta)
# 111,598 matches

# count unique matchups:
wta <- wta %>%
  mutate(lower = pmin(winner_id, loser_id), 
         upper = pmax(winner_id, loser_id))

# count number of head to head matches for each pair of players
m <- wta %>% group_by(lower, upper) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame()

N <- sum(m$n >= 22)
N
# only 24 pairs of players with 22 or more head-to-head matchups in the WTA data

# filter match-level data to just these 35 matchups
df <- inner_join(wta, filter(m, n >= 22))

# for each matchup, compute cumulative win-loss record
h2h <- vector("list", N)
for (i in 1:N) {
  h2h[[i]] <- filter(df, lower == m$lower[i], upper == m$upper[i]) %>%
    select(winner_id, winner_name, loser_id, loser_name, lower, upper)
  h2h[[i]] <- h2h[[i]] %>%
    mutate(wins = cumsum(winner_id == lower), 
           losses = cumsum(winner_id == upper), 
           player1 = ifelse(winner_id == lower, winner_name, loser_name), 
           player2 = ifelse(winner_id == lower, loser_name, winner_name))
}

# look at the 22nd match between each pair of players:
match_22_wta <- lapply(h2h, slice, 22) %>%
  bind_rows() %>%
  select(wins:player2) %>%
  mutate(W = pmax(wins, losses), 
         L = pmin(wins, losses), 
         W_player = ifelse(wins > losses, player1, player2), 
         L_player = ifelse(wins > losses, player2, player1)) %>%
  select(W:L_player) %>%
  arrange(desc(W))

match_22_wta

