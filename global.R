library(WNBAballr)
library(httr)
library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(ggthemes)
library(ggdark)
library(scales)
library(extrafont)
library(shiny)
library(shinythemes)
library(reactable)
library(ggplot2)
library(extrafont)

#############################################################
### read in bio details
wnba_bio_2020 <- read_csv("https://raw.githubusercontent.com/katiesegreti/WNBA/master/wnba_2020_bio.csv") %>% unique()

### read in history
wnba_history <- read_csv("https://raw.githubusercontent.com/katiesegreti/WNBA/master/WNBA_player_historical.csv")

### read in shooting data
wnba_shooting <- read_csv("https://raw.githubusercontent.com/katiesegreti/WNBA/master/WNBA_2020_shooting_players.csv") %>%
  mutate( player = as.factor(player),
          tm = as.factor(tm),
          zone = as.factor(zone),
          stat = as.factor(stat),
          shots = as.factor(shots)
          )

# function to clean player stats
clean_season <- function(df, season) {
  numbers <- df[,4:27] %>% mutate_if(is.character, as.numeric)
  deets <- data.frame(player = df$player, season = season, 
                      tm = as.factor(df$tm), pos = as.factor(df$pos))
  cleaned <- cbind(deets, numbers) %>%
    mutate(
      avg_mp = round(mp/g, 1),
      avg_fg = round(fg / g, 1),
      avg_fga = round(fga / g, 1),
      avg_3p = round(x3p / g, 1),
      avg_3pa = round(x3pa / g, 1),
      avg_2p = round(x2p / g, 1),
      avg_2pa = round(x2pa / g, 1),
      avg_ft = round(ft / g, 1),
      avg_fta = round(fta / g, 1),
      avg_pts = round(pts / g, 1),
      avg_orb = round(orb / g, 1),
      avg_trb = round(trb / g, 1),
      avg_ast = round(ast / g, 1),
      avg_blk = round(blk / g, 1),
      avg_stl = round(stl / g, 1),
      avg_tov = round(tov / g, 1),
      avg_pf = round(pf / g, 1)
    )
}

# get the current 2020 stats 
unclean_2020 <- WNBAPerGameStatistics(season = 2020)
player_stats_2020 <- clean_season(unclean_2020, "2020*")

#combine today's 2020 stats with history (also combine season and team into a column for chart making)
current_stats <- rbind(wnba_history, player_stats_2020) #%>%
  #mutate(season_team = paste0(season, " ", tm))

#JOIN THE LATEST STATS WITH BIO DETAILS
wnba_today <- current_stats %>%
  left_join(wnba_bio_2020)

wnba_today1 <- wnba_today %>%
  mutate(season_team = paste0(season, " ", tm))

#filter to TOT for players on >1 team in 2020
player_counts <- wnba_today %>% filter(season == "2020*") %>%
  count(player)

#filter to "TOT" for players who were on more than one team
players_2020 <- wnba_today %>% 
  filter(season == "2020*") %>%
  left_join(player_counts) %>%
  filter(n == 1 | tm == "TOT") %>%
  mutate(hilite = 0)

# wnba team colors
ATL <- "#C8102E"
CHI <- "#418FDE"
CON <- "#A6192E"
DAL <- "#00A9E0"
IND <- "#FFCD00"
LVA <- "#85714D"
LAS <- "#702F8A"
MIN <- "#78BE20"
NYL <- "#FF671F"
PHO <- "#201747"
SEA <- "#2C5234"
WAS <- "#8A8D8F"
SAS <- "#85714D"

team_colors <- c("ATL" = ATL, "CHI" = CHI, "CON" = CON, "DAL" = DAL,
                 "IND" = IND, "LVA" = LVA, "LAS" = LAS, "MIN" = MIN,
                 "NYL" = NYL, "PHO" = PHO, "SEA" = SEA, "WAS" = WAS, "SAS" = SAS)

team_names <- c("", "Atlanta Dream" = "ATL", "Chicago Sky" = "CHI", "Connecticut Sun" = "CON",
                "Dallas Wings" = "DAL", "Indiana Fever" = "IND", "Las Vegas Aces" = "LVA",
                "Los Angeles Sparks" = "LAS", "Minnesota Lynx" = "MIN", "New York Liberty" = "NYL",
                "Phoenix Mercury" = "PHO", "Seattle Storm" = "SEA", "Washington Mystics" = "WAS"
                )

#non-dark theme
bg_color <- "white"
nyl_theme <- theme_wsj() +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x = element_text(face = "plain", family = "Arial"),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.title = element_text(size = 16, family = "Arial"),
    legend.background = element_rect(fill = bg_color),
    plot.subtitle = element_text(size = 12, family = "Arial")
  )
#dark mode theme
wnba_darkmode <- dark_theme_bw() +
  theme(
    text = element_text(family = "Arial", color = "white"),
    line = element_line(color = "#2bd1fc"),
    plot.subtitle = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 12),
    axis.line = element_line(color = "#2bd1fc"),
    axis.text = element_text(size = 12),
    # axis.ticks = element_line(color = "#ff48c4"),
    panel.grid.major.y = element_line(color = "#2bd1fc", linetype = "dotted"),
    panel.grid.minor.y = element_line(color = "#2bd1fc", linetype = "dotted"),
    panel.grid.major.x = element_line(color = "black"),
    legend.position = "none",
    panel.border = element_rect(color = "#2bd1fc")
  )

#sticky style for reactable
sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                     borderRight = "1px solid #eee")

# write a function to make avg_point charts by season for a player
avgpoint_chart <- function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$avg_pts)
  df %>% ggplot(aes(x = season_team, y = avg_pts)) +
    geom_col(fill = "#dd1f22") +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(avg_pts, 1)," pts"), 
                   y = avg_pts + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Points: ", player_name),
         x = "season",
         y = "") +
    nyl_theme
}

#fg % chart
fgpct_chart <- function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$fg_percent)
  df %>% ggplot(aes(x = season_team, y = fg_percent)) +
    geom_col(fill = "#dd1f22") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(fg_percent * 100, "%"), 
                   y = fg_percent + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Field Goal Percentage: ", player_name),
         x = "",
         y = "") +
    nyl_theme
}

#fg % chart
threepct_chart <- function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$x3p_percent)
  df %>% ggplot(aes(x = season_team, y = x3p_percent)) +
    geom_col(fill = "#dd1f22") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(x3p_percent * 100, "%"), 
                   y = x3p_percent + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("3 Point Percentage: ", player_name),
         x = "",
         y = "") +
    nyl_theme
}

#rebound chart
rebound_chart <- function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT") %>% mutate(rebs = avg_orb + avg_trb)
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$rebs)
  df %>% ggplot(aes(x = season_team, y = rebs)) +
    geom_col(fill = "#dd1f22") +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(rebs, 1),""), 
                   y = rebs + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Rebounds: ", player_name),
         x = "season",
         y = "") +
    nyl_theme
}

#assist chart
assist_chart <-  function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$avg_ast)
  df %>% ggplot(aes(x = season_team, y = avg_ast)) +
    geom_col(fill = "#dd1f22") +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(avg_ast, 1),""), 
                   y = avg_ast + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Assists: ", player_name),
         x = "season",
         y = "") +
    nyl_theme
}

#block chart
block_chart <-  function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$avg_blk)
  df %>% ggplot(aes(x = season_team, y = avg_blk)) +
    geom_col(fill = "#dd1f22") +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(avg_blk, 1),""), 
                   y = avg_blk + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Blocks: ", player_name),
         x = "season",
         y = "") +
    nyl_theme
}

#steal chart
steal_chart <-  function(player_name) {
  df <- wnba_today1 %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$avg_stl)
  df %>% ggplot(aes(x = season_team, y = avg_stl)) +
    geom_col(fill = "#dd1f22") +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(avg_stl, 1),""), 
                   y = avg_stl + max_value / 25 )) +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Steals: ", player_name),
         x = "season",
         y = "") +
    nyl_theme
}

#team comparison chart for pts
team_compare_player_pts <- function(player_name, team, team_fullname) {
  df <- wnba_today %>% filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite))
  max_value <- max(df$avg_pts)
  df %>% ggplot(aes(x = reorder(player, avg_pts), y = avg_pts, fill = hilite)) +
    geom_col() +
    geom_text(aes(label = avg_pts), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    coord_flip() +
    labs(title = paste0("Average points compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}
#league comparison chart for pts
league_compare_player_pts <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_pts), y = avg_pts, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    labs(title = paste0("Average points compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}

#team comparison chart for fg%
team_compare_player_fg <- function(player_name, team, team_fullname) {
  wnba_today %>%
    filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, fg_percent), y = fg_percent, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    labs(title = paste0("Field goal percentage compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}

#leage comparison chart for fg%
league_compare_player_fg <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, fg_percent), y = fg_percent, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    scale_y_continuous(labels = percent) +
    labs(title = paste0("Field goal percentage compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}
#team comparison chart for 3p%
team_compare_player_3p <- function(player_name, team, team_fullname) {
  wnba_today %>%
    filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, x3p_percent), y = x3p_percent, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    labs(title = paste0("3 point percentage compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}
#leage comparison chart for 3p%
league_compare_player_3p <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, x3p_percent), y = x3p_percent, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    scale_y_continuous(labels = percent) +
    labs(title = paste0("3 point percentage compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}

#team comparison for rebounds
team_compare_player_rebounds <- function(player_name, team, team_fullname) {
  wnba_today %>% 
    filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    mutate(rebs = avg_orb + avg_trb) %>%
    ggplot(aes(x = reorder(player, rebs), y = rebs, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    coord_flip() +
    labs(title = paste0("Average rebounds compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}
#league comparison chart for rebounds
league_compare_player_rebounds <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    mutate(rebs = avg_orb + avg_trb) %>%
    ggplot(aes(x = reorder(player, rebs), y = rebs, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    labs(title = paste0("Average rebounds compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}
#team comparison chart for assists
team_compare_player_assists <- function(player_name, team, team_fullname) {
  wnba_today %>%
    filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_ast), y = avg_ast, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    coord_flip() +
    labs(title = paste0("Average assists compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}
#league comparison chart for assists
league_compare_player_assists <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_ast), y = avg_ast, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    labs(title = paste0("Average assists compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}
#team comparison chart for blocks
team_compare_player_blocks <- function(player_name, team, team_fullname) {
  wnba_today %>%
    filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_blk), y = avg_blk, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    coord_flip() +
    labs(title = paste0("Average blocks compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}
#league comparison chart for blocks
league_compare_player_blocks <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_blk), y = avg_blk, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    labs(title = paste0("Average blocks compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}
#team comparison chart for steals
team_compare_player_steals <- function(player_name, team, team_fullname) {
  wnba_today %>%
    filter(tm == team & season == "2020*") %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_stl), y = avg_stl, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    coord_flip() +
    labs(title = paste0("Average steals compared to ", team_fullname, ": ", player_name),
         x = "",
         y = "") +
    nyl_theme 
}
#league comparison chart for steals
league_compare_player_steals <- function(player_name) {
  players_2020 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, avg_stl), y = avg_stl, fill = hilite)) +
    geom_col() +
    scale_fill_manual(values = c("#5b5858", "#dd1f22")) +
    scale_x_discrete(labels = "") +
    labs(title = paste0("Average steals compared to WNBA: ", player_name),
         x = "",
         y = "") +
    nyl_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}

#make it a function that takes player name
shooting_chart <- function(player_name) {
  df <- wnba_shooting %>%
    filter(player == toupper(player_name) ) %>%
    mutate(shots = ordered(shots, levels = c("missed", "made"))) 
  max_fga <- max(df$FGA, na.rm = TRUE)
  df %>%
    ggplot(aes(x = reorder(zone, FGA), y = value, fill = shots)) +
    geom_col() +
    geom_text(aes(label = ifelse(value > 1, value, ""), y = ifelse(shots == "made", value / 2, (FGA - value / 2)))) +
    geom_label(aes(label = ifelse(FGA > 0, paste0(percent, "%"), "-"), 
                   y = ifelse(FGA > 0, FGA + max_fga * 0.05, 1)), 
               fill = "#385097", color = "white") +
    scale_x_discrete(labels = c("right" = "right corner 3", "restricted" = "restricted area",
                                "paint" = "in the paint", "mid" = "mid range", "left" = "left corner 3",
                                "break" = "above the break 3")) +
    scale_fill_manual(values = c("grey", "#dd1f22")) +
    coord_flip() +
    nyl_theme + theme(
      legend.position = "top"
    ) +
    labs(
      title = paste0("Shooting by Zone - ", player_name),
      subtitle = "Field Goal Attempts - 2020 Season",
      x = "",
      y = ""
    )
}