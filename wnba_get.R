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

wnba2020_names <- WNBAPerGameStatistics(season = 2020)

#player names and ids for their bio stats, don't do this everyday.
players <- wnba2020_names$player %>%
  str_remove_all("\'") %>%
  tolower() %>%
  str_replace_all(" ", "-") %>%
  str_replace_all("Ã¡", "a") %>%
  str_replace_all("Ã¼", "u")

# get player id from their player pages
player_ids <- c()
for (k in 1:length(players)) {
  url <- paste0("https://www.wnba.com/player/", players[k], "/#/stats")
  player_id <- read_html(url) %>%
    html_nodes('.player-profile') %>%
    html_attr("data-playerid")
  player_ids <- c(player_ids, player_id)
}

bio_info <- data.frame(player = wnba2020_names$player, 
                       snake_name = players, 
                       player_id = player_ids,
                       dob = 0,
                       height = 0,
                       weight = 0,
                       college = 0)



# loop throught all players to get their details
for(j in 1:nrow(bio_info)) {
  p_url <- paste0("https://a.data.nba.com/wnba/player/", bio_info$player_id[j])
  p <- GET(p_url)
  details <- content(p, "parsed")
  bio_info$dob[j] <- ifelse(length(details$data$info$dob)>0, details$data$info$dob, NA)
  bio_info$height[j] <- ifelse(length(details$data$info$ht)>0, details$data$info$ht, NA)
  bio_info$weight[j] <- ifelse(length(details$data$info$wt)>0, details$data$info$wt, NA)
  bio_info$college[j] <- ifelse(length(details$data$info$hcc)>0, details$data$info$hcc, NA)
}

#save this now, you don't need to do it every time
write_csv(bio_info, "wnba_2020_bio.csv")

#############################################################
########START HERE IF YOU DON'T NEED TO REDO THE BIOS########
#############################################################
wnba_bio_2020 <- read_csv("wnba_2020_bio.csv")

#GET THE LATEST STATS
current_stats1 <- WNBAPerGameStatistics(season = 2020)
str(current_stats1)
current_stats_numeric <- current_stats1[,4:27] %>% mutate_if(is.character, as.numeric)
str(current_stats_numeric)
current_stats <- cbind(current_stats1[,1:3], current_stats_numeric) %>%
  mutate(tm = as.factor(tm),
         pos = as.factor(pos)
         )
str(current_stats)

#JOIN THE LATEST STATS WITH BIO DETAILS
wnba_today <- current_stats %>%
  left_join(wnba_bio_2020)
str(wnba_today)

#calculate averages for pts etc
wnba_today <- wnba_today %>%
  mutate(avg_pts = pts / g,
         avg_orb = orb / g,
         avg_trb = trb / g,
         avt_ast = ast / g,
         avg_blk = blk / g,
         avg_stl = stl / g,
         avg_tov = tov / g,
         avg_pf = pf / g
         )
str(wnba_today)


# function to clean player stats
clean_season <- function(df, season) {
  numbers <- df[,4:27] %>% mutate_if(is.character, as.numeric)
  deets <- data.frame(player = df$player, season = season, 
                      tm = as.factor(df$tm), pos = as.factor(df$pos))
  cleaned <- cbind(deets, numbers) %>%
    mutate(avg_mp = round(mp/g, 1),
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

#get previous seasons
unclean_2019 <- WNBAPerGameStatistics(season = 2019)
unclean_2018 <- WNBAPerGameStatistics(season = 2018)
unclean_2017 <- WNBAPerGameStatistics(season = 2017)
unclean_2016 <- WNBAPerGameStatistics(season = 2016)
unclean_2015 <- WNBAPerGameStatistics(season = 2015)
unclean_2014 <- WNBAPerGameStatistics(season = 2014)
unclean_2013 <- WNBAPerGameStatistics(season = 2013)
unclean_2012 <- WNBAPerGameStatistics(season = 2012)
unclean_2011 <- WNBAPerGameStatistics(season = 2011)


player_stats_2019 <- clean_season(unclean_2019, 2019)
player_stats_2018 <- clean_season(unclean_2018, 2018)
player_stats_2017 <- clean_season(unclean_2017, 2017)
player_stats_2016 <- clean_season(unclean_2016, 2016)
player_stats_2015 <- clean_season(unclean_2015, 2015)
player_stats_2014 <- clean_season(unclean_2014, 2014)
player_stats_2013 <- clean_season(unclean_2013, 2013)
player_stats_2012 <- clean_season(unclean_2012, 2012)
player_stats_2011 <- clean_season(unclean_2011, 2011)


#combine 2011 - 2019 (this stays the same!)
historical_player_stats <- bind_rows(list(player_stats_2011, player_stats_2012, player_stats_2013, 
                                          player_stats_2014, player_stats_2015, player_stats_2016, 
                                          player_stats_2017, player_stats_2018, player_stats_2019))
#save it to reuse
str(historical_player_stats)

#read in historical csv
wnba_history <- write_csv(historical_player_stats, "WNBA_player_historical.csv") %>%
  mutate(tm = as.factor(tm),
         pos = as.factor(pos))
str(wnba_history)

# get the current 2020 stats 
# rerun this every day during the season
unclean_2020 <- WNBAPerGameStatistics(season = 2020)
player_stats_2020 <- clean_season(unclean_2020, "2020*")

#combine today's 2020 stats with history (also combine season and team into a column for chart making)
wnba_today <- rbind(wnba_history, player_stats_2020) %>%
  mutate(season_team = paste0(season, " ", tm))


jj <- wnba_today %>% filter(player == "Jazmine Jones")
mw <- wnba_today %>% filter(player == "Megan Walker")

kn <- wnba_today %>% filter(player == "Kia Nurse")
liberty <- wnba_today %>% filter(tm == "NYL")

lo <- wnba_today %>% filter(player == "Epiphanny Prince")

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

team_colors <- c("ATL" = ATL, "CHI" = CHI, "CON" = CON, "DAL" = DAL,
                 "IND" = IND, "LVA" = LVA, "LAS" = LAS, "MIN" = MIN,
                 "NYL" = NYL, "PHO" = PHO, "SEA" = SEA, "WAS" = WAS)

bg_color <- "white"
nyl_theme <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x = element_text(face = "plain", family = "Arial"),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.title = element_text(size = 20, family = "Arial"),
    plot.subtitle = element_text(size = 15, family = "Arial")
  )


wnba_today %>%
  filter(player == "Kia Nurse" ) %>%
  ggplot(aes(x = season, y = avg_pts)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = paste0(round(avg_pts, 1)," pts/game"), 
                y = avg_pts + 0.5), color = "black") +
  geom_label(aes(label = tm, y = avg_pts - 1, fill = tm)) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Average Points: Kia Nurse",
       x = "season",
       y = "") +
  nyl_theme

wnba_today %>%
  filter(player == "Kia Nurse" ) %>%
  ggplot(aes(x = season, y = fg_percent)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = paste0(fg_percent * 100,"%"), 
                y = fg_percent + 0.02), color = "black") +
  geom_label(aes(label = tm, y = fg_percent - 0.05, fill = tm), size = 5) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Field goal percentage: Kia Nurse",
       x = "season",
       y = "") +
  nyl_theme


wnba_today %>%
  filter(player == "Epiphanny Prince" ) %>%
  ggplot(aes(x = season, y = fg_percent)) +
  geom_col(fill = "grey35") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  geom_text(aes(label = paste0(fg_percent * 100,"%"), 
                y = fg_percent - 0.02)) +
  geom_label(aes(label = tm, fill = tm), y = 0.05, size = 6) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Field goal percentage: Epiphanny Prince",
       x = "season",
       y = "") +
  nyl_theme

wnba_today %>%
  filter(player == "Epiphanny Prince" ) %>%
  ggplot(aes(x = season, y = avg_pts)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = paste0(round(avg_pts, 1)," pts/game"), 
                y = avg_pts + 0.5), color = "black") +
  geom_label(aes(label = tm, y = avg_pts - 1, fill = tm), size = 6) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Average Points: Epiphanny Prince",
       x = "season",
       y = "") +
  nyl_theme




# fg percent
player_stats_2020 %>%
  #filter(g > 20) %>%
  filter(fga > 105) %>%
  top_n(10, fg_percent) %>%
  ggplot(aes(x = reorder(player, fg_percent), y = fg_percent)) +
  geom_col() +
  geom_text(aes(label = paste0(round(fg_percent * 100, 2), "%"), y = fg_percent - 0.03), color = "white") +
  geom_label(aes(label = tm, fill = tm), y = 0.03, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  nyl_theme +
  labs(
    title = "WNBA top shooters - 2020 season",
    subtitle = "average field goal percent"
  )


# library(extrafont)
# font_import()
# loadfonts(device = "win")
# windowsFonts()

wnba_today$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()


# write a function to make avg_point charts by season for a player
avgpoint_chart <- function(player_name) {
  df <- wnba_today %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$avg_pts)
    df %>% ggplot(aes(x = season_team, y = avg_pts)) +
    geom_col(fill = "grey35") +
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

avgpoint_chart("Kia Nurse")
avgpoint_chart("Essence Carson")
avgpoint_chart("Amanda Zahui B.")
avgpoint_chart("Aerial Powers")
avgpoint_chart("Layshia Clarendon")

fgpct_chart <- function(player_name) {
  df <- wnba_today %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$fg_percent)
  df %>% ggplot(aes(x = season_team, y = fg_percent)) +
    geom_col(fill = "grey35") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(fg_percent * 100, "%"), 
                  y = fg_percent + max_value / 25 ), color = "black") +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, color = "white") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Field Goal Percentage: ", player_name),
         x = "",
         y = "") +
    nyl_theme
}

fgpct_chart("Kia Nurse")
fgpct_chart("Essence Carson")
fgpct_chart("Amanda Zahui B.")
fgpct_chart("Layshia Clarendon")


#team players charts
# fg percentage
player_stats_2020 %>%
  filter(tm == "NYL" & fga > 1) %>%
  ggplot(aes(x = reorder(player, fg_percent), y = fg_percent)) +
  geom_col(fill = NYL, width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  geom_text(aes(label = paste0(fg_percent * 100, "%"), y = fg_percent - 0.03), color = "white") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  nyl_theme +
  labs(
    title = "2020 New York Liberty",
    subtitle = "field goal percentage as of 8/21"
  )

# avg pts 
player_stats_2020 %>%
  filter(tm == "NYL" & pts > 1) %>%
  ggplot(aes(x = reorder(player, avg_pts), y = avg_pts)) +
  geom_col(fill = "grey35", color = NYL, width = 0.8) +
  
  geom_text(aes(label = round(avg_pts, 1), y = avg_pts - 0.8), color = "white") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  a_theme +
  labs(
    title = "2020 New York Liberty",
    subtitle = "average points per game as of 8/21"
  )

# 3 pts
player_stats_2020 %>%
  filter(tm == "NYL" & x3p > 0) %>%
  ggplot(aes(x = reorder(player, x3p_percent), y = x3p_percent)) +
  geom_col( fill = NYL, width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  geom_text(aes(label = paste0(x3p_percent * 100, "%"), y = x3p_percent - 0.03), color = "white") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  a_theme +
  labs(
    title = "2020 New York Liberty",
    subtitle = "3 point shot percentage as of 8/21"
  )


wnba_darkmode <- dark_theme_bw() +
  theme(
    text = element_text(family = "OCR A Extended", color = "white"),
    line = element_line(color = "#2bd1fc"),
    plot.subtitle = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.caption = element_text(size = 12),
    axis.line = element_line(color = "#2bd1fc"),
    # axis.ticks = element_line(color = "#ff48c4"),
    panel.grid.major.y = element_line(color = "#2bd1fc", linetype = "dotted"),
    panel.grid.minor.y = element_line(color = "#2bd1fc", linetype = "dotted"),
    panel.grid.major.x = element_line(color = "black"),
    legend.position = "none",
    panel.border = element_rect(color = "#2bd1fc")
  )

fgpct_chart_dark <- function(player_name) {
  df <- wnba_today %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$fg_percent)
  df %>% ggplot(aes(x = season_team, y = fg_percent)) +
    geom_col(fill = "#ff48c4") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(fg_percent * 100, "%"), 
                  y = fg_percent + max_value / 25 ), family = "OCR A Extended", color = "#f3ea5f") +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, family = "OCR A Extended") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6) +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Field Goal Percentage: ", player_name),
         subtitle = "*as of 8/21",
         x = "",
         y = "") +
    wnba_darkmode
}
fgpct_chart_dark("Kia Nurse")
fgpct_chart_dark("A'ja Wilson")
fgpct_chart_dark("Glory Johnson")
fgpct_chart_dark("Breanna Stewart")

#avgpoint dark mode
avgpoint_chart_dark <- function(player_name) {
  df <- wnba_today %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist()
  max_value = max(df$avg_pts)
  df %>% ggplot(aes(x = season_team, y = avg_pts)) +
    geom_col(fill = "#ff48c4") +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(avg_pts, 1)," pts"), 
                  y = avg_pts + max_value / 25), family = "OCR A Extended", color = "#f3ea5f") +
    geom_text(aes(label = paste0(g," games")), y = max_value / 6, family = "OCR A Extended") +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 6) +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Points: ", player_name),
         subtitle = "*as of 9/10",
         x = "",
         y = "") +
    wnba_darkmode
}

avgpoint_chart_dark("Glory Johnson")
avgpoint_chart_dark("Jazmine Jones")
avgpoint_chart_dark("Kia Nurse")
avgpoint_chart_dark("Diana Taurasi")
avgpoint_chart_dark("A'ja Wilson")
avgpoint_chart_dark("Breanna Stewart")
avgpoint_chart_dark("Layshia Clarendon")

# avg pts 
player_stats_2020 %>%
  filter(tm == "LVA" & fg > 1) %>%
  ggplot(aes(x = reorder(player, fg_percent), y = fg_percent)) +
  geom_col(fill = "#ff48c4", color = LVA, width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  geom_label(aes(label = paste0(fg_percent * 100, "%"), y = fg_percent - 0.03), color = "white", fill = "#c04df9") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  wnba_darkmode +
  labs(
    title = "2020 Las Vegas Aces",
    subtitle = "field goal percentage as of 8/21",
    x = "",
    y = ""
  )

# avg pts 
player_stats_2020 %>%
  filter(tm == "LVA" & pts > 4) %>%
  ggplot(aes(x = reorder(player, avg_pts), y = avg_pts)) +
  geom_col(fill = "#ff48c4", color = LVA, width = 0.8) +
  
  geom_text(aes(label = round(avg_pts, 1), y = avg_pts - 0.8), color = "white") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  wnba_darkmode +
  labs(
    title = "2020 Las Vegas Aces",
    subtitle = "average points per game as of 8/21",
    x = "",
    y = ""
  )

WNBASeasonTeamByYear(team = "LVA", season = 2020)


# fg percentage
player_stats_2020 %>%
  filter(tm == "SEA" & fga > 1) %>%
  ggplot(aes(x = reorder(player, fg_percent), y = fg_percent)) +
  geom_col(fill = "#ff48c4", width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  geom_text(aes(label = paste0(fg_percent * 100, "%"), y = fg_percent - 0.03), color = "white", family = "OCR A Extended") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  wnba_darkmode +
  labs(
    title = "2020 Seattle Storm",
    subtitle = "field goal percentage as of 8/21",
    x = "",
    y = ""
  )


# avg pts 
player_stats_2020 %>%
  filter(tm == "SEA" & pts > 10) %>%
  ggplot(aes(x = reorder(player, avg_pts), y = avg_pts)) +
  geom_col(fill = "#ff48c4", color = NYL, width = 0.8) +
  
  geom_text(aes(label = round(avg_pts, 1), y = avg_pts - 0.8), color = "white", family = "OCR A Extended") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  wnba_darkmode +
  labs(
    title = "2020 Seattle Storm",
    subtitle = "average points per game as of 8/21",
    x = "",
    y = ""
  )

# avg pts 
player_stats_2020 %>%
  filter(tm == "LVA" & pts > 10) %>%
  ggplot(aes(x = reorder(player, avg_pts), y = avg_pts)) +
  geom_col(fill = "#ff48c4", color = NYL, width = 0.8) +
  
  geom_text(aes(label = round(avg_pts, 1), y = avg_pts - 0.8), color = "white", family = "OCR A Extended") +
  #geom_label(aes(label = tm, fill = tm), y = 3, color = "white") +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  wnba_darkmode +
  labs(
    title = "2020 Las Vegas Aces",
    subtitle = "average points per game as of 8/21",
    x = "",
    y = ""
  )
