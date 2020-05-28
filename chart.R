library(tidyverse)
library(theme538)
library(teamcolors)

elo_by_week <- read_csv("data/nflelo.csv")

montana_weeks <- elo_by_week %>%
   filter(qb1 == "Joe Montana") %>%
   arrange(date) %>%
   mutate(game_num = row_number())

young_weeks <- elo_by_week %>%
   filter(qb1 == "Steve Young") %>%
   arrange(date) %>%
   mutate(game_num = row_number())

# young_weeks %>%
#    filter(game_num < 20) %>%
#    mutate(below_zero = ifelse(qb1_vs_avg_game <= 0, 1, 0)) %>%
#    summarize(below_zero = sum(below_zero)/n())
#
# young_weeks %>%
#    filter(game_num >= 20 & game_num < 39) %>%
#    mutate(below_zero = ifelse(qb1_vs_avg_game <= 0, 1, 0)) %>%
#    summarize(below_zero = sum(below_zero)/n())

young_and_montana_weeks <- montana_weeks %>%
   bind_rows(young_weeks)

young_and_montana_weeks %>%
   filter(game_num >= 20) %>%
   ggplot(aes(x = game_num, y = qb1_vs_avg_game, color = qb1)) +
   geom_vline(xintercept = 20, color = "grey", size = 1, linetype = "dashed") +
   geom_vline(xintercept = 159, color = "grey", size = 1, linetype = "dashed") +
   geom_hline(yintercept = 0, color = "black", size = 1) +
   geom_point(size = 3, alpha = .5) +
   geom_smooth(se = FALSE, size = 2) +
   theme_538 +
   scale_color_manual(values = c("#AA0000", "#B3995D")) +
   labs(x = "Game started", y = "Elo vs. Weekly Average", caption = "Source: FiveThirtyEight", subtitle = "Left dashed vertical line indicates Young's first 49er's start. Right dashed vertical line indicates Montana's last 49ers start.")

young_and_montana_weeks %>%
   group_by(qb1) %>%
   summarize(elo = mean(qb1_vs_avg_game))

### SRS

srs <- read_csv("data/srs.csv")

# Creat table for Carpenter
srs %>%
   filter(Passer %in% c("Montana", "Young")) %>%
   group_by(Passer) %>%
   summarise(mov = mean(MoV),
             OSRS = mean(OSRS),
             DSRS = mean(DSRS), seasons = n())
