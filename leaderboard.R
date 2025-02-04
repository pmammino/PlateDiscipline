library(plotly)
library(reshape)
library(devtools)
library(tidyverse)
library(baseballr)
library(ggplot2)
library(mgcv)
library(REdaS)
library(gridExtra)
library(lme4)
library(Hmisc)
library(SDMTools)
setwd("~/GitHub/plate_discipline")

master <- read.csv("master.csv")
master <- master[,c("fg_id","mlb_id")]
master$fg_id <- as.numeric(as.character(master$fg_id))
master$mlb_id <- as.numeric(as.character(master$mlb_id))

results_2019 <- fg_pitch_leaders(2019,2019,qual = 20)
results_2019 <- results_2019[,c("playerid",
                                "Seasons",
                                "Name",
                                "IP",
                                "ERA",
                                "FIP",
                                "xFIP")]
results_2018 <- fg_pitch_leaders(2018,2018,qual = 20)
results_2018 <- results_2018[,c("playerid",
                                "Seasons",
                                "Name",
                                "IP",
                                "ERA",
                                "FIP",
                                "xFIP")]
results_2017 <- fg_pitch_leaders(2017,2017,qual = 20)
results_2017 <- results_2017[,c("playerid",
                                "Seasons",
                                "Name",
                                "IP",
                                "ERA",
                                "FIP",
                                "xFIP")]
results_2016 <- fg_pitch_leaders(2016,2016,qual = 20)
results_2016 <- results_2016[,c("playerid",
                                "Seasons",
                                "Name",
                                "IP",
                                "ERA",
                                "FIP",
                                "xFIP")]

results_2015 <- fg_pitch_leaders(2015,2015,qual = 20)
results_2015 <- results_2015[,c("playerid",
                                "Seasons",
                                "Name",
                                "IP",
                                "ERA",
                                "FIP",
                                "xFIP")]

all_seasons <- readRDS("plate_discipline_all_woba.rds")
all_seasons <- all_seasons[,c("pitcher",
                              "player_name",
                              "Season",
                              "Pitches",
                              "IZ",
                              "OOZ",
                              "In_Whiff",
                              "In_wOBA",
                              "Command",
                              "StuffERA")]

woba <- all_seasons %>% 
  select(pitcher,player_name,Season,Pitches,In_wOBA)

woba <- woba %>%
  pivot_wider(id_cols = c(pitcher,player_name), 
              names_from = Season, 
              values_from = c("Pitches", "In_wOBA"))

woba[is.na(woba)] <- 0

woba$Average <- round(((woba$Pitches_2015 * woba$In_wOBA_2015) +
                        (woba$Pitches_2016 * woba$In_wOBA_2016) +
                        (woba$Pitches_2017 * woba$In_wOBA_2017) +
                        (woba$Pitches_2018 * woba$In_wOBA_2018) +
                        (woba$Pitches_2019 * woba$In_wOBA_2019))/(woba$Pitches_2015 +
                                                                    woba$Pitches_2016 +
                                                                    woba$Pitches_2017 +
                                                                    woba$Pitches_2018 +
                                                                    woba$Pitches_2019 + 5000),5)


results_all_seasons <- rbind(results_2015,results_2016,results_2017,results_2018,results_2019)
results_all_seasons$playerid <- as.numeric(results_all_seasons$playerid)
results_all_seasons <- left_join(results_all_seasons,master, by = c("playerid" = "fg_id"))

results_all_seasons_NA <- results_all_seasons %>%
  filter(is.na(mlb_id))
results_all_seasons_NA <- left_join(results_all_seasons_NA, all_seasons[,c("pitcher", "player_name")], by = c("Name" = "player_name"))
results_all_seasons_NA <- distinct(results_all_seasons_NA)
results_all_seasons_NA$mlb_id <- results_all_seasons_NA$pitcher
results_all_seasons_NA <- results_all_seasons_NA[,c("playerid",
                                                    "Seasons",
                                                    "Name",
                                                    "IP",
                                                    "ERA",
                                                    "FIP",
                                                    "xFIP",
                                                    "mlb_id")]
results_all_seasons <- rbind(results_all_seasons,results_all_seasons_NA)

results_all_seasons <- results_all_seasons %>%
  filter(!is.na(mlb_id))
results_all_seasons$Seasons <- as.numeric(results_all_seasons$Seasons)

results_all_seasons_Stuff <- left_join(results_all_seasons,all_seasons, by = c("mlb_id" = "pitcher", "Seasons" = "Season"))

results_next <- results_all_seasons_Stuff %>% 
  filter(IP >= 120) %>%
  arrange(playerid, Seasons) %>%
  group_by(playerid) %>% 
  mutate(ERA_Next = dplyr::lead(ERA, n=1, default=NA))

results_next <- results_next %>%
  filter(!is.na(ERA_Next))

results_next <- left_join(results_next,woba[,c("pitcher","Average")], by = c("mlb_id" = "pitcher"))

era_stuff_model_next <- lm(ERA_Next ~ In_Whiff + IZ + Average + Command,
                                 data = results_next)
results_next$StuffERANext <- predict(era_stuff_model_next,results_next)

innings_list <- seq(20, 160, 20)

all_correlations_next <- function(innings){
  results_next <- results_all_seasons_Stuff %>% 
    filter(IP >= innings) %>%
    arrange(playerid, Seasons) %>%
    group_by(playerid) %>% 
    mutate(ERA_Next = dplyr::lead(ERA, n=1, default=NA))
  
  results_next <- results_next %>%
    filter(!is.na(ERA_Next))
  
  results_next <- left_join(results_next,woba[,c("pitcher","Average")], by = c("mlb_id" = "pitcher"))
  
  results_next$StuffERANext <- predict(era_stuff_model_next, newdata = results_next)
  
  correlations <- data.frame("IP" = innings,
                             "NextFIP_Cor" = round(cor(results_next$ERA_Next,results_next$FIP),3), 
                             "NextStuff_Cor" = round(cor(results_next$ERA_Next,results_next$StuffERANext),3))
  correlations
  
}

all_correlations_next <- lapply(innings_list, all_correlations_next)
all_correlations_next <- do.call("rbind", all_correlations_next)

all_correlations_next <- all_correlations_next %>%
  gather(key = "variable", value = "value", -IP)

corr_next_plot <- ggplot(all_correlations_next, aes(x = IP, y = value)) + 
  geom_line(aes(color = variable), lwd=1.5) + 
  geom_point(aes(color = variable), size = 4) +
  scale_color_manual(values = c("green", "blue")) +
  theme_bw() +
  labs(x = "Innings Pitched (Min)",
       y = "Correlation",
       caption = "@pmamminofantasy",
       title = "Correlations To Future ERA By IP Totals",
       subtitle = "FIP and New_Stuff") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10))
corr_next_plot



all_seasons_pt <- readRDS("all_results_allpitches_allseasons.rds")

results_all_seasons_Stuff <- left_join(results_all_seasons_Stuff,woba[,c("pitcher","Average")], by = c("mlb_id" = "pitcher"))

results_all_seasons_Stuff$ERA_Next <- round(predict(era_stuff_model_next,results_all_seasons_Stuff),3)

results_all_seasons_Stuff$Improve <- results_all_seasons_Stuff$ERA_Next - results_all_seasons_Stuff$ERA