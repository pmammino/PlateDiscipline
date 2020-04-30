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
setwd("~/GitHub/plate_discipline")

master <- read.csv("master.csv")
master <- master[,c("fg_id","mlb_id")]
master$fg_id <- as.numeric(as.character(master$fg_id))
master$mlb_id <- as.numeric(as.character(master$mlb_id))


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
results_all_seasons_Stuff$StuffDiff <- results_all_seasons_Stuff$ERA - results_all_seasons_Stuff$StuffERA


all_seasons_pt <- readRDS("all_results_allpitches_allseasons.rds")
