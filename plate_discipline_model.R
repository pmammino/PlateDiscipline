install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mgcv")
install.packages("REdaS")
install.packages("gridExtra")
install.packages("lme4")
install.packages('reshape')
install.packages("Hmisc")
install.packages("plotly")
library(plotly)
library(reshape)
library(devtools)
library(tidyverse)
install_github("BillPetti/baseballr")
library(baseballr)
library(ggplot2)
library(mgcv)
library(REdaS)
library(gridExtra)
library(lme4)
library(Hmisc)
options(scipen = 999)
memory.limit(100000)
# 
# #Helper Functions -----
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

##Scrape 2019 ----
date <- as.Date("2019-03-19")

all_pitches_2019 <- scrape_statcast_savant(start_date = date,
                       end_date = date, player_type = "pitcher")

while(date < as.Date("2019-09-30"))
{
  if (date + 10 > as.Date("2019-09-30"))
  {
  test <- scrape_statcast_savant(start_date = date,
                                        end_date = as.Date("2019-09-30"), player_type = "pitcher")
  }
  else
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = date + 10, player_type = "pitcher")
  }
  all_pitches_2019 <- rbind(all_pitches_2019,test)
  date <- date + 10
}

##Scrape 2018 ----
date <- as.Date("2018-03-28")

all_pitches_2018 <- scrape_statcast_savant(start_date = date,
                                           end_date = date, player_type = "pitcher")

while(date < as.Date("2018-10-01"))
{
  if (date + 10 > as.Date("2018-10-01"))
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = as.Date("2018-10-01"), player_type = "pitcher")
  }
  else
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = date + 10, player_type = "pitcher")
  }
  all_pitches_2018 <- rbind(all_pitches_2018,test)
  date <- date + 10
}

##Scrape 2017 ----
date <- as.Date("2017-04-01")

all_pitches_2017 <- scrape_statcast_savant(start_date = date,
                                           end_date = date, player_type = "pitcher")

while(date < as.Date("2017-10-01"))
{
  if (date + 10 > as.Date("2017-10-01"))
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = as.Date("2017-10-01"), player_type = "pitcher")
  }
  else
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = date + 10, player_type = "pitcher")
  }
  all_pitches_2017 <- rbind(all_pitches_2017,test)
  date <- date + 10
}

##Scrape 2016 ----
date <- as.Date("2016-04-02")
all_pitches_2016 <- scrape_statcast_savant(start_date = date,
                                           end_date = date, player_type = "pitcher")

while(date < as.Date("2016-10-01"))
{
  if (date + 10 > as.Date("2016-10-02"))
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = as.Date("2016-10-02"), player_type = "pitcher")
  }
  else
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = date + 10, player_type = "pitcher")
  }
  all_pitches_2016 <- rbind(all_pitches_2016,test)
  date <- date + 10
}

##Scrape 2015 ----
date <- as.Date("2015-04-04")
all_pitches_2015 <- scrape_statcast_savant(start_date = date,
                                           end_date = date, player_type = "pitcher")

while(date < as.Date("2015-10-04"))
{
  if (date + 10 > as.Date("2015-10-04"))
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = as.Date("2015-10-04"), player_type = "pitcher")
  }
  else
  {
    test <- scrape_statcast_savant(start_date = date,
                                   end_date = date + 10, player_type = "pitcher")
  }
  all_pitches_2015 <- rbind(all_pitches_2015,test)
  date <- date + 10
}

# ###Set Up----
all_pitches_2019$Season <- 2019
all_pitches_2018$Season <- 2018
all_pitches_2017$Season <- 2017
all_pitches_2016$Season <- 2016
all_pitches_2015$Season <- 2015
#
all_pitches <- rbind(all_pitches_2019,all_pitches_2018,all_pitches_2017,all_pitches_2016,all_pitches_2015)

all_pitches <- all_pitches %>% distinct()
# 
# all_pitches_stats <- all_pitches[,c("Season",
#                                     "pitch_type",
#                                     "player_name",
#                                     "pitcher",
#                                     "p_throws",
#                                     "release_speed",
#                                     "pfx_x",
#                                     "pfx_z",
#                                     "effective_speed",
#                                     "release_spin_rate",
#                                     "release_extension")]
# 
# saveRDS(all_pitches_stats,"all_pitches_stats.rds")                                    
# 
# all_pitches_trim <- all_pitches[,c("Season",
#                                  "pitch_type",
#                                  "player_name",
#                                  "batter",
#                                  "pitcher",
#                                  "description",
#                                  "zone",
#                                  "stand",
#                                  "p_throws",
#                                  "balls",
#                                  "strikes",
#                                  "plate_x",
#                                  "plate_z",
#                                  "fielder_2",
#                                  "woba_value")]
# 
# all_pitches_trim <- mutate(all_pitches_trim, swing=ifelse(description %in% c("hit_into_play", "foul","swinging_strike", "hit_into_play_score", "hit_into_play_no_out", "foul_tip", "swinging_strike_blocked"),
#                                              1, 0))
# all_pitches_trim <-mutate(all_pitches_trim, whiff=ifelse(description %in% c("swinging_strike","swinging_strike_blocked"),
#                                     1, 0))
# all_pitches_trim <- all_pitches_trim[!(is.na(all_pitches_trim$plate_x)) | !(is.na(all_pitches_trim$plate_z)),]
# all_pitches_trim <- all_pitches_trim[!(is.na(all_pitches_trim$pitch_type)),]
# 
# all_pitches_trim <- mutate(all_pitches_trim, pitch_group=ifelse(pitch_type %in% c("FF", "FT","FC", "SI", "FA"),
#                                                            "Fastball", ifelse(pitch_type %in% c("SL", "EP","CU", "KN", "KC"),
#                                                                               "Breaking", "OffSpeed")))
# 
# all_pitches_trim$count <- paste(all_pitches_trim$balls,"-",all_pitches_trim$strikes)
# 
# all_splits <- split(all_pitches_trim, with(all_pitches_trim, interaction(pitch_group,count)), drop = TRUE)
# list2env(all_splits,envir=.GlobalEnv)
# 
# #Swing Predict ----
# all_predict <- all_pitches_trim[FALSE,]
# all_predict$predict <- numeric()
# 
# predict_swing <- function(x)
# {
#   if(nrow(all_splits[[x]]) > 100)
#    {
#      fit <- gam(swing ~ s(plate_x,plate_z), family=binomial, data=all_splits[[x]])
# 
#      all_splits[[x]]$predict <- exp(predict(fit,all_splits[[x]]))/(1 + exp(predict(fit,all_splits[[x]])))
# 
#      all_predict <- rbind(all_predict,all_splits[[x]])
#    }
#  }
# 
# 
# 
# all_data <- lapply(1:length(all_splits), predict_swing)
# all_data <- do.call("rbind", all_data)
# 
# saveRDS(all_pitches_trim,"all_pitches_trim.rds")
# saveRDS(all_data,"all_data.rds")

all_pitches_trim <- readRDS("all_pitches_trim.rds")
all_data <- readRDS("all_data.rds")

in_zone <- all_data %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone <- all_data %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
               data = in_zone)

model_out_of_zone <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = out_of_zone)

out_of_zone_pitchers <- data.frame(ranef(model_out_of_zone)$pitcher)
colnames(out_of_zone_pitchers) <- "OOZ"

in_zone_pitchers <- data.frame(ranef(model_zone)$pitcher)
colnames(in_zone_pitchers) <- "IZ"

all_results <- merge(in_zone_pitchers,out_of_zone_pitchers,by = "row.names")
colnames(all_results) <- c("pitcher","IZ","OOZ")
all_results$pitcher <- as.numeric(all_results$pitcher)

in_zone_data <- in_zone %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data <- out_of_zone %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)


##Whiff Predict ----
# all_predict_whiff_predict <- all_pitches_trim[FALSE,]
# all_predict_whiff_predict$predict_whiff <- numeric()
#  
# predict_whiff <- function(x)
#   {
#  if(nrow(all_splits[[x]]) > 100)
#    {
#    fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=all_splits[[x]])
#      
#    all_splits[[x]]$predict_whiff <- exp(predict(fit,all_splits[[x]]))/(1 + exp(predict(fit,all_splits[[x]])))
#      
#    all_splits <- rbind(all_predict_whiff_predict,all_splits[[x]])
#   }
# }
#  
# whiff_predict_data <- lapply(1:length(all_splits), predict_whiff)
# whiff_predict_data <- do.call("rbind", whiff_predict_data)
# 
# 
# saveRDS(whiff_predict_data,"whiff_predict_data.rds")

whiff_predict_data <- readRDS("whiff_predict_data.rds")

model_whiff <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                   data = whiff_predict_data)

whiff_pitchers <- data.frame(ranef(model_whiff)$pitcher)
colnames(whiff_pitchers) <- "In_Whiff"

whiff_data <- whiff_predict_data %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data <- merge(whiff_data,whiff_pitchers, by.x = "pitcher", by.y = "row.names")

###Plate Discipline All ----
plate_discipline <- merge(whiff_data,out_of_zone_data,by = "pitcher")
plate_discipline <- merge(plate_discipline,in_zone_data,by = "pitcher")
plate_discipline <- merge(plate_discipline,all_results,by = "pitcher")
plate_discipline <- plate_discipline[,c("pitcher",
                                        "player_name",
                                        "Pitches",
                                        "Whiff",
                                        "xWhiff",
                                        "In_Whiff",
                                        "OOZ.Pitches",
                                        "OOZ.Swing",
                                        "OOZ.xSwing",
                                        "OOZ",
                                        "IZ.Pitches",
                                        "IZ.Swing",
                                        "IZ.xSwing",
                                        "IZ")]
plate_discipline$Score <- plate_discipline$OOZ - plate_discipline$IZ + plate_discipline$In_Whiff

write.csv(plate_discipline, "plate_discipline.csv")
###2019 Whiff ----
whiff_predict_data_2019 <- whiff_predict_data %>% 
                            filter(Season == 2019)
model_whiff_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                    data = whiff_predict_data_2019)

whiff_pitchers_2019 <- data.frame(ranef(model_whiff_2019)$pitcher)
colnames(whiff_pitchers_2019) <- "In_Whiff"

whiff_data_2019 <- whiff_predict_data_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_2019 <- merge(whiff_data_2019,whiff_pitchers_2019, by.x = "pitcher", by.y = "row.names")

###2018 Whiff ----
whiff_predict_data_2018 <- whiff_predict_data %>% 
  filter(Season == 2018)
model_whiff_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                         data = whiff_predict_data_2018)

whiff_pitchers_2018 <- data.frame(ranef(model_whiff_2018)$pitcher)
colnames(whiff_pitchers_2018) <- "In_Whiff"

whiff_data_2018 <- whiff_predict_data_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_2018 <- merge(whiff_data_2018,whiff_pitchers_2018, by.x = "pitcher", by.y = "row.names")

###2017 Whiff ----
whiff_predict_data_2017 <- whiff_predict_data %>% 
  filter(Season == 2017)
model_whiff_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                         data = whiff_predict_data_2017)

whiff_pitchers_2017 <- data.frame(ranef(model_whiff_2017)$pitcher)
colnames(whiff_pitchers_2017) <- "In_Whiff"

whiff_data_2017 <- whiff_predict_data_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_2017 <- merge(whiff_data_2017,whiff_pitchers_2017, by.x = "pitcher", by.y = "row.names")

###2016 Whiff ----
whiff_predict_data_2016 <- whiff_predict_data %>% 
  filter(Season == 2016)
model_whiff_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                         data = whiff_predict_data_2016)

whiff_pitchers_2016 <- data.frame(ranef(model_whiff_2016)$pitcher)
colnames(whiff_pitchers_2016) <- "In_Whiff"

whiff_data_2016 <- whiff_predict_data_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_2016 <- merge(whiff_data_2016,whiff_pitchers_2016, by.x = "pitcher", by.y = "row.names")

###2015 Whiff ----
whiff_predict_data_2015 <- whiff_predict_data %>% 
  filter(Season == 2015)
model_whiff_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                         data = whiff_predict_data_2015)

whiff_pitchers_2015 <- data.frame(ranef(model_whiff_2015)$pitcher)
colnames(whiff_pitchers_2015) <- "In_Whiff"

whiff_data_2015 <- whiff_predict_data_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_2015 <- merge(whiff_data_2015,whiff_pitchers_2015, by.x = "pitcher", by.y = "row.names")

#Season Whiffs----
all_whiff_seasons <- rbind(whiff_data_2019,whiff_data_2018,whiff_data_2017,whiff_data_2016,whiff_data_2015)


####2019 PD ----
all_data_2019 <- all_data %>% filter(Season == 2019)
in_zone_2019 <- all_data_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_2019 <- all_data_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                   data = in_zone_2019)

model_out_of_zone_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = out_of_zone_2019)

out_of_zone_pitchers_2019 <- data.frame(ranef(model_out_of_zone_2019)$pitcher)
colnames(out_of_zone_pitchers_2019) <- "OOZ"

in_zone_pitchers_2019 <- data.frame(ranef(model_zone_2019)$pitcher)
colnames(in_zone_pitchers_2019) <- "IZ"

all_results_2019 <- merge(in_zone_pitchers_2019,out_of_zone_pitchers_2019,by = "row.names")
colnames(all_results_2019) <- c("pitcher","IZ","OOZ")
all_results_2019$pitcher <- as.numeric(all_results_2019$pitcher)
all_results_2019$Season <- 2019

in_zone_data_2019 <- in_zone_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_2019$Season <- 2019

out_of_zone_data_2019 <- out_of_zone_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_2019$Season <- 2019

####2018 PD ----
all_data_2018 <- all_data %>% filter(Season == 2018)
in_zone_2018 <- all_data_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_2018 <- all_data_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                        data = in_zone_2018)

model_out_of_zone_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = out_of_zone_2018)

out_of_zone_pitchers_2018 <- data.frame(ranef(model_out_of_zone_2018)$pitcher)
colnames(out_of_zone_pitchers_2018) <- "OOZ"

in_zone_pitchers_2018 <- data.frame(ranef(model_zone_2018)$pitcher)
colnames(in_zone_pitchers_2018) <- "IZ"

all_results_2018 <- merge(in_zone_pitchers_2018,out_of_zone_pitchers_2018,by = "row.names")
colnames(all_results_2018) <- c("pitcher","IZ","OOZ")
all_results_2018$pitcher <- as.numeric(all_results_2018$pitcher)
all_results_2018$Season <- 2018

in_zone_data_2018 <- in_zone_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_2018$Season <- 2018

out_of_zone_data_2018 <- out_of_zone_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_2018$Season <- 2018

####2017 PD ----
all_data_2017 <- all_data %>% filter(Season == 2017)
in_zone_2017 <- all_data_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_2017 <- all_data_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                        data = in_zone_2017)

model_out_of_zone_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = out_of_zone_2017)

out_of_zone_pitchers_2017 <- data.frame(ranef(model_out_of_zone_2017)$pitcher)
colnames(out_of_zone_pitchers_2017) <- "OOZ"

in_zone_pitchers_2017 <- data.frame(ranef(model_zone_2017)$pitcher)
colnames(in_zone_pitchers_2017) <- "IZ"

all_results_2017 <- merge(in_zone_pitchers_2017,out_of_zone_pitchers_2017,by = "row.names")
colnames(all_results_2017) <- c("pitcher","IZ","OOZ")
all_results_2017$pitcher <- as.numeric(all_results_2017$pitcher)
all_results_2017$Season <- 2017

in_zone_data_2017 <- in_zone_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_2017$Season <- 2017

out_of_zone_data_2017 <- out_of_zone_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_2017$Season <- 2017

####2016 PD ----
all_data_2016 <- all_data %>% filter(Season == 2016)
in_zone_2016 <- all_data_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_2016 <- all_data_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                        data = in_zone_2016)

model_out_of_zone_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = out_of_zone_2016)

out_of_zone_pitchers_2016 <- data.frame(ranef(model_out_of_zone_2016)$pitcher)
colnames(out_of_zone_pitchers_2016) <- "OOZ"

in_zone_pitchers_2016 <- data.frame(ranef(model_zone_2016)$pitcher)
colnames(in_zone_pitchers_2016) <- "IZ"

all_results_2016 <- merge(in_zone_pitchers_2016,out_of_zone_pitchers_2016,by = "row.names")
colnames(all_results_2016) <- c("pitcher","IZ","OOZ")
all_results_2016$pitcher <- as.numeric(all_results_2016$pitcher)
all_results_2016$Season <- 2016

in_zone_data_2016 <- in_zone_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_2016$Season <- 2016

out_of_zone_data_2016 <- out_of_zone_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_2016$Season <- 2016

####2015 PD ----
all_data_2015 <- all_data %>% filter(Season == 2015)
in_zone_2015 <- all_data_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_2015 <- all_data_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                        data = in_zone_2015)

model_out_of_zone_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = out_of_zone_2015)

out_of_zone_pitchers_2015 <- data.frame(ranef(model_out_of_zone_2015)$pitcher)
colnames(out_of_zone_pitchers_2015) <- "OOZ"

in_zone_pitchers_2015 <- data.frame(ranef(model_zone_2015)$pitcher)
colnames(in_zone_pitchers_2015) <- "IZ"

all_results_2015 <- merge(in_zone_pitchers_2015,out_of_zone_pitchers_2015,by = "row.names")
colnames(all_results_2015) <- c("pitcher","IZ","OOZ")
all_results_2015$pitcher <- as.numeric(all_results_2015$pitcher)
all_results_2015$Season <- 2015

in_zone_data_2015 <- in_zone_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_2015$Season <- 2015

out_of_zone_data_2015 <- out_of_zone_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_2015$Season <- 2015

### All PD ----
out_of_zone_data_all <- rbind(out_of_zone_data_2015,out_of_zone_data_2016,out_of_zone_data_2017,out_of_zone_data_2018,out_of_zone_data_2019)
in_zone_data_all <- rbind(in_zone_data_2015,in_zone_data_2016,in_zone_data_2017,in_zone_data_2018,in_zone_data_2019)
all_results_all <- rbind(all_results_2015,all_results_2016,all_results_2017,all_results_2018,all_results_2019)

plate_discipline_all <- merge(all_whiff_seasons,out_of_zone_data_all,by = c("pitcher","Season"))
plate_discipline_all <- merge(plate_discipline_all,in_zone_data_all,by = c("pitcher","Season"))
plate_discipline_all <- merge(plate_discipline_all,all_results_all,by = c("pitcher","Season"))

plate_discipline_all <- plate_discipline_all[,c("pitcher",
                                        "player_name",
                                        "Season",
                                        "Pitches",
                                        "Whiff",
                                        "xWhiff",
                                        "In_Whiff",
                                        "OOZ.Pitches",
                                        "OOZ.Swing",
                                        "OOZ.xSwing",
                                        "OOZ",
                                        "IZ.Pitches",
                                        "IZ.Swing",
                                        "IZ.xSwing",
                                        "IZ")]
plate_discipline_all$Score <- round(plate_discipline_all$OOZ - plate_discipline_all$IZ + plate_discipline_all$In_Whiff,3)

whiff_league <- mean(whiff_predict_data$predict_whiff)
ooz_all <- all_data %>% 
              filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)
ooz_league <- mean(ooz_all$predict)
iz_all <- all_data %>% 
  filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)
iz_league <- mean(iz_all$predict)

plate_discipline_all$command <- round((plate_discipline_all$xWhiff/whiff_league) +
                                (plate_discipline_all$OOZ.xSwing/ooz_league) +
                                (1+((iz_league-plate_discipline_all$IZ.xSwing)/iz_league)),3)

write.csv(plate_discipline_all,"plate_discipline_all.csv")

all_pitches_trim <- mutate(all_pitches_trim, called_strike=ifelse(description %in% c("called_strike"),1, 0))

called_strikes <- all_pitches_trim %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
    CS.Rate = round(mean(called_strike),3))

called_strikes_season <- all_pitches_trim %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            CS = sum(called_strike),
            CS.Rate = round(mean(called_strike),3))

### 2019 SL----
all_slider_2019 <- all_data_2019 %>% filter(pitch_type == "SL")

in_zone_slider_2019 <- all_slider_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_slider_2019 <- all_slider_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_slider_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                        data = in_zone_slider_2019)

model_out_of_zone_slider_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = out_of_zone_slider_2019)

out_of_zone_pitchers_slider_2019 <- data.frame(ranef(model_out_of_zone_slider_2019)$pitcher)
colnames(out_of_zone_pitchers_slider_2019) <- "OOZ"

in_zone_pitchers_slider_2019 <- data.frame(ranef(model_zone_slider_2019)$pitcher)
colnames(in_zone_pitchers_slider_2019) <- "IZ"

all_results_slider_2019 <- merge(in_zone_pitchers_slider_2019,out_of_zone_pitchers_slider_2019,by = "row.names")
colnames(all_results_slider_2019) <- c("pitcher","IZ","OOZ")
all_results_slider_2019$pitcher <- as.numeric(all_results_slider_2019$pitcher)
all_results_slider_2019$Season <- 2019

in_zone_data_slider_2019 <- in_zone_slider_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_slider_2019$Season <- 2019

out_of_zone_data_slider_2019 <- out_of_zone_slider_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_slider_2019$Season <- 2019

whiff_predict_data_slider_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "SL")
model_whiff_slider_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                         data = whiff_predict_data_slider_2019)

whiff_pitchers_slider_2019 <- data.frame(ranef(model_whiff_slider_2019)$pitcher)
colnames(whiff_pitchers_slider_2019) <- "In_Whiff"

whiff_data_slider_2019 <- whiff_predict_data_slider_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_slider_2019 <- merge(whiff_data_slider_2019,whiff_pitchers_slider_2019, by.x = "pitcher", by.y = "row.names")

### 2018 SL----
all_slider_2018 <- all_data_2018 %>% filter(pitch_type == "SL")

in_zone_slider_2018 <- all_slider_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_slider_2018 <- all_slider_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_slider_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_slider_2018)

model_out_of_zone_slider_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_slider_2018)

out_of_zone_pitchers_slider_2018 <- data.frame(ranef(model_out_of_zone_slider_2018)$pitcher)
colnames(out_of_zone_pitchers_slider_2018) <- "OOZ"

in_zone_pitchers_slider_2018 <- data.frame(ranef(model_zone_slider_2018)$pitcher)
colnames(in_zone_pitchers_slider_2018) <- "IZ"

all_results_slider_2018 <- merge(in_zone_pitchers_slider_2018,out_of_zone_pitchers_slider_2018,by = "row.names")
colnames(all_results_slider_2018) <- c("pitcher","IZ","OOZ")
all_results_slider_2018$pitcher <- as.numeric(all_results_slider_2018$pitcher)
all_results_slider_2018$Season <- 2018

in_zone_data_slider_2018 <- in_zone_slider_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_slider_2018$Season <- 2018

out_of_zone_data_slider_2018 <- out_of_zone_slider_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_slider_2018$Season <- 2018

whiff_predict_data_slider_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "SL")
model_whiff_slider_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_slider_2018)

whiff_pitchers_slider_2018 <- data.frame(ranef(model_whiff_slider_2018)$pitcher)
colnames(whiff_pitchers_slider_2018) <- "In_Whiff"

whiff_data_slider_2018 <- whiff_predict_data_slider_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_slider_2018 <- merge(whiff_data_slider_2018,whiff_pitchers_slider_2018, by.x = "pitcher", by.y = "row.names")

### 2017 SL----
all_slider_2017 <- all_data_2017 %>% filter(pitch_type == "SL")

in_zone_slider_2017 <- all_slider_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_slider_2017 <- all_slider_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_slider_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_slider_2017)

model_out_of_zone_slider_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_slider_2017)

out_of_zone_pitchers_slider_2017 <- data.frame(ranef(model_out_of_zone_slider_2017)$pitcher)
colnames(out_of_zone_pitchers_slider_2017) <- "OOZ"

in_zone_pitchers_slider_2017 <- data.frame(ranef(model_zone_slider_2017)$pitcher)
colnames(in_zone_pitchers_slider_2017) <- "IZ"

all_results_slider_2017 <- merge(in_zone_pitchers_slider_2017,out_of_zone_pitchers_slider_2017,by = "row.names")
colnames(all_results_slider_2017) <- c("pitcher","IZ","OOZ")
all_results_slider_2017$pitcher <- as.numeric(all_results_slider_2017$pitcher)
all_results_slider_2017$Season <- 2017

in_zone_data_slider_2017 <- in_zone_slider_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_slider_2017$Season <- 2017

out_of_zone_data_slider_2017 <- out_of_zone_slider_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_slider_2017$Season <- 2017

whiff_predict_data_slider_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "SL")
model_whiff_slider_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_slider_2017)

whiff_pitchers_slider_2017 <- data.frame(ranef(model_whiff_slider_2017)$pitcher)
colnames(whiff_pitchers_slider_2017) <- "In_Whiff"

whiff_data_slider_2017 <- whiff_predict_data_slider_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_slider_2017 <- merge(whiff_data_slider_2017,whiff_pitchers_slider_2017, by.x = "pitcher", by.y = "row.names")

### 2016 SL----
all_slider_2016 <- all_data_2016 %>% filter(pitch_type == "SL")

in_zone_slider_2016 <- all_slider_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_slider_2016 <- all_slider_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_slider_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_slider_2016)

model_out_of_zone_slider_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_slider_2016)

out_of_zone_pitchers_slider_2016 <- data.frame(ranef(model_out_of_zone_slider_2016)$pitcher)
colnames(out_of_zone_pitchers_slider_2016) <- "OOZ"

in_zone_pitchers_slider_2016 <- data.frame(ranef(model_zone_slider_2016)$pitcher)
colnames(in_zone_pitchers_slider_2016) <- "IZ"

all_results_slider_2016 <- merge(in_zone_pitchers_slider_2016,out_of_zone_pitchers_slider_2016,by = "row.names")
colnames(all_results_slider_2016) <- c("pitcher","IZ","OOZ")
all_results_slider_2016$pitcher <- as.numeric(all_results_slider_2016$pitcher)
all_results_slider_2016$Season <- 2016

in_zone_data_slider_2016 <- in_zone_slider_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_slider_2016$Season <- 2016

out_of_zone_data_slider_2016 <- out_of_zone_slider_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_slider_2016$Season <- 2016

whiff_predict_data_slider_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "SL")
model_whiff_slider_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_slider_2016)

whiff_pitchers_slider_2016 <- data.frame(ranef(model_whiff_slider_2016)$pitcher)
colnames(whiff_pitchers_slider_2016) <- "In_Whiff"

whiff_data_slider_2016 <- whiff_predict_data_slider_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_slider_2016 <- merge(whiff_data_slider_2016,whiff_pitchers_slider_2016, by.x = "pitcher", by.y = "row.names")

### 2015 SL----
all_slider_2015 <- all_data_2015 %>% filter(pitch_type == "SL")

in_zone_slider_2015 <- all_slider_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_slider_2015 <- all_slider_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_slider_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_slider_2015)

model_out_of_zone_slider_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_slider_2015)

out_of_zone_pitchers_slider_2015 <- data.frame(ranef(model_out_of_zone_slider_2015)$pitcher)
colnames(out_of_zone_pitchers_slider_2015) <- "OOZ"

in_zone_pitchers_slider_2015 <- data.frame(ranef(model_zone_slider_2015)$pitcher)
colnames(in_zone_pitchers_slider_2015) <- "IZ"

all_results_slider_2015 <- merge(in_zone_pitchers_slider_2015,out_of_zone_pitchers_slider_2015,by = "row.names")
colnames(all_results_slider_2015) <- c("pitcher","IZ","OOZ")
all_results_slider_2015$pitcher <- as.numeric(all_results_slider_2015$pitcher)
all_results_slider_2015$Season <- 2015

in_zone_data_slider_2015 <- in_zone_slider_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_slider_2015$Season <- 2015

out_of_zone_data_slider_2015 <- out_of_zone_slider_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_slider_2015$Season <- 2015

whiff_predict_data_slider_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "SL")
model_whiff_slider_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_slider_2015)

whiff_pitchers_slider_2015 <- data.frame(ranef(model_whiff_slider_2015)$pitcher)
colnames(whiff_pitchers_slider_2015) <- "In_Whiff"

whiff_data_slider_2015 <- whiff_predict_data_slider_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_slider_2015 <- merge(whiff_data_slider_2015,whiff_pitchers_slider_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_slider_all <- rbind(out_of_zone_data_slider_2015,out_of_zone_data_slider_2016,out_of_zone_data_slider_2017,out_of_zone_data_slider_2018,out_of_zone_data_2019)
in_zone_data_slider_all <- rbind(in_zone_data_slider_2015,in_zone_data_slider_2016,in_zone_data_slider_2017,in_zone_data_slider_2018,in_zone_data_slider_2019)
all_results_slider_all <- rbind(all_results_slider_2015,all_results_slider_2016,all_results_slider_2017,all_results_slider_2018,all_results_slider_2019)
all_whiffs_slider_all <- rbind(whiff_data_slider_2015,whiff_data_slider_2016,whiff_data_slider_2017,whiff_data_slider_2018,whiff_data_slider_2019)

plate_discipline_slider_all <- merge(all_whiffs_slider_all,out_of_zone_data_slider_all,by = c("pitcher","Season"))
plate_discipline_slider_all <- merge(plate_discipline_slider_all,in_zone_data_slider_all,by = c("pitcher","Season"))
plate_discipline_slider_all <- merge(plate_discipline_slider_all,all_results_slider_all,by = c("pitcher","Season"))

plate_discipline_slider_all <- plate_discipline_slider_all[,c("pitcher",
                                                "player_name",
                                                "Season",
                                                "Pitches",
                                                "Whiff",
                                                "xWhiff",
                                                "In_Whiff",
                                                "OOZ.Pitches",
                                                "OOZ.Swing",
                                                "OOZ.xSwing",
                                                "OOZ",
                                                "IZ.Pitches",
                                                "IZ.Swing",
                                                "IZ.xSwing",
                                                "IZ")]
plate_discipline_slider_all$Score <- plate_discipline_slider_all$OOZ - plate_discipline_slider_all$IZ + plate_discipline_slider_all$In_Whiff

### 2019 CU----
all_curve_2019 <- all_data_2019 %>% filter(pitch_type == "CU")

in_zone_curve_2019 <- all_curve_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_curve_2019 <- all_curve_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_curve_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_curve_2019)

model_out_of_zone_curve_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_curve_2019)

out_of_zone_pitchers_curve_2019 <- data.frame(ranef(model_out_of_zone_curve_2019)$pitcher)
colnames(out_of_zone_pitchers_curve_2019) <- "OOZ"

in_zone_pitchers_curve_2019 <- data.frame(ranef(model_zone_curve_2019)$pitcher)
colnames(in_zone_pitchers_curve_2019) <- "IZ"

all_results_curve_2019 <- merge(in_zone_pitchers_curve_2019,out_of_zone_pitchers_curve_2019,by = "row.names")
colnames(all_results_curve_2019) <- c("pitcher","IZ","OOZ")
all_results_curve_2019$pitcher <- as.numeric(all_results_curve_2019$pitcher)
all_results_curve_2019$Season <- 2019

in_zone_data_curve_2019 <- in_zone_curve_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_curve_2019$Season <- 2019

out_of_zone_data_curve_2019 <- out_of_zone_curve_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_curve_2019$Season <- 2019

whiff_predict_data_curve_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "CU")
model_whiff_curve_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_curve_2019)

whiff_pitchers_curve_2019 <- data.frame(ranef(model_whiff_curve_2019)$pitcher)
colnames(whiff_pitchers_curve_2019) <- "In_Whiff"

whiff_data_curve_2019 <- whiff_predict_data_curve_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_curve_2019 <- merge(whiff_data_curve_2019,whiff_pitchers_curve_2019, by.x = "pitcher", by.y = "row.names")

### 2018 CU----
all_curve_2018 <- all_data_2018 %>% filter(pitch_type == "CU")

in_zone_curve_2018 <- all_curve_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_curve_2018 <- all_curve_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_curve_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_curve_2018)

model_out_of_zone_curve_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_curve_2018)

out_of_zone_pitchers_curve_2018 <- data.frame(ranef(model_out_of_zone_curve_2018)$pitcher)
colnames(out_of_zone_pitchers_curve_2018) <- "OOZ"

in_zone_pitchers_curve_2018 <- data.frame(ranef(model_zone_curve_2018)$pitcher)
colnames(in_zone_pitchers_curve_2018) <- "IZ"

all_results_curve_2018 <- merge(in_zone_pitchers_curve_2018,out_of_zone_pitchers_curve_2018,by = "row.names")
colnames(all_results_curve_2018) <- c("pitcher","IZ","OOZ")
all_results_curve_2018$pitcher <- as.numeric(all_results_curve_2018$pitcher)
all_results_curve_2018$Season <- 2018

in_zone_data_curve_2018 <- in_zone_curve_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_curve_2018$Season <- 2018

out_of_zone_data_curve_2018 <- out_of_zone_curve_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_curve_2018$Season <- 2018

whiff_predict_data_curve_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "CU")
model_whiff_curve_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_curve_2018)

whiff_pitchers_curve_2018 <- data.frame(ranef(model_whiff_curve_2018)$pitcher)
colnames(whiff_pitchers_curve_2018) <- "In_Whiff"

whiff_data_curve_2018 <- whiff_predict_data_curve_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_curve_2018 <- merge(whiff_data_curve_2018,whiff_pitchers_curve_2018, by.x = "pitcher", by.y = "row.names")

### 2017 CU----
all_curve_2017 <- all_data_2017 %>% filter(pitch_type == "CU")

in_zone_curve_2017 <- all_curve_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_curve_2017 <- all_curve_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_curve_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_curve_2017)

model_out_of_zone_curve_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_curve_2017)

out_of_zone_pitchers_curve_2017 <- data.frame(ranef(model_out_of_zone_curve_2017)$pitcher)
colnames(out_of_zone_pitchers_curve_2017) <- "OOZ"

in_zone_pitchers_curve_2017 <- data.frame(ranef(model_zone_curve_2017)$pitcher)
colnames(in_zone_pitchers_curve_2017) <- "IZ"

all_results_curve_2017 <- merge(in_zone_pitchers_curve_2017,out_of_zone_pitchers_curve_2017,by = "row.names")
colnames(all_results_curve_2017) <- c("pitcher","IZ","OOZ")
all_results_curve_2017$pitcher <- as.numeric(all_results_curve_2017$pitcher)
all_results_curve_2017$Season <- 2017

in_zone_data_curve_2017 <- in_zone_curve_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_curve_2017$Season <- 2017

out_of_zone_data_curve_2017 <- out_of_zone_curve_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_curve_2017$Season <- 2017

whiff_predict_data_curve_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "CU")
model_whiff_curve_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_curve_2017)

whiff_pitchers_curve_2017 <- data.frame(ranef(model_whiff_curve_2017)$pitcher)
colnames(whiff_pitchers_curve_2017) <- "In_Whiff"

whiff_data_curve_2017 <- whiff_predict_data_curve_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_curve_2017 <- merge(whiff_data_curve_2017,whiff_pitchers_curve_2017, by.x = "pitcher", by.y = "row.names")

### 2016 CU----
all_curve_2016 <- all_data_2016 %>% filter(pitch_type == "CU")

in_zone_curve_2016 <- all_curve_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_curve_2016 <- all_curve_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_curve_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_curve_2016)

model_out_of_zone_curve_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_curve_2016)

out_of_zone_pitchers_curve_2016 <- data.frame(ranef(model_out_of_zone_curve_2016)$pitcher)
colnames(out_of_zone_pitchers_curve_2016) <- "OOZ"

in_zone_pitchers_curve_2016 <- data.frame(ranef(model_zone_curve_2016)$pitcher)
colnames(in_zone_pitchers_curve_2016) <- "IZ"

all_results_curve_2016 <- merge(in_zone_pitchers_curve_2016,out_of_zone_pitchers_curve_2016,by = "row.names")
colnames(all_results_curve_2016) <- c("pitcher","IZ","OOZ")
all_results_curve_2016$pitcher <- as.numeric(all_results_curve_2016$pitcher)
all_results_curve_2016$Season <- 2016

in_zone_data_curve_2016 <- in_zone_curve_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_curve_2016$Season <- 2016

out_of_zone_data_curve_2016 <- out_of_zone_curve_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_curve_2016$Season <- 2016

whiff_predict_data_curve_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "CU")
model_whiff_curve_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_curve_2016)

whiff_pitchers_curve_2016 <- data.frame(ranef(model_whiff_curve_2016)$pitcher)
colnames(whiff_pitchers_curve_2016) <- "In_Whiff"

whiff_data_curve_2016 <- whiff_predict_data_curve_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_curve_2016 <- merge(whiff_data_curve_2016,whiff_pitchers_curve_2016, by.x = "pitcher", by.y = "row.names")

### 2015 CU----
all_curve_2015 <- all_data_2015 %>% filter(pitch_type == "CU")

in_zone_curve_2015 <- all_curve_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_curve_2015 <- all_curve_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_curve_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_curve_2015)

model_out_of_zone_curve_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_curve_2015)

out_of_zone_pitchers_curve_2015 <- data.frame(ranef(model_out_of_zone_curve_2015)$pitcher)
colnames(out_of_zone_pitchers_curve_2015) <- "OOZ"

in_zone_pitchers_curve_2015 <- data.frame(ranef(model_zone_curve_2015)$pitcher)
colnames(in_zone_pitchers_curve_2015) <- "IZ"

all_results_curve_2015 <- merge(in_zone_pitchers_curve_2015,out_of_zone_pitchers_curve_2015,by = "row.names")
colnames(all_results_curve_2015) <- c("pitcher","IZ","OOZ")
all_results_curve_2015$pitcher <- as.numeric(all_results_curve_2015$pitcher)
all_results_curve_2015$Season <- 2015

in_zone_data_curve_2015 <- in_zone_curve_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_curve_2015$Season <- 2015

out_of_zone_data_curve_2015 <- out_of_zone_curve_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_curve_2015$Season <- 2015

whiff_predict_data_curve_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "CU")
model_whiff_curve_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_curve_2015)

whiff_pitchers_curve_2015 <- data.frame(ranef(model_whiff_curve_2015)$pitcher)
colnames(whiff_pitchers_curve_2015) <- "In_Whiff"

whiff_data_curve_2015 <- whiff_predict_data_curve_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_curve_2015 <- merge(whiff_data_curve_2015,whiff_pitchers_curve_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_curve_all <- rbind(out_of_zone_data_curve_2015,out_of_zone_data_curve_2016,out_of_zone_data_curve_2017,out_of_zone_data_curve_2018,out_of_zone_data_2019)
in_zone_data_curve_all <- rbind(in_zone_data_curve_2015,in_zone_data_curve_2016,in_zone_data_curve_2017,in_zone_data_curve_2018,in_zone_data_curve_2019)
all_results_curve_all <- rbind(all_results_curve_2015,all_results_curve_2016,all_results_curve_2017,all_results_curve_2018,all_results_curve_2019)
all_whiffs_curve_all <- rbind(whiff_data_curve_2015,whiff_data_curve_2016,whiff_data_curve_2017,whiff_data_curve_2018,whiff_data_curve_2019)

plate_discipline_curve_all <- merge(all_whiffs_curve_all,out_of_zone_data_curve_all,by = c("pitcher","Season"))
plate_discipline_curve_all <- merge(plate_discipline_curve_all,in_zone_data_curve_all,by = c("pitcher","Season"))
plate_discipline_curve_all <- merge(plate_discipline_curve_all,all_results_curve_all,by = c("pitcher","Season"))

plate_discipline_curve_all <- plate_discipline_curve_all[,c("pitcher",
                                                              "player_name",
                                                              "Season",
                                                              "Pitches",
                                                              "Whiff",
                                                              "xWhiff",
                                                              "In_Whiff",
                                                              "OOZ.Pitches",
                                                              "OOZ.Swing",
                                                              "OOZ.xSwing",
                                                              "OOZ",
                                                              "IZ.Pitches",
                                                              "IZ.Swing",
                                                              "IZ.xSwing",
                                                              "IZ")]
plate_discipline_curve_all$Score <- plate_discipline_curve_all$OOZ - plate_discipline_curve_all$IZ + plate_discipline_curve_all$In_Whiff

### 2019 CH----
all_change_2019 <- all_data_2019 %>% filter(pitch_type == "CH")

in_zone_change_2019 <- all_change_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_change_2019 <- all_change_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_change_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                              data = in_zone_change_2019)

model_out_of_zone_change_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                     data = out_of_zone_change_2019)

out_of_zone_pitchers_change_2019 <- data.frame(ranef(model_out_of_zone_change_2019)$pitcher)
colnames(out_of_zone_pitchers_change_2019) <- "OOZ"

in_zone_pitchers_change_2019 <- data.frame(ranef(model_zone_change_2019)$pitcher)
colnames(in_zone_pitchers_change_2019) <- "IZ"

all_results_change_2019 <- merge(in_zone_pitchers_change_2019,out_of_zone_pitchers_change_2019,by = "row.names")
colnames(all_results_change_2019) <- c("pitcher","IZ","OOZ")
all_results_change_2019$pitcher <- as.numeric(all_results_change_2019$pitcher)
all_results_change_2019$Season <- 2019

in_zone_data_change_2019 <- in_zone_change_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_change_2019$Season <- 2019

out_of_zone_data_change_2019 <- out_of_zone_change_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_change_2019$Season <- 2019

whiff_predict_data_change_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "CH")
model_whiff_change_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                               data = whiff_predict_data_change_2019)

whiff_pitchers_change_2019 <- data.frame(ranef(model_whiff_change_2019)$pitcher)
colnames(whiff_pitchers_change_2019) <- "In_Whiff"

whiff_data_change_2019 <- whiff_predict_data_change_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_change_2019 <- merge(whiff_data_change_2019,whiff_pitchers_change_2019, by.x = "pitcher", by.y = "row.names")

### 2018 CH----
all_change_2018 <- all_data_2018 %>% filter(pitch_type == "CH")

in_zone_change_2018 <- all_change_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_change_2018 <- all_change_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_change_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                              data = in_zone_change_2018)

model_out_of_zone_change_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                     data = out_of_zone_change_2018)

out_of_zone_pitchers_change_2018 <- data.frame(ranef(model_out_of_zone_change_2018)$pitcher)
colnames(out_of_zone_pitchers_change_2018) <- "OOZ"

in_zone_pitchers_change_2018 <- data.frame(ranef(model_zone_change_2018)$pitcher)
colnames(in_zone_pitchers_change_2018) <- "IZ"

all_results_change_2018 <- merge(in_zone_pitchers_change_2018,out_of_zone_pitchers_change_2018,by = "row.names")
colnames(all_results_change_2018) <- c("pitcher","IZ","OOZ")
all_results_change_2018$pitcher <- as.numeric(all_results_change_2018$pitcher)
all_results_change_2018$Season <- 2018

in_zone_data_change_2018 <- in_zone_change_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_change_2018$Season <- 2018

out_of_zone_data_change_2018 <- out_of_zone_change_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_change_2018$Season <- 2018

whiff_predict_data_change_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "CH")
model_whiff_change_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                               data = whiff_predict_data_change_2018)

whiff_pitchers_change_2018 <- data.frame(ranef(model_whiff_change_2018)$pitcher)
colnames(whiff_pitchers_change_2018) <- "In_Whiff"

whiff_data_change_2018 <- whiff_predict_data_change_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_change_2018 <- merge(whiff_data_change_2018,whiff_pitchers_change_2018, by.x = "pitcher", by.y = "row.names")

### 2017 CH----
all_change_2017 <- all_data_2017 %>% filter(pitch_type == "CH")

in_zone_change_2017 <- all_change_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_change_2017 <- all_change_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_change_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                              data = in_zone_change_2017)

model_out_of_zone_change_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                     data = out_of_zone_change_2017)

out_of_zone_pitchers_change_2017 <- data.frame(ranef(model_out_of_zone_change_2017)$pitcher)
colnames(out_of_zone_pitchers_change_2017) <- "OOZ"

in_zone_pitchers_change_2017 <- data.frame(ranef(model_zone_change_2017)$pitcher)
colnames(in_zone_pitchers_change_2017) <- "IZ"

all_results_change_2017 <- merge(in_zone_pitchers_change_2017,out_of_zone_pitchers_change_2017,by = "row.names")
colnames(all_results_change_2017) <- c("pitcher","IZ","OOZ")
all_results_change_2017$pitcher <- as.numeric(all_results_change_2017$pitcher)
all_results_change_2017$Season <- 2017

in_zone_data_change_2017 <- in_zone_change_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_change_2017$Season <- 2017

out_of_zone_data_change_2017 <- out_of_zone_change_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_change_2017$Season <- 2017

whiff_predict_data_change_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "CH")
model_whiff_change_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                               data = whiff_predict_data_change_2017)

whiff_pitchers_change_2017 <- data.frame(ranef(model_whiff_change_2017)$pitcher)
colnames(whiff_pitchers_change_2017) <- "In_Whiff"

whiff_data_change_2017 <- whiff_predict_data_change_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_change_2017 <- merge(whiff_data_change_2017,whiff_pitchers_change_2017, by.x = "pitcher", by.y = "row.names")

### 2016 CH----
all_change_2016 <- all_data_2016 %>% filter(pitch_type == "CH")

in_zone_change_2016 <- all_change_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_change_2016 <- all_change_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_change_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                              data = in_zone_change_2016)

model_out_of_zone_change_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                     data = out_of_zone_change_2016)

out_of_zone_pitchers_change_2016 <- data.frame(ranef(model_out_of_zone_change_2016)$pitcher)
colnames(out_of_zone_pitchers_change_2016) <- "OOZ"

in_zone_pitchers_change_2016 <- data.frame(ranef(model_zone_change_2016)$pitcher)
colnames(in_zone_pitchers_change_2016) <- "IZ"

all_results_change_2016 <- merge(in_zone_pitchers_change_2016,out_of_zone_pitchers_change_2016,by = "row.names")
colnames(all_results_change_2016) <- c("pitcher","IZ","OOZ")
all_results_change_2016$pitcher <- as.numeric(all_results_change_2016$pitcher)
all_results_change_2016$Season <- 2016

in_zone_data_change_2016 <- in_zone_change_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_change_2016$Season <- 2016

out_of_zone_data_change_2016 <- out_of_zone_change_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_change_2016$Season <- 2016

whiff_predict_data_change_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "CH")
model_whiff_change_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                               data = whiff_predict_data_change_2016)

whiff_pitchers_change_2016 <- data.frame(ranef(model_whiff_change_2016)$pitcher)
colnames(whiff_pitchers_change_2016) <- "In_Whiff"

whiff_data_change_2016 <- whiff_predict_data_change_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_change_2016 <- merge(whiff_data_change_2016,whiff_pitchers_change_2016, by.x = "pitcher", by.y = "row.names")

### 2015 CH----
all_change_2015 <- all_data_2015 %>% filter(pitch_type == "CH")

in_zone_change_2015 <- all_change_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_change_2015 <- all_change_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_change_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                              data = in_zone_change_2015)

model_out_of_zone_change_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                     data = out_of_zone_change_2015)

out_of_zone_pitchers_change_2015 <- data.frame(ranef(model_out_of_zone_change_2015)$pitcher)
colnames(out_of_zone_pitchers_change_2015) <- "OOZ"

in_zone_pitchers_change_2015 <- data.frame(ranef(model_zone_change_2015)$pitcher)
colnames(in_zone_pitchers_change_2015) <- "IZ"

all_results_change_2015 <- merge(in_zone_pitchers_change_2015,out_of_zone_pitchers_change_2015,by = "row.names")
colnames(all_results_change_2015) <- c("pitcher","IZ","OOZ")
all_results_change_2015$pitcher <- as.numeric(all_results_change_2015$pitcher)
all_results_change_2015$Season <- 2015

in_zone_data_change_2015 <- in_zone_change_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_change_2015$Season <- 2015

out_of_zone_data_change_2015 <- out_of_zone_change_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_change_2015$Season <- 2015

whiff_predict_data_change_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "CH")
model_whiff_change_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                               data = whiff_predict_data_change_2015)

whiff_pitchers_change_2015 <- data.frame(ranef(model_whiff_change_2015)$pitcher)
colnames(whiff_pitchers_change_2015) <- "In_Whiff"

whiff_data_change_2015 <- whiff_predict_data_change_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_change_2015 <- merge(whiff_data_change_2015,whiff_pitchers_change_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_change_all <- rbind(out_of_zone_data_change_2015,out_of_zone_data_change_2016,out_of_zone_data_change_2017,out_of_zone_data_change_2018,out_of_zone_data_2019)
in_zone_data_change_all <- rbind(in_zone_data_change_2015,in_zone_data_change_2016,in_zone_data_change_2017,in_zone_data_change_2018,in_zone_data_change_2019)
all_results_change_all <- rbind(all_results_change_2015,all_results_change_2016,all_results_change_2017,all_results_change_2018,all_results_change_2019)
all_whiffs_change_all <- rbind(whiff_data_change_2015,whiff_data_change_2016,whiff_data_change_2017,whiff_data_change_2018,whiff_data_change_2019)

plate_discipline_change_all <- merge(all_whiffs_change_all,out_of_zone_data_change_all,by = c("pitcher","Season"))
plate_discipline_change_all <- merge(plate_discipline_change_all,in_zone_data_change_all,by = c("pitcher","Season"))
plate_discipline_change_all <- merge(plate_discipline_change_all,all_results_change_all,by = c("pitcher","Season"))

plate_discipline_change_all <- plate_discipline_change_all[,c("pitcher",
                                                            "player_name",
                                                            "Season",
                                                            "Pitches",
                                                            "Whiff",
                                                            "xWhiff",
                                                            "In_Whiff",
                                                            "OOZ.Pitches",
                                                            "OOZ.Swing",
                                                            "OOZ.xSwing",
                                                            "OOZ",
                                                            "IZ.Pitches",
                                                            "IZ.Swing",
                                                            "IZ.xSwing",
                                                            "IZ")]
plate_discipline_change_all$Score <- plate_discipline_change_all$OOZ - plate_discipline_change_all$IZ + plate_discipline_change_all$In_Whiff

### 2019 FF----
all_fourseam_2019 <- all_data_2019 %>% filter(pitch_type == "FF")

in_zone_fourseam_2019 <- all_fourseam_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_fourseam_2019 <- all_fourseam_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_fourseam_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_fourseam_2019)

model_out_of_zone_fourseam_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_fourseam_2019)

out_of_zone_pitchers_fourseam_2019 <- data.frame(ranef(model_out_of_zone_fourseam_2019)$pitcher)
colnames(out_of_zone_pitchers_fourseam_2019) <- "OOZ"

in_zone_pitchers_fourseam_2019 <- data.frame(ranef(model_zone_fourseam_2019)$pitcher)
colnames(in_zone_pitchers_fourseam_2019) <- "IZ"

all_results_fourseam_2019 <- merge(in_zone_pitchers_fourseam_2019,out_of_zone_pitchers_fourseam_2019,by = "row.names")
colnames(all_results_fourseam_2019) <- c("pitcher","IZ","OOZ")
all_results_fourseam_2019$pitcher <- as.numeric(all_results_fourseam_2019$pitcher)
all_results_fourseam_2019$Season <- 2019

in_zone_data_fourseam_2019 <- in_zone_fourseam_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_fourseam_2019$Season <- 2019

out_of_zone_data_fourseam_2019 <- out_of_zone_fourseam_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_fourseam_2019$Season <- 2019

whiff_predict_data_fourseam_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "FF")
model_whiff_fourseam_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_fourseam_2019)

whiff_pitchers_fourseam_2019 <- data.frame(ranef(model_whiff_fourseam_2019)$pitcher)
colnames(whiff_pitchers_fourseam_2019) <- "In_Whiff"

whiff_data_fourseam_2019 <- whiff_predict_data_fourseam_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_fourseam_2019 <- merge(whiff_data_fourseam_2019,whiff_pitchers_fourseam_2019, by.x = "pitcher", by.y = "row.names")

### 2018 FF----
all_fourseam_2018 <- all_data_2018 %>% filter(pitch_type == "FF")

in_zone_fourseam_2018 <- all_fourseam_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_fourseam_2018 <- all_fourseam_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_fourseam_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_fourseam_2018)

model_out_of_zone_fourseam_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_fourseam_2018)

out_of_zone_pitchers_fourseam_2018 <- data.frame(ranef(model_out_of_zone_fourseam_2018)$pitcher)
colnames(out_of_zone_pitchers_fourseam_2018) <- "OOZ"

in_zone_pitchers_fourseam_2018 <- data.frame(ranef(model_zone_fourseam_2018)$pitcher)
colnames(in_zone_pitchers_fourseam_2018) <- "IZ"

all_results_fourseam_2018 <- merge(in_zone_pitchers_fourseam_2018,out_of_zone_pitchers_fourseam_2018,by = "row.names")
colnames(all_results_fourseam_2018) <- c("pitcher","IZ","OOZ")
all_results_fourseam_2018$pitcher <- as.numeric(all_results_fourseam_2018$pitcher)
all_results_fourseam_2018$Season <- 2018

in_zone_data_fourseam_2018 <- in_zone_fourseam_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_fourseam_2018$Season <- 2018

out_of_zone_data_fourseam_2018 <- out_of_zone_fourseam_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_fourseam_2018$Season <- 2018

whiff_predict_data_fourseam_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "FF")
model_whiff_fourseam_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_fourseam_2018)

whiff_pitchers_fourseam_2018 <- data.frame(ranef(model_whiff_fourseam_2018)$pitcher)
colnames(whiff_pitchers_fourseam_2018) <- "In_Whiff"

whiff_data_fourseam_2018 <- whiff_predict_data_fourseam_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_fourseam_2018 <- merge(whiff_data_fourseam_2018,whiff_pitchers_fourseam_2018, by.x = "pitcher", by.y = "row.names")

### 2017 FF----
all_fourseam_2017 <- all_data_2017 %>% filter(pitch_type == "FF")

in_zone_fourseam_2017 <- all_fourseam_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_fourseam_2017 <- all_fourseam_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_fourseam_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_fourseam_2017)

model_out_of_zone_fourseam_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_fourseam_2017)

out_of_zone_pitchers_fourseam_2017 <- data.frame(ranef(model_out_of_zone_fourseam_2017)$pitcher)
colnames(out_of_zone_pitchers_fourseam_2017) <- "OOZ"

in_zone_pitchers_fourseam_2017 <- data.frame(ranef(model_zone_fourseam_2017)$pitcher)
colnames(in_zone_pitchers_fourseam_2017) <- "IZ"

all_results_fourseam_2017 <- merge(in_zone_pitchers_fourseam_2017,out_of_zone_pitchers_fourseam_2017,by = "row.names")
colnames(all_results_fourseam_2017) <- c("pitcher","IZ","OOZ")
all_results_fourseam_2017$pitcher <- as.numeric(all_results_fourseam_2017$pitcher)
all_results_fourseam_2017$Season <- 2017

in_zone_data_fourseam_2017 <- in_zone_fourseam_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_fourseam_2017$Season <- 2017

out_of_zone_data_fourseam_2017 <- out_of_zone_fourseam_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_fourseam_2017$Season <- 2017

whiff_predict_data_fourseam_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "FF")
model_whiff_fourseam_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_fourseam_2017)

whiff_pitchers_fourseam_2017 <- data.frame(ranef(model_whiff_fourseam_2017)$pitcher)
colnames(whiff_pitchers_fourseam_2017) <- "In_Whiff"

whiff_data_fourseam_2017 <- whiff_predict_data_fourseam_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_fourseam_2017 <- merge(whiff_data_fourseam_2017,whiff_pitchers_fourseam_2017, by.x = "pitcher", by.y = "row.names")

### 2016 FF----
all_fourseam_2016 <- all_data_2016 %>% filter(pitch_type == "FF")

in_zone_fourseam_2016 <- all_fourseam_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_fourseam_2016 <- all_fourseam_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_fourseam_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_fourseam_2016)

model_out_of_zone_fourseam_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_fourseam_2016)

out_of_zone_pitchers_fourseam_2016 <- data.frame(ranef(model_out_of_zone_fourseam_2016)$pitcher)
colnames(out_of_zone_pitchers_fourseam_2016) <- "OOZ"

in_zone_pitchers_fourseam_2016 <- data.frame(ranef(model_zone_fourseam_2016)$pitcher)
colnames(in_zone_pitchers_fourseam_2016) <- "IZ"

all_results_fourseam_2016 <- merge(in_zone_pitchers_fourseam_2016,out_of_zone_pitchers_fourseam_2016,by = "row.names")
colnames(all_results_fourseam_2016) <- c("pitcher","IZ","OOZ")
all_results_fourseam_2016$pitcher <- as.numeric(all_results_fourseam_2016$pitcher)
all_results_fourseam_2016$Season <- 2016

in_zone_data_fourseam_2016 <- in_zone_fourseam_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_fourseam_2016$Season <- 2016

out_of_zone_data_fourseam_2016 <- out_of_zone_fourseam_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_fourseam_2016$Season <- 2016

whiff_predict_data_fourseam_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "FF")
model_whiff_fourseam_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_fourseam_2016)

whiff_pitchers_fourseam_2016 <- data.frame(ranef(model_whiff_fourseam_2016)$pitcher)
colnames(whiff_pitchers_fourseam_2016) <- "In_Whiff"

whiff_data_fourseam_2016 <- whiff_predict_data_fourseam_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_fourseam_2016 <- merge(whiff_data_fourseam_2016,whiff_pitchers_fourseam_2016, by.x = "pitcher", by.y = "row.names")

### 2015 FF----
all_fourseam_2015 <- all_data_2015 %>% filter(pitch_type == "FF")

in_zone_fourseam_2015 <- all_fourseam_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_fourseam_2015 <- all_fourseam_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_fourseam_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_fourseam_2015)

model_out_of_zone_fourseam_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_fourseam_2015)

out_of_zone_pitchers_fourseam_2015 <- data.frame(ranef(model_out_of_zone_fourseam_2015)$pitcher)
colnames(out_of_zone_pitchers_fourseam_2015) <- "OOZ"

in_zone_pitchers_fourseam_2015 <- data.frame(ranef(model_zone_fourseam_2015)$pitcher)
colnames(in_zone_pitchers_fourseam_2015) <- "IZ"

all_results_fourseam_2015 <- merge(in_zone_pitchers_fourseam_2015,out_of_zone_pitchers_fourseam_2015,by = "row.names")
colnames(all_results_fourseam_2015) <- c("pitcher","IZ","OOZ")
all_results_fourseam_2015$pitcher <- as.numeric(all_results_fourseam_2015$pitcher)
all_results_fourseam_2015$Season <- 2015

in_zone_data_fourseam_2015 <- in_zone_fourseam_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_fourseam_2015$Season <- 2015

out_of_zone_data_fourseam_2015 <- out_of_zone_fourseam_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_fourseam_2015$Season <- 2015

whiff_predict_data_fourseam_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "FF")
model_whiff_fourseam_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_fourseam_2015)

whiff_pitchers_fourseam_2015 <- data.frame(ranef(model_whiff_fourseam_2015)$pitcher)
colnames(whiff_pitchers_fourseam_2015) <- "In_Whiff"

whiff_data_fourseam_2015 <- whiff_predict_data_fourseam_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_fourseam_2015 <- merge(whiff_data_fourseam_2015,whiff_pitchers_fourseam_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_fourseam_all <- rbind(out_of_zone_data_fourseam_2015,out_of_zone_data_fourseam_2016,out_of_zone_data_fourseam_2017,out_of_zone_data_fourseam_2018,out_of_zone_data_2019)
in_zone_data_fourseam_all <- rbind(in_zone_data_fourseam_2015,in_zone_data_fourseam_2016,in_zone_data_fourseam_2017,in_zone_data_fourseam_2018,in_zone_data_fourseam_2019)
all_results_fourseam_all <- rbind(all_results_fourseam_2015,all_results_fourseam_2016,all_results_fourseam_2017,all_results_fourseam_2018,all_results_fourseam_2019)
all_whiffs_fourseam_all <- rbind(whiff_data_fourseam_2015,whiff_data_fourseam_2016,whiff_data_fourseam_2017,whiff_data_fourseam_2018,whiff_data_fourseam_2019)

plate_discipline_fourseam_all <- merge(all_whiffs_fourseam_all,out_of_zone_data_fourseam_all,by = c("pitcher","Season"))
plate_discipline_fourseam_all <- merge(plate_discipline_fourseam_all,in_zone_data_fourseam_all,by = c("pitcher","Season"))
plate_discipline_fourseam_all <- merge(plate_discipline_fourseam_all,all_results_fourseam_all,by = c("pitcher","Season"))

plate_discipline_fourseam_all <- plate_discipline_fourseam_all[,c("pitcher",
                                                              "player_name",
                                                              "Season",
                                                              "Pitches",
                                                              "Whiff",
                                                              "xWhiff",
                                                              "In_Whiff",
                                                              "OOZ.Pitches",
                                                              "OOZ.Swing",
                                                              "OOZ.xSwing",
                                                              "OOZ",
                                                              "IZ.Pitches",
                                                              "IZ.Swing",
                                                              "IZ.xSwing",
                                                              "IZ")]
plate_discipline_fourseam_all$Score <- plate_discipline_fourseam_all$OOZ - plate_discipline_fourseam_all$IZ + plate_discipline_fourseam_all$In_Whiff

### 2019 FT----
all_twoseam_2019 <- all_data_2019 %>% filter(pitch_type == "FT")

in_zone_twoseam_2019 <- all_twoseam_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_twoseam_2019 <- all_twoseam_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_twoseam_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_twoseam_2019)

model_out_of_zone_twoseam_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_twoseam_2019)

out_of_zone_pitchers_twoseam_2019 <- data.frame(ranef(model_out_of_zone_twoseam_2019)$pitcher)
colnames(out_of_zone_pitchers_twoseam_2019) <- "OOZ"

in_zone_pitchers_twoseam_2019 <- data.frame(ranef(model_zone_twoseam_2019)$pitcher)
colnames(in_zone_pitchers_twoseam_2019) <- "IZ"

all_results_twoseam_2019 <- merge(in_zone_pitchers_twoseam_2019,out_of_zone_pitchers_twoseam_2019,by = "row.names")
colnames(all_results_twoseam_2019) <- c("pitcher","IZ","OOZ")
all_results_twoseam_2019$pitcher <- as.numeric(all_results_twoseam_2019$pitcher)
all_results_twoseam_2019$Season <- 2019

in_zone_data_twoseam_2019 <- in_zone_twoseam_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_twoseam_2019$Season <- 2019

out_of_zone_data_twoseam_2019 <- out_of_zone_twoseam_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_twoseam_2019$Season <- 2019

whiff_predict_data_twoseam_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "FT")
model_whiff_twoseam_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_twoseam_2019)

whiff_pitchers_twoseam_2019 <- data.frame(ranef(model_whiff_twoseam_2019)$pitcher)
colnames(whiff_pitchers_twoseam_2019) <- "In_Whiff"

whiff_data_twoseam_2019 <- whiff_predict_data_twoseam_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_twoseam_2019 <- merge(whiff_data_twoseam_2019,whiff_pitchers_twoseam_2019, by.x = "pitcher", by.y = "row.names")

### 2018 FT----
all_twoseam_2018 <- all_data_2018 %>% filter(pitch_type == "FT")

in_zone_twoseam_2018 <- all_twoseam_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_twoseam_2018 <- all_twoseam_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_twoseam_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_twoseam_2018)

model_out_of_zone_twoseam_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_twoseam_2018)

out_of_zone_pitchers_twoseam_2018 <- data.frame(ranef(model_out_of_zone_twoseam_2018)$pitcher)
colnames(out_of_zone_pitchers_twoseam_2018) <- "OOZ"

in_zone_pitchers_twoseam_2018 <- data.frame(ranef(model_zone_twoseam_2018)$pitcher)
colnames(in_zone_pitchers_twoseam_2018) <- "IZ"

all_results_twoseam_2018 <- merge(in_zone_pitchers_twoseam_2018,out_of_zone_pitchers_twoseam_2018,by = "row.names")
colnames(all_results_twoseam_2018) <- c("pitcher","IZ","OOZ")
all_results_twoseam_2018$pitcher <- as.numeric(all_results_twoseam_2018$pitcher)
all_results_twoseam_2018$Season <- 2018

in_zone_data_twoseam_2018 <- in_zone_twoseam_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_twoseam_2018$Season <- 2018

out_of_zone_data_twoseam_2018 <- out_of_zone_twoseam_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_twoseam_2018$Season <- 2018

whiff_predict_data_twoseam_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "FT")
model_whiff_twoseam_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_twoseam_2018)

whiff_pitchers_twoseam_2018 <- data.frame(ranef(model_whiff_twoseam_2018)$pitcher)
colnames(whiff_pitchers_twoseam_2018) <- "In_Whiff"

whiff_data_twoseam_2018 <- whiff_predict_data_twoseam_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_twoseam_2018 <- merge(whiff_data_twoseam_2018,whiff_pitchers_twoseam_2018, by.x = "pitcher", by.y = "row.names")

### 2017 FT----
all_twoseam_2017 <- all_data_2017 %>% filter(pitch_type == "FT")

in_zone_twoseam_2017 <- all_twoseam_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_twoseam_2017 <- all_twoseam_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_twoseam_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_twoseam_2017)

model_out_of_zone_twoseam_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_twoseam_2017)

out_of_zone_pitchers_twoseam_2017 <- data.frame(ranef(model_out_of_zone_twoseam_2017)$pitcher)
colnames(out_of_zone_pitchers_twoseam_2017) <- "OOZ"

in_zone_pitchers_twoseam_2017 <- data.frame(ranef(model_zone_twoseam_2017)$pitcher)
colnames(in_zone_pitchers_twoseam_2017) <- "IZ"

all_results_twoseam_2017 <- merge(in_zone_pitchers_twoseam_2017,out_of_zone_pitchers_twoseam_2017,by = "row.names")
colnames(all_results_twoseam_2017) <- c("pitcher","IZ","OOZ")
all_results_twoseam_2017$pitcher <- as.numeric(all_results_twoseam_2017$pitcher)
all_results_twoseam_2017$Season <- 2017

in_zone_data_twoseam_2017 <- in_zone_twoseam_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_twoseam_2017$Season <- 2017

out_of_zone_data_twoseam_2017 <- out_of_zone_twoseam_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_twoseam_2017$Season <- 2017

whiff_predict_data_twoseam_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "FT")
model_whiff_twoseam_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_twoseam_2017)

whiff_pitchers_twoseam_2017 <- data.frame(ranef(model_whiff_twoseam_2017)$pitcher)
colnames(whiff_pitchers_twoseam_2017) <- "In_Whiff"

whiff_data_twoseam_2017 <- whiff_predict_data_twoseam_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_twoseam_2017 <- merge(whiff_data_twoseam_2017,whiff_pitchers_twoseam_2017, by.x = "pitcher", by.y = "row.names")

### 2016 FT----
all_twoseam_2016 <- all_data_2016 %>% filter(pitch_type == "FT")

in_zone_twoseam_2016 <- all_twoseam_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_twoseam_2016 <- all_twoseam_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_twoseam_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_twoseam_2016)

model_out_of_zone_twoseam_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_twoseam_2016)

out_of_zone_pitchers_twoseam_2016 <- data.frame(ranef(model_out_of_zone_twoseam_2016)$pitcher)
colnames(out_of_zone_pitchers_twoseam_2016) <- "OOZ"

in_zone_pitchers_twoseam_2016 <- data.frame(ranef(model_zone_twoseam_2016)$pitcher)
colnames(in_zone_pitchers_twoseam_2016) <- "IZ"

all_results_twoseam_2016 <- merge(in_zone_pitchers_twoseam_2016,out_of_zone_pitchers_twoseam_2016,by = "row.names")
colnames(all_results_twoseam_2016) <- c("pitcher","IZ","OOZ")
all_results_twoseam_2016$pitcher <- as.numeric(all_results_twoseam_2016$pitcher)
all_results_twoseam_2016$Season <- 2016

in_zone_data_twoseam_2016 <- in_zone_twoseam_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_twoseam_2016$Season <- 2016

out_of_zone_data_twoseam_2016 <- out_of_zone_twoseam_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_twoseam_2016$Season <- 2016

whiff_predict_data_twoseam_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "FT")
model_whiff_twoseam_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_twoseam_2016)

whiff_pitchers_twoseam_2016 <- data.frame(ranef(model_whiff_twoseam_2016)$pitcher)
colnames(whiff_pitchers_twoseam_2016) <- "In_Whiff"

whiff_data_twoseam_2016 <- whiff_predict_data_twoseam_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_twoseam_2016 <- merge(whiff_data_twoseam_2016,whiff_pitchers_twoseam_2016, by.x = "pitcher", by.y = "row.names")

### 2015 FT----
all_twoseam_2015 <- all_data_2015 %>% filter(pitch_type == "FT")

in_zone_twoseam_2015 <- all_twoseam_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_twoseam_2015 <- all_twoseam_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_twoseam_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_twoseam_2015)

model_out_of_zone_twoseam_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_twoseam_2015)

out_of_zone_pitchers_twoseam_2015 <- data.frame(ranef(model_out_of_zone_twoseam_2015)$pitcher)
colnames(out_of_zone_pitchers_twoseam_2015) <- "OOZ"

in_zone_pitchers_twoseam_2015 <- data.frame(ranef(model_zone_twoseam_2015)$pitcher)
colnames(in_zone_pitchers_twoseam_2015) <- "IZ"

all_results_twoseam_2015 <- merge(in_zone_pitchers_twoseam_2015,out_of_zone_pitchers_twoseam_2015,by = "row.names")
colnames(all_results_twoseam_2015) <- c("pitcher","IZ","OOZ")
all_results_twoseam_2015$pitcher <- as.numeric(all_results_twoseam_2015$pitcher)
all_results_twoseam_2015$Season <- 2015

in_zone_data_twoseam_2015 <- in_zone_twoseam_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_twoseam_2015$Season <- 2015

out_of_zone_data_twoseam_2015 <- out_of_zone_twoseam_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_twoseam_2015$Season <- 2015

whiff_predict_data_twoseam_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "FT")
model_whiff_twoseam_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_twoseam_2015)

whiff_pitchers_twoseam_2015 <- data.frame(ranef(model_whiff_twoseam_2015)$pitcher)
colnames(whiff_pitchers_twoseam_2015) <- "In_Whiff"

whiff_data_twoseam_2015 <- whiff_predict_data_twoseam_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_twoseam_2015 <- merge(whiff_data_twoseam_2015,whiff_pitchers_twoseam_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_twoseam_all <- rbind(out_of_zone_data_twoseam_2015,out_of_zone_data_twoseam_2016,out_of_zone_data_twoseam_2017,out_of_zone_data_twoseam_2018,out_of_zone_data_2019)
in_zone_data_twoseam_all <- rbind(in_zone_data_twoseam_2015,in_zone_data_twoseam_2016,in_zone_data_twoseam_2017,in_zone_data_twoseam_2018,in_zone_data_twoseam_2019)
all_results_twoseam_all <- rbind(all_results_twoseam_2015,all_results_twoseam_2016,all_results_twoseam_2017,all_results_twoseam_2018,all_results_twoseam_2019)
all_whiffs_twoseam_all <- rbind(whiff_data_twoseam_2015,whiff_data_twoseam_2016,whiff_data_twoseam_2017,whiff_data_twoseam_2018,whiff_data_twoseam_2019)

plate_discipline_twoseam_all <- merge(all_whiffs_twoseam_all,out_of_zone_data_twoseam_all,by = c("pitcher","Season"))
plate_discipline_twoseam_all <- merge(plate_discipline_twoseam_all,in_zone_data_twoseam_all,by = c("pitcher","Season"))
plate_discipline_twoseam_all <- merge(plate_discipline_twoseam_all,all_results_twoseam_all,by = c("pitcher","Season"))

plate_discipline_twoseam_all <- plate_discipline_twoseam_all[,c("pitcher",
                                                                  "player_name",
                                                                  "Season",
                                                                  "Pitches",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "OOZ.Pitches",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "IZ.Pitches",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ")]
plate_discipline_twoseam_all$Score <- plate_discipline_twoseam_all$OOZ - plate_discipline_twoseam_all$IZ + plate_discipline_twoseam_all$In_Whiff

### 2019 FC----
all_cutter_2019 <- all_data_2019 %>% filter(pitch_type == "FC")

in_zone_cutter_2019 <- all_cutter_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_cutter_2019 <- all_cutter_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_cutter_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                data = in_zone_cutter_2019)

model_out_of_zone_cutter_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                       data = out_of_zone_cutter_2019)

out_of_zone_pitchers_cutter_2019 <- data.frame(ranef(model_out_of_zone_cutter_2019)$pitcher)
colnames(out_of_zone_pitchers_cutter_2019) <- "OOZ"

in_zone_pitchers_cutter_2019 <- data.frame(ranef(model_zone_cutter_2019)$pitcher)
colnames(in_zone_pitchers_cutter_2019) <- "IZ"

all_results_cutter_2019 <- merge(in_zone_pitchers_cutter_2019,out_of_zone_pitchers_cutter_2019,by = "row.names")
colnames(all_results_cutter_2019) <- c("pitcher","IZ","OOZ")
all_results_cutter_2019$pitcher <- as.numeric(all_results_cutter_2019$pitcher)
all_results_cutter_2019$Season <- 2019

in_zone_data_cutter_2019 <- in_zone_cutter_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_cutter_2019$Season <- 2019

out_of_zone_data_cutter_2019 <- out_of_zone_cutter_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_cutter_2019$Season <- 2019

whiff_predict_data_cutter_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "FC")
model_whiff_cutter_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                 data = whiff_predict_data_cutter_2019)

whiff_pitchers_cutter_2019 <- data.frame(ranef(model_whiff_cutter_2019)$pitcher)
colnames(whiff_pitchers_cutter_2019) <- "In_Whiff"

whiff_data_cutter_2019 <- whiff_predict_data_cutter_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_cutter_2019 <- merge(whiff_data_cutter_2019,whiff_pitchers_cutter_2019, by.x = "pitcher", by.y = "row.names")

### 2018 FC----
all_cutter_2018 <- all_data_2018 %>% filter(pitch_type == "FC")

in_zone_cutter_2018 <- all_cutter_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_cutter_2018 <- all_cutter_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_cutter_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                data = in_zone_cutter_2018)

model_out_of_zone_cutter_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                       data = out_of_zone_cutter_2018)

out_of_zone_pitchers_cutter_2018 <- data.frame(ranef(model_out_of_zone_cutter_2018)$pitcher)
colnames(out_of_zone_pitchers_cutter_2018) <- "OOZ"

in_zone_pitchers_cutter_2018 <- data.frame(ranef(model_zone_cutter_2018)$pitcher)
colnames(in_zone_pitchers_cutter_2018) <- "IZ"

all_results_cutter_2018 <- merge(in_zone_pitchers_cutter_2018,out_of_zone_pitchers_cutter_2018,by = "row.names")
colnames(all_results_cutter_2018) <- c("pitcher","IZ","OOZ")
all_results_cutter_2018$pitcher <- as.numeric(all_results_cutter_2018$pitcher)
all_results_cutter_2018$Season <- 2018

in_zone_data_cutter_2018 <- in_zone_cutter_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_cutter_2018$Season <- 2018

out_of_zone_data_cutter_2018 <- out_of_zone_cutter_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_cutter_2018$Season <- 2018

whiff_predict_data_cutter_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "FC")
model_whiff_cutter_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                 data = whiff_predict_data_cutter_2018)

whiff_pitchers_cutter_2018 <- data.frame(ranef(model_whiff_cutter_2018)$pitcher)
colnames(whiff_pitchers_cutter_2018) <- "In_Whiff"

whiff_data_cutter_2018 <- whiff_predict_data_cutter_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_cutter_2018 <- merge(whiff_data_cutter_2018,whiff_pitchers_cutter_2018, by.x = "pitcher", by.y = "row.names")

### 2017 FC----
all_cutter_2017 <- all_data_2017 %>% filter(pitch_type == "FC")

in_zone_cutter_2017 <- all_cutter_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_cutter_2017 <- all_cutter_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_cutter_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                data = in_zone_cutter_2017)

model_out_of_zone_cutter_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                       data = out_of_zone_cutter_2017)

out_of_zone_pitchers_cutter_2017 <- data.frame(ranef(model_out_of_zone_cutter_2017)$pitcher)
colnames(out_of_zone_pitchers_cutter_2017) <- "OOZ"

in_zone_pitchers_cutter_2017 <- data.frame(ranef(model_zone_cutter_2017)$pitcher)
colnames(in_zone_pitchers_cutter_2017) <- "IZ"

all_results_cutter_2017 <- merge(in_zone_pitchers_cutter_2017,out_of_zone_pitchers_cutter_2017,by = "row.names")
colnames(all_results_cutter_2017) <- c("pitcher","IZ","OOZ")
all_results_cutter_2017$pitcher <- as.numeric(all_results_cutter_2017$pitcher)
all_results_cutter_2017$Season <- 2017

in_zone_data_cutter_2017 <- in_zone_cutter_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_cutter_2017$Season <- 2017

out_of_zone_data_cutter_2017 <- out_of_zone_cutter_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_cutter_2017$Season <- 2017

whiff_predict_data_cutter_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "FC")
model_whiff_cutter_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                 data = whiff_predict_data_cutter_2017)

whiff_pitchers_cutter_2017 <- data.frame(ranef(model_whiff_cutter_2017)$pitcher)
colnames(whiff_pitchers_cutter_2017) <- "In_Whiff"

whiff_data_cutter_2017 <- whiff_predict_data_cutter_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_cutter_2017 <- merge(whiff_data_cutter_2017,whiff_pitchers_cutter_2017, by.x = "pitcher", by.y = "row.names")

### 2016 FC----
all_cutter_2016 <- all_data_2016 %>% filter(pitch_type == "FC")

in_zone_cutter_2016 <- all_cutter_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_cutter_2016 <- all_cutter_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_cutter_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                data = in_zone_cutter_2016)

model_out_of_zone_cutter_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                       data = out_of_zone_cutter_2016)

out_of_zone_pitchers_cutter_2016 <- data.frame(ranef(model_out_of_zone_cutter_2016)$pitcher)
colnames(out_of_zone_pitchers_cutter_2016) <- "OOZ"

in_zone_pitchers_cutter_2016 <- data.frame(ranef(model_zone_cutter_2016)$pitcher)
colnames(in_zone_pitchers_cutter_2016) <- "IZ"

all_results_cutter_2016 <- merge(in_zone_pitchers_cutter_2016,out_of_zone_pitchers_cutter_2016,by = "row.names")
colnames(all_results_cutter_2016) <- c("pitcher","IZ","OOZ")
all_results_cutter_2016$pitcher <- as.numeric(all_results_cutter_2016$pitcher)
all_results_cutter_2016$Season <- 2016

in_zone_data_cutter_2016 <- in_zone_cutter_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_cutter_2016$Season <- 2016

out_of_zone_data_cutter_2016 <- out_of_zone_cutter_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_cutter_2016$Season <- 2016

whiff_predict_data_cutter_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "FC")
model_whiff_cutter_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                 data = whiff_predict_data_cutter_2016)

whiff_pitchers_cutter_2016 <- data.frame(ranef(model_whiff_cutter_2016)$pitcher)
colnames(whiff_pitchers_cutter_2016) <- "In_Whiff"

whiff_data_cutter_2016 <- whiff_predict_data_cutter_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_cutter_2016 <- merge(whiff_data_cutter_2016,whiff_pitchers_cutter_2016, by.x = "pitcher", by.y = "row.names")

### 2015 FC----
all_cutter_2015 <- all_data_2015 %>% filter(pitch_type == "FC")

in_zone_cutter_2015 <- all_cutter_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_cutter_2015 <- all_cutter_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_cutter_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                data = in_zone_cutter_2015)

model_out_of_zone_cutter_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                       data = out_of_zone_cutter_2015)

out_of_zone_pitchers_cutter_2015 <- data.frame(ranef(model_out_of_zone_cutter_2015)$pitcher)
colnames(out_of_zone_pitchers_cutter_2015) <- "OOZ"

in_zone_pitchers_cutter_2015 <- data.frame(ranef(model_zone_cutter_2015)$pitcher)
colnames(in_zone_pitchers_cutter_2015) <- "IZ"

all_results_cutter_2015 <- merge(in_zone_pitchers_cutter_2015,out_of_zone_pitchers_cutter_2015,by = "row.names")
colnames(all_results_cutter_2015) <- c("pitcher","IZ","OOZ")
all_results_cutter_2015$pitcher <- as.numeric(all_results_cutter_2015$pitcher)
all_results_cutter_2015$Season <- 2015

in_zone_data_cutter_2015 <- in_zone_cutter_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_cutter_2015$Season <- 2015

out_of_zone_data_cutter_2015 <- out_of_zone_cutter_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_cutter_2015$Season <- 2015

whiff_predict_data_cutter_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "FC")
model_whiff_cutter_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                 data = whiff_predict_data_cutter_2015)

whiff_pitchers_cutter_2015 <- data.frame(ranef(model_whiff_cutter_2015)$pitcher)
colnames(whiff_pitchers_cutter_2015) <- "In_Whiff"

whiff_data_cutter_2015 <- whiff_predict_data_cutter_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_cutter_2015 <- merge(whiff_data_cutter_2015,whiff_pitchers_cutter_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_cutter_all <- rbind(out_of_zone_data_cutter_2015,out_of_zone_data_cutter_2016,out_of_zone_data_cutter_2017,out_of_zone_data_cutter_2018,out_of_zone_data_2019)
in_zone_data_cutter_all <- rbind(in_zone_data_cutter_2015,in_zone_data_cutter_2016,in_zone_data_cutter_2017,in_zone_data_cutter_2018,in_zone_data_cutter_2019)
all_results_cutter_all <- rbind(all_results_cutter_2015,all_results_cutter_2016,all_results_cutter_2017,all_results_cutter_2018,all_results_cutter_2019)
all_whiffs_cutter_all <- rbind(whiff_data_cutter_2015,whiff_data_cutter_2016,whiff_data_cutter_2017,whiff_data_cutter_2018,whiff_data_cutter_2019)

plate_discipline_cutter_all <- merge(all_whiffs_cutter_all,out_of_zone_data_cutter_all,by = c("pitcher","Season"))
plate_discipline_cutter_all <- merge(plate_discipline_cutter_all,in_zone_data_cutter_all,by = c("pitcher","Season"))
plate_discipline_cutter_all <- merge(plate_discipline_cutter_all,all_results_cutter_all,by = c("pitcher","Season"))

plate_discipline_cutter_all <- plate_discipline_cutter_all[,c("pitcher",
                                                                "player_name",
                                                                "Season",
                                                                "Pitches",
                                                                "Whiff",
                                                                "xWhiff",
                                                                "In_Whiff",
                                                                "OOZ.Pitches",
                                                                "OOZ.Swing",
                                                                "OOZ.xSwing",
                                                                "OOZ",
                                                                "IZ.Pitches",
                                                                "IZ.Swing",
                                                                "IZ.xSwing",
                                                                "IZ")]
plate_discipline_cutter_all$Score <- plate_discipline_cutter_all$OOZ - plate_discipline_cutter_all$IZ + plate_discipline_cutter_all$In_Whiff

### 2019 SI----
all_sinker_2019 <- all_data_2019 %>% filter(pitch_type == "SI")

in_zone_sinker_2019 <- all_sinker_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_sinker_2019 <- all_sinker_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_sinker_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_sinker_2019)

model_out_of_zone_sinker_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_sinker_2019)

out_of_zone_pitchers_sinker_2019 <- data.frame(ranef(model_out_of_zone_sinker_2019)$pitcher)
colnames(out_of_zone_pitchers_sinker_2019) <- "OOZ"

in_zone_pitchers_sinker_2019 <- data.frame(ranef(model_zone_sinker_2019)$pitcher)
colnames(in_zone_pitchers_sinker_2019) <- "IZ"

all_results_sinker_2019 <- merge(in_zone_pitchers_sinker_2019,out_of_zone_pitchers_sinker_2019,by = "row.names")
colnames(all_results_sinker_2019) <- c("pitcher","IZ","OOZ")
all_results_sinker_2019$pitcher <- as.numeric(all_results_sinker_2019$pitcher)
all_results_sinker_2019$Season <- 2019

in_zone_data_sinker_2019 <- in_zone_sinker_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_sinker_2019$Season <- 2019

out_of_zone_data_sinker_2019 <- out_of_zone_sinker_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_sinker_2019$Season <- 2019

whiff_predict_data_sinker_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "SI")
model_whiff_sinker_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_sinker_2019)

whiff_pitchers_sinker_2019 <- data.frame(ranef(model_whiff_sinker_2019)$pitcher)
colnames(whiff_pitchers_sinker_2019) <- "In_Whiff"

whiff_data_sinker_2019 <- whiff_predict_data_sinker_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_sinker_2019 <- merge(whiff_data_sinker_2019,whiff_pitchers_sinker_2019, by.x = "pitcher", by.y = "row.names")

### 2018 SI----
all_sinker_2018 <- all_data_2018 %>% filter(pitch_type == "SI")

in_zone_sinker_2018 <- all_sinker_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_sinker_2018 <- all_sinker_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_sinker_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_sinker_2018)

model_out_of_zone_sinker_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_sinker_2018)

out_of_zone_pitchers_sinker_2018 <- data.frame(ranef(model_out_of_zone_sinker_2018)$pitcher)
colnames(out_of_zone_pitchers_sinker_2018) <- "OOZ"

in_zone_pitchers_sinker_2018 <- data.frame(ranef(model_zone_sinker_2018)$pitcher)
colnames(in_zone_pitchers_sinker_2018) <- "IZ"

all_results_sinker_2018 <- merge(in_zone_pitchers_sinker_2018,out_of_zone_pitchers_sinker_2018,by = "row.names")
colnames(all_results_sinker_2018) <- c("pitcher","IZ","OOZ")
all_results_sinker_2018$pitcher <- as.numeric(all_results_sinker_2018$pitcher)
all_results_sinker_2018$Season <- 2018

in_zone_data_sinker_2018 <- in_zone_sinker_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_sinker_2018$Season <- 2018

out_of_zone_data_sinker_2018 <- out_of_zone_sinker_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_sinker_2018$Season <- 2018

whiff_predict_data_sinker_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "SI")
model_whiff_sinker_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_sinker_2018)

whiff_pitchers_sinker_2018 <- data.frame(ranef(model_whiff_sinker_2018)$pitcher)
colnames(whiff_pitchers_sinker_2018) <- "In_Whiff"

whiff_data_sinker_2018 <- whiff_predict_data_sinker_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_sinker_2018 <- merge(whiff_data_sinker_2018,whiff_pitchers_sinker_2018, by.x = "pitcher", by.y = "row.names")

### 2017 SI----
all_sinker_2017 <- all_data_2017 %>% filter(pitch_type == "SI")

in_zone_sinker_2017 <- all_sinker_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_sinker_2017 <- all_sinker_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_sinker_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_sinker_2017)

model_out_of_zone_sinker_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_sinker_2017)

out_of_zone_pitchers_sinker_2017 <- data.frame(ranef(model_out_of_zone_sinker_2017)$pitcher)
colnames(out_of_zone_pitchers_sinker_2017) <- "OOZ"

in_zone_pitchers_sinker_2017 <- data.frame(ranef(model_zone_sinker_2017)$pitcher)
colnames(in_zone_pitchers_sinker_2017) <- "IZ"

all_results_sinker_2017 <- merge(in_zone_pitchers_sinker_2017,out_of_zone_pitchers_sinker_2017,by = "row.names")
colnames(all_results_sinker_2017) <- c("pitcher","IZ","OOZ")
all_results_sinker_2017$pitcher <- as.numeric(all_results_sinker_2017$pitcher)
all_results_sinker_2017$Season <- 2017

in_zone_data_sinker_2017 <- in_zone_sinker_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_sinker_2017$Season <- 2017

out_of_zone_data_sinker_2017 <- out_of_zone_sinker_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_sinker_2017$Season <- 2017

whiff_predict_data_sinker_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "SI")
model_whiff_sinker_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_sinker_2017)

whiff_pitchers_sinker_2017 <- data.frame(ranef(model_whiff_sinker_2017)$pitcher)
colnames(whiff_pitchers_sinker_2017) <- "In_Whiff"

whiff_data_sinker_2017 <- whiff_predict_data_sinker_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_sinker_2017 <- merge(whiff_data_sinker_2017,whiff_pitchers_sinker_2017, by.x = "pitcher", by.y = "row.names")

### 2016 SI----
all_sinker_2016 <- all_data_2016 %>% filter(pitch_type == "SI")

in_zone_sinker_2016 <- all_sinker_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_sinker_2016 <- all_sinker_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_sinker_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_sinker_2016)

model_out_of_zone_sinker_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_sinker_2016)

out_of_zone_pitchers_sinker_2016 <- data.frame(ranef(model_out_of_zone_sinker_2016)$pitcher)
colnames(out_of_zone_pitchers_sinker_2016) <- "OOZ"

in_zone_pitchers_sinker_2016 <- data.frame(ranef(model_zone_sinker_2016)$pitcher)
colnames(in_zone_pitchers_sinker_2016) <- "IZ"

all_results_sinker_2016 <- merge(in_zone_pitchers_sinker_2016,out_of_zone_pitchers_sinker_2016,by = "row.names")
colnames(all_results_sinker_2016) <- c("pitcher","IZ","OOZ")
all_results_sinker_2016$pitcher <- as.numeric(all_results_sinker_2016$pitcher)
all_results_sinker_2016$Season <- 2016

in_zone_data_sinker_2016 <- in_zone_sinker_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_sinker_2016$Season <- 2016

out_of_zone_data_sinker_2016 <- out_of_zone_sinker_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_sinker_2016$Season <- 2016

whiff_predict_data_sinker_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "SI")
model_whiff_sinker_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_sinker_2016)

whiff_pitchers_sinker_2016 <- data.frame(ranef(model_whiff_sinker_2016)$pitcher)
colnames(whiff_pitchers_sinker_2016) <- "In_Whiff"

whiff_data_sinker_2016 <- whiff_predict_data_sinker_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_sinker_2016 <- merge(whiff_data_sinker_2016,whiff_pitchers_sinker_2016, by.x = "pitcher", by.y = "row.names")

### 2015 SI----
all_sinker_2015 <- all_data_2015 %>% filter(pitch_type == "SI")

in_zone_sinker_2015 <- all_sinker_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_sinker_2015 <- all_sinker_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_sinker_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_sinker_2015)

model_out_of_zone_sinker_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_sinker_2015)

out_of_zone_pitchers_sinker_2015 <- data.frame(ranef(model_out_of_zone_sinker_2015)$pitcher)
colnames(out_of_zone_pitchers_sinker_2015) <- "OOZ"

in_zone_pitchers_sinker_2015 <- data.frame(ranef(model_zone_sinker_2015)$pitcher)
colnames(in_zone_pitchers_sinker_2015) <- "IZ"

all_results_sinker_2015 <- merge(in_zone_pitchers_sinker_2015,out_of_zone_pitchers_sinker_2015,by = "row.names")
colnames(all_results_sinker_2015) <- c("pitcher","IZ","OOZ")
all_results_sinker_2015$pitcher <- as.numeric(all_results_sinker_2015$pitcher)
all_results_sinker_2015$Season <- 2015

in_zone_data_sinker_2015 <- in_zone_sinker_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_sinker_2015$Season <- 2015

out_of_zone_data_sinker_2015 <- out_of_zone_sinker_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_sinker_2015$Season <- 2015

whiff_predict_data_sinker_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "SI")
model_whiff_sinker_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_sinker_2015)

whiff_pitchers_sinker_2015 <- data.frame(ranef(model_whiff_sinker_2015)$pitcher)
colnames(whiff_pitchers_sinker_2015) <- "In_Whiff"

whiff_data_sinker_2015 <- whiff_predict_data_sinker_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_sinker_2015 <- merge(whiff_data_sinker_2015,whiff_pitchers_sinker_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_sinker_all <- rbind(out_of_zone_data_sinker_2015,out_of_zone_data_sinker_2016,out_of_zone_data_sinker_2017,out_of_zone_data_sinker_2018,out_of_zone_data_2019)
in_zone_data_sinker_all <- rbind(in_zone_data_sinker_2015,in_zone_data_sinker_2016,in_zone_data_sinker_2017,in_zone_data_sinker_2018,in_zone_data_sinker_2019)
all_results_sinker_all <- rbind(all_results_sinker_2015,all_results_sinker_2016,all_results_sinker_2017,all_results_sinker_2018,all_results_sinker_2019)
all_whiffs_sinker_all <- rbind(whiff_data_sinker_2015,whiff_data_sinker_2016,whiff_data_sinker_2017,whiff_data_sinker_2018,whiff_data_sinker_2019)

plate_discipline_sinker_all <- merge(all_whiffs_sinker_all,out_of_zone_data_sinker_all,by = c("pitcher","Season"))
plate_discipline_sinker_all <- merge(plate_discipline_sinker_all,in_zone_data_sinker_all,by = c("pitcher","Season"))
plate_discipline_sinker_all <- merge(plate_discipline_sinker_all,all_results_sinker_all,by = c("pitcher","Season"))

plate_discipline_sinker_all <- plate_discipline_sinker_all[,c("pitcher",
                                                              "player_name",
                                                              "Season",
                                                              "Pitches",
                                                              "Whiff",
                                                              "xWhiff",
                                                              "In_Whiff",
                                                              "OOZ.Pitches",
                                                              "OOZ.Swing",
                                                              "OOZ.xSwing",
                                                              "OOZ",
                                                              "IZ.Pitches",
                                                              "IZ.Swing",
                                                              "IZ.xSwing",
                                                              "IZ")]
plate_discipline_sinker_all$Score <- plate_discipline_sinker_all$OOZ - plate_discipline_sinker_all$IZ + plate_discipline_sinker_all$In_Whiff

### 2019 FS----
all_splitter_2019 <- all_data_2019 %>% filter(pitch_type == "FS")

in_zone_splitter_2019 <- all_splitter_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_splitter_2019 <- all_splitter_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_splitter_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_splitter_2019)

model_out_of_zone_splitter_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_splitter_2019)

out_of_zone_pitchers_splitter_2019 <- data.frame(ranef(model_out_of_zone_splitter_2019)$pitcher)
colnames(out_of_zone_pitchers_splitter_2019) <- "OOZ"

in_zone_pitchers_splitter_2019 <- data.frame(ranef(model_zone_splitter_2019)$pitcher)
colnames(in_zone_pitchers_splitter_2019) <- "IZ"

all_results_splitter_2019 <- merge(in_zone_pitchers_splitter_2019,out_of_zone_pitchers_splitter_2019,by = "row.names")
colnames(all_results_splitter_2019) <- c("pitcher","IZ","OOZ")
all_results_splitter_2019$pitcher <- as.numeric(all_results_splitter_2019$pitcher)
all_results_splitter_2019$Season <- 2019

in_zone_data_splitter_2019 <- in_zone_splitter_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_splitter_2019$Season <- 2019

out_of_zone_data_splitter_2019 <- out_of_zone_splitter_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_splitter_2019$Season <- 2019

whiff_predict_data_splitter_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "FS")
model_whiff_splitter_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_splitter_2019)

whiff_pitchers_splitter_2019 <- data.frame(ranef(model_whiff_splitter_2019)$pitcher)
colnames(whiff_pitchers_splitter_2019) <- "In_Whiff"

whiff_data_splitter_2019 <- whiff_predict_data_splitter_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_splitter_2019 <- merge(whiff_data_splitter_2019,whiff_pitchers_splitter_2019, by.x = "pitcher", by.y = "row.names")

### 2018 FS----
all_splitter_2018 <- all_data_2018 %>% filter(pitch_type == "FS")

in_zone_splitter_2018 <- all_splitter_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_splitter_2018 <- all_splitter_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_splitter_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_splitter_2018)

model_out_of_zone_splitter_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_splitter_2018)

out_of_zone_pitchers_splitter_2018 <- data.frame(ranef(model_out_of_zone_splitter_2018)$pitcher)
colnames(out_of_zone_pitchers_splitter_2018) <- "OOZ"

in_zone_pitchers_splitter_2018 <- data.frame(ranef(model_zone_splitter_2018)$pitcher)
colnames(in_zone_pitchers_splitter_2018) <- "IZ"

all_results_splitter_2018 <- merge(in_zone_pitchers_splitter_2018,out_of_zone_pitchers_splitter_2018,by = "row.names")
colnames(all_results_splitter_2018) <- c("pitcher","IZ","OOZ")
all_results_splitter_2018$pitcher <- as.numeric(all_results_splitter_2018$pitcher)
all_results_splitter_2018$Season <- 2018

in_zone_data_splitter_2018 <- in_zone_splitter_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_splitter_2018$Season <- 2018

out_of_zone_data_splitter_2018 <- out_of_zone_splitter_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_splitter_2018$Season <- 2018

whiff_predict_data_splitter_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "FS")
model_whiff_splitter_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_splitter_2018)

whiff_pitchers_splitter_2018 <- data.frame(ranef(model_whiff_splitter_2018)$pitcher)
colnames(whiff_pitchers_splitter_2018) <- "In_Whiff"

whiff_data_splitter_2018 <- whiff_predict_data_splitter_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_splitter_2018 <- merge(whiff_data_splitter_2018,whiff_pitchers_splitter_2018, by.x = "pitcher", by.y = "row.names")

### 2017 FS----
all_splitter_2017 <- all_data_2017 %>% filter(pitch_type == "FS")

in_zone_splitter_2017 <- all_splitter_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_splitter_2017 <- all_splitter_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_splitter_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_splitter_2017)

model_out_of_zone_splitter_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_splitter_2017)

out_of_zone_pitchers_splitter_2017 <- data.frame(ranef(model_out_of_zone_splitter_2017)$pitcher)
colnames(out_of_zone_pitchers_splitter_2017) <- "OOZ"

in_zone_pitchers_splitter_2017 <- data.frame(ranef(model_zone_splitter_2017)$pitcher)
colnames(in_zone_pitchers_splitter_2017) <- "IZ"

all_results_splitter_2017 <- merge(in_zone_pitchers_splitter_2017,out_of_zone_pitchers_splitter_2017,by = "row.names")
colnames(all_results_splitter_2017) <- c("pitcher","IZ","OOZ")
all_results_splitter_2017$pitcher <- as.numeric(all_results_splitter_2017$pitcher)
all_results_splitter_2017$Season <- 2017

in_zone_data_splitter_2017 <- in_zone_splitter_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_splitter_2017$Season <- 2017

out_of_zone_data_splitter_2017 <- out_of_zone_splitter_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_splitter_2017$Season <- 2017

whiff_predict_data_splitter_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "FS")
model_whiff_splitter_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_splitter_2017)

whiff_pitchers_splitter_2017 <- data.frame(ranef(model_whiff_splitter_2017)$pitcher)
colnames(whiff_pitchers_splitter_2017) <- "In_Whiff"

whiff_data_splitter_2017 <- whiff_predict_data_splitter_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_splitter_2017 <- merge(whiff_data_splitter_2017,whiff_pitchers_splitter_2017, by.x = "pitcher", by.y = "row.names")

### 2016 FS----
all_splitter_2016 <- all_data_2016 %>% filter(pitch_type == "FS")

in_zone_splitter_2016 <- all_splitter_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_splitter_2016 <- all_splitter_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_splitter_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_splitter_2016)

model_out_of_zone_splitter_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_splitter_2016)

out_of_zone_pitchers_splitter_2016 <- data.frame(ranef(model_out_of_zone_splitter_2016)$pitcher)
colnames(out_of_zone_pitchers_splitter_2016) <- "OOZ"

in_zone_pitchers_splitter_2016 <- data.frame(ranef(model_zone_splitter_2016)$pitcher)
colnames(in_zone_pitchers_splitter_2016) <- "IZ"

all_results_splitter_2016 <- merge(in_zone_pitchers_splitter_2016,out_of_zone_pitchers_splitter_2016,by = "row.names")
colnames(all_results_splitter_2016) <- c("pitcher","IZ","OOZ")
all_results_splitter_2016$pitcher <- as.numeric(all_results_splitter_2016$pitcher)
all_results_splitter_2016$Season <- 2016

in_zone_data_splitter_2016 <- in_zone_splitter_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_splitter_2016$Season <- 2016

out_of_zone_data_splitter_2016 <- out_of_zone_splitter_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_splitter_2016$Season <- 2016

whiff_predict_data_splitter_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "FS")
model_whiff_splitter_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_splitter_2016)

whiff_pitchers_splitter_2016 <- data.frame(ranef(model_whiff_splitter_2016)$pitcher)
colnames(whiff_pitchers_splitter_2016) <- "In_Whiff"

whiff_data_splitter_2016 <- whiff_predict_data_splitter_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_splitter_2016 <- merge(whiff_data_splitter_2016,whiff_pitchers_splitter_2016, by.x = "pitcher", by.y = "row.names")

### 2015 FS----
all_splitter_2015 <- all_data_2015 %>% filter(pitch_type == "FS")

in_zone_splitter_2015 <- all_splitter_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_splitter_2015 <- all_splitter_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_splitter_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_splitter_2015)

model_out_of_zone_splitter_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_splitter_2015)

out_of_zone_pitchers_splitter_2015 <- data.frame(ranef(model_out_of_zone_splitter_2015)$pitcher)
colnames(out_of_zone_pitchers_splitter_2015) <- "OOZ"

in_zone_pitchers_splitter_2015 <- data.frame(ranef(model_zone_splitter_2015)$pitcher)
colnames(in_zone_pitchers_splitter_2015) <- "IZ"

all_results_splitter_2015 <- merge(in_zone_pitchers_splitter_2015,out_of_zone_pitchers_splitter_2015,by = "row.names")
colnames(all_results_splitter_2015) <- c("pitcher","IZ","OOZ")
all_results_splitter_2015$pitcher <- as.numeric(all_results_splitter_2015$pitcher)
all_results_splitter_2015$Season <- 2015

in_zone_data_splitter_2015 <- in_zone_splitter_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_splitter_2015$Season <- 2015

out_of_zone_data_splitter_2015 <- out_of_zone_splitter_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_splitter_2015$Season <- 2015

whiff_predict_data_splitter_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "FS")
model_whiff_splitter_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_splitter_2015)

whiff_pitchers_splitter_2015 <- data.frame(ranef(model_whiff_splitter_2015)$pitcher)
colnames(whiff_pitchers_splitter_2015) <- "In_Whiff"

whiff_data_splitter_2015 <- whiff_predict_data_splitter_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_splitter_2015 <- merge(whiff_data_splitter_2015,whiff_pitchers_splitter_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_splitter_all <- rbind(out_of_zone_data_splitter_2015,out_of_zone_data_splitter_2016,out_of_zone_data_splitter_2017,out_of_zone_data_splitter_2018,out_of_zone_data_2019)
in_zone_data_splitter_all <- rbind(in_zone_data_splitter_2015,in_zone_data_splitter_2016,in_zone_data_splitter_2017,in_zone_data_splitter_2018,in_zone_data_splitter_2019)
all_results_splitter_all <- rbind(all_results_splitter_2015,all_results_splitter_2016,all_results_splitter_2017,all_results_splitter_2018,all_results_splitter_2019)
all_whiffs_splitter_all <- rbind(whiff_data_splitter_2015,whiff_data_splitter_2016,whiff_data_splitter_2017,whiff_data_splitter_2018,whiff_data_splitter_2019)

plate_discipline_splitter_all <- merge(all_whiffs_splitter_all,out_of_zone_data_splitter_all,by = c("pitcher","Season"))
plate_discipline_splitter_all <- merge(plate_discipline_splitter_all,in_zone_data_splitter_all,by = c("pitcher","Season"))
plate_discipline_splitter_all <- merge(plate_discipline_splitter_all,all_results_splitter_all,by = c("pitcher","Season"))

plate_discipline_splitter_all <- plate_discipline_splitter_all[,c("pitcher",
                                                              "player_name",
                                                              "Season",
                                                              "Pitches",
                                                              "Whiff",
                                                              "xWhiff",
                                                              "In_Whiff",
                                                              "OOZ.Pitches",
                                                              "OOZ.Swing",
                                                              "OOZ.xSwing",
                                                              "OOZ",
                                                              "IZ.Pitches",
                                                              "IZ.Swing",
                                                              "IZ.xSwing",
                                                              "IZ")]
plate_discipline_splitter_all$Score <- plate_discipline_splitter_all$OOZ - plate_discipline_splitter_all$IZ + plate_discipline_splitter_all$In_Whiff

### 2019 KC----
all_kcurve_2019 <- all_data_2019 %>% filter(pitch_type == "KC")

in_zone_kcurve_2019 <- all_kcurve_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_kcurve_2019 <- all_kcurve_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_kcurve_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_kcurve_2019)

model_out_of_zone_kcurve_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_kcurve_2019)

out_of_zone_pitchers_kcurve_2019 <- data.frame(ranef(model_out_of_zone_kcurve_2019)$pitcher)
colnames(out_of_zone_pitchers_kcurve_2019) <- "OOZ"

in_zone_pitchers_kcurve_2019 <- data.frame(ranef(model_zone_kcurve_2019)$pitcher)
colnames(in_zone_pitchers_kcurve_2019) <- "IZ"

all_results_kcurve_2019 <- merge(in_zone_pitchers_kcurve_2019,out_of_zone_pitchers_kcurve_2019,by = "row.names")
colnames(all_results_kcurve_2019) <- c("pitcher","IZ","OOZ")
all_results_kcurve_2019$pitcher <- as.numeric(all_results_kcurve_2019$pitcher)
all_results_kcurve_2019$Season <- 2019

in_zone_data_kcurve_2019 <- in_zone_kcurve_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_kcurve_2019$Season <- 2019

out_of_zone_data_kcurve_2019 <- out_of_zone_kcurve_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_kcurve_2019$Season <- 2019

whiff_predict_data_kcurve_2019 <- whiff_predict_data_2019 %>% filter(pitch_type == "KC")
model_whiff_kcurve_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_kcurve_2019)

whiff_pitchers_kcurve_2019 <- data.frame(ranef(model_whiff_kcurve_2019)$pitcher)
colnames(whiff_pitchers_kcurve_2019) <- "In_Whiff"

whiff_data_kcurve_2019 <- whiff_predict_data_kcurve_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_kcurve_2019 <- merge(whiff_data_kcurve_2019,whiff_pitchers_kcurve_2019, by.x = "pitcher", by.y = "row.names")

### 2018 KC----
all_kcurve_2018 <- all_data_2018 %>% filter(pitch_type == "KC")

in_zone_kcurve_2018 <- all_kcurve_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_kcurve_2018 <- all_kcurve_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_kcurve_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_kcurve_2018)

model_out_of_zone_kcurve_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_kcurve_2018)

out_of_zone_pitchers_kcurve_2018 <- data.frame(ranef(model_out_of_zone_kcurve_2018)$pitcher)
colnames(out_of_zone_pitchers_kcurve_2018) <- "OOZ"

in_zone_pitchers_kcurve_2018 <- data.frame(ranef(model_zone_kcurve_2018)$pitcher)
colnames(in_zone_pitchers_kcurve_2018) <- "IZ"

all_results_kcurve_2018 <- merge(in_zone_pitchers_kcurve_2018,out_of_zone_pitchers_kcurve_2018,by = "row.names")
colnames(all_results_kcurve_2018) <- c("pitcher","IZ","OOZ")
all_results_kcurve_2018$pitcher <- as.numeric(all_results_kcurve_2018$pitcher)
all_results_kcurve_2018$Season <- 2018

in_zone_data_kcurve_2018 <- in_zone_kcurve_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_kcurve_2018$Season <- 2018

out_of_zone_data_kcurve_2018 <- out_of_zone_kcurve_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_kcurve_2018$Season <- 2018

whiff_predict_data_kcurve_2018 <- whiff_predict_data_2018 %>% filter(pitch_type == "KC")
model_whiff_kcurve_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_kcurve_2018)

whiff_pitchers_kcurve_2018 <- data.frame(ranef(model_whiff_kcurve_2018)$pitcher)
colnames(whiff_pitchers_kcurve_2018) <- "In_Whiff"

whiff_data_kcurve_2018 <- whiff_predict_data_kcurve_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_kcurve_2018 <- merge(whiff_data_kcurve_2018,whiff_pitchers_kcurve_2018, by.x = "pitcher", by.y = "row.names")

### 2017 KC----
all_kcurve_2017 <- all_data_2017 %>% filter(pitch_type == "KC")

in_zone_kcurve_2017 <- all_kcurve_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_kcurve_2017 <- all_kcurve_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_kcurve_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_kcurve_2017)

model_out_of_zone_kcurve_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_kcurve_2017)

out_of_zone_pitchers_kcurve_2017 <- data.frame(ranef(model_out_of_zone_kcurve_2017)$pitcher)
colnames(out_of_zone_pitchers_kcurve_2017) <- "OOZ"

in_zone_pitchers_kcurve_2017 <- data.frame(ranef(model_zone_kcurve_2017)$pitcher)
colnames(in_zone_pitchers_kcurve_2017) <- "IZ"

all_results_kcurve_2017 <- merge(in_zone_pitchers_kcurve_2017,out_of_zone_pitchers_kcurve_2017,by = "row.names")
colnames(all_results_kcurve_2017) <- c("pitcher","IZ","OOZ")
all_results_kcurve_2017$pitcher <- as.numeric(all_results_kcurve_2017$pitcher)
all_results_kcurve_2017$Season <- 2017

in_zone_data_kcurve_2017 <- in_zone_kcurve_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_kcurve_2017$Season <- 2017

out_of_zone_data_kcurve_2017 <- out_of_zone_kcurve_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_kcurve_2017$Season <- 2017

whiff_predict_data_kcurve_2017 <- whiff_predict_data_2017 %>% filter(pitch_type == "KC")
model_whiff_kcurve_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_kcurve_2017)

whiff_pitchers_kcurve_2017 <- data.frame(ranef(model_whiff_kcurve_2017)$pitcher)
colnames(whiff_pitchers_kcurve_2017) <- "In_Whiff"

whiff_data_kcurve_2017 <- whiff_predict_data_kcurve_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_kcurve_2017 <- merge(whiff_data_kcurve_2017,whiff_pitchers_kcurve_2017, by.x = "pitcher", by.y = "row.names")

### 2016 KC----
all_kcurve_2016 <- all_data_2016 %>% filter(pitch_type == "KC")

in_zone_kcurve_2016 <- all_kcurve_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_kcurve_2016 <- all_kcurve_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_kcurve_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_kcurve_2016)

model_out_of_zone_kcurve_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_kcurve_2016)

out_of_zone_pitchers_kcurve_2016 <- data.frame(ranef(model_out_of_zone_kcurve_2016)$pitcher)
colnames(out_of_zone_pitchers_kcurve_2016) <- "OOZ"

in_zone_pitchers_kcurve_2016 <- data.frame(ranef(model_zone_kcurve_2016)$pitcher)
colnames(in_zone_pitchers_kcurve_2016) <- "IZ"

all_results_kcurve_2016 <- merge(in_zone_pitchers_kcurve_2016,out_of_zone_pitchers_kcurve_2016,by = "row.names")
colnames(all_results_kcurve_2016) <- c("pitcher","IZ","OOZ")
all_results_kcurve_2016$pitcher <- as.numeric(all_results_kcurve_2016$pitcher)
all_results_kcurve_2016$Season <- 2016

in_zone_data_kcurve_2016 <- in_zone_kcurve_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_kcurve_2016$Season <- 2016

out_of_zone_data_kcurve_2016 <- out_of_zone_kcurve_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_kcurve_2016$Season <- 2016

whiff_predict_data_kcurve_2016 <- whiff_predict_data_2016 %>% filter(pitch_type == "KC")
model_whiff_kcurve_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_kcurve_2016)

whiff_pitchers_kcurve_2016 <- data.frame(ranef(model_whiff_kcurve_2016)$pitcher)
colnames(whiff_pitchers_kcurve_2016) <- "In_Whiff"

whiff_data_kcurve_2016 <- whiff_predict_data_kcurve_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_kcurve_2016 <- merge(whiff_data_kcurve_2016,whiff_pitchers_kcurve_2016, by.x = "pitcher", by.y = "row.names")

### 2015 KC----
all_kcurve_2015 <- all_data_2015 %>% filter(pitch_type == "KC")

in_zone_kcurve_2015 <- all_kcurve_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_kcurve_2015 <- all_kcurve_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_kcurve_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_kcurve_2015)

model_out_of_zone_kcurve_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_kcurve_2015)

out_of_zone_pitchers_kcurve_2015 <- data.frame(ranef(model_out_of_zone_kcurve_2015)$pitcher)
colnames(out_of_zone_pitchers_kcurve_2015) <- "OOZ"

in_zone_pitchers_kcurve_2015 <- data.frame(ranef(model_zone_kcurve_2015)$pitcher)
colnames(in_zone_pitchers_kcurve_2015) <- "IZ"

all_results_kcurve_2015 <- merge(in_zone_pitchers_kcurve_2015,out_of_zone_pitchers_kcurve_2015,by = "row.names")
colnames(all_results_kcurve_2015) <- c("pitcher","IZ","OOZ")
all_results_kcurve_2015$pitcher <- as.numeric(all_results_kcurve_2015$pitcher)
all_results_kcurve_2015$Season <- 2015

in_zone_data_kcurve_2015 <- in_zone_kcurve_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_kcurve_2015$Season <- 2015

out_of_zone_data_kcurve_2015 <- out_of_zone_kcurve_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_kcurve_2015$Season <- 2015

whiff_predict_data_kcurve_2015 <- whiff_predict_data_2015 %>% filter(pitch_type == "KC")
model_whiff_kcurve_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_kcurve_2015)

whiff_pitchers_kcurve_2015 <- data.frame(ranef(model_whiff_kcurve_2015)$pitcher)
colnames(whiff_pitchers_kcurve_2015) <- "In_Whiff"

whiff_data_kcurve_2015 <- whiff_predict_data_kcurve_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_kcurve_2015 <- merge(whiff_data_kcurve_2015,whiff_pitchers_kcurve_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_kcurve_all <- rbind(out_of_zone_data_kcurve_2015,out_of_zone_data_kcurve_2016,out_of_zone_data_kcurve_2017,out_of_zone_data_kcurve_2018,out_of_zone_data_2019)
in_zone_data_kcurve_all <- rbind(in_zone_data_kcurve_2015,in_zone_data_kcurve_2016,in_zone_data_kcurve_2017,in_zone_data_kcurve_2018,in_zone_data_kcurve_2019)
all_results_kcurve_all <- rbind(all_results_kcurve_2015,all_results_kcurve_2016,all_results_kcurve_2017,all_results_kcurve_2018,all_results_kcurve_2019)
all_whiffs_kcurve_all <- rbind(whiff_data_kcurve_2015,whiff_data_kcurve_2016,whiff_data_kcurve_2017,whiff_data_kcurve_2018,whiff_data_kcurve_2019)

plate_discipline_kcurve_all <- merge(all_whiffs_kcurve_all,out_of_zone_data_kcurve_all,by = c("pitcher","Season"))
plate_discipline_kcurve_all <- merge(plate_discipline_kcurve_all,in_zone_data_kcurve_all,by = c("pitcher","Season"))
plate_discipline_kcurve_all <- merge(plate_discipline_kcurve_all,all_results_kcurve_all,by = c("pitcher","Season"))

plate_discipline_kcurve_all <- plate_discipline_kcurve_all[,c("pitcher",
                                                                  "player_name",
                                                                  "Season",
                                                                  "Pitches",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "OOZ.Pitches",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "IZ.Pitches",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ")]
plate_discipline_kcurve_all$Score <- plate_discipline_kcurve_all$OOZ - plate_discipline_kcurve_all$IZ + plate_discipline_kcurve_all$In_Whiff

### By Pitch Type All ----
plate_discipline_change_all$pitch_type <- "CH"
plate_discipline_curve_all$pitch_type <- "CU"
plate_discipline_cutter_all$pitch_type <- "FC"
plate_discipline_fourseam_all$pitch_type <- "FF"
plate_discipline_kcurve_all$pitch_type <- "KC"
plate_discipline_sinker_all$pitch_type <- "SI"
plate_discipline_slider_all$pitch_type <- "SL"
plate_discipline_splitter_all$pitch_type <- "FS"
plate_discipline_twoseam_all$pitch_type <- "FT"

plate_discipline_all_pitch_type <- rbind(plate_discipline_change_all,
                                         plate_discipline_curve_all,
                                         plate_discipline_cutter_all,
                                         plate_discipline_fourseam_all,
                                         plate_discipline_kcurve_all,
                                         plate_discipline_sinker_all,
                                         plate_discipline_slider_all,
                                         plate_discipline_splitter_all,
                                         plate_discipline_twoseam_all)

plate_discipline_all_pitch_type <- plate_discipline_all_pitch_type[,c("pitcher",
                                                                      "player_name",
                                                                      "Season",
                                                                      "pitch_type",
                                                                      "Pitches",
                                                                      "Whiff",
                                                                      "xWhiff",
                                                                      "In_Whiff",
                                                                      "OOZ.Pitches",
                                                                      "OOZ.Swing",
                                                                      "OOZ.xSwing",
                                                                      "OOZ",
                                                                      "IZ.Pitches",
                                                                      "IZ.Swing",
                                                                      "IZ.xSwing",
                                                                      "IZ",
                                                                      "Score")]

saveRDS(plate_discipline, "plate_discipline.rds")
saveRDS(plate_discipline_all, "plate_discipline_all.rds")
saveRDS(plate_discipline_all_pitch_type, "plate_discipline_all_pitch_type.rds")

all_pitches_stats <- readRDS("all_pitches_stats.rds")

all_pitches_stats <- mutate(all_pitches_stats, pitch_group=ifelse(pitch_type %in% c("FF", "FT","FC", "SI", "FA"),
                                                                 "Fastball", ifelse(pitch_type %in% c("SL", "EP","CU", "KN", "KC"),
                                                                                    "Breaking", "OffSpeed")))
all_pitches_clean <- all_pitches_stats[complete.cases(all_pitches_stats),]

all_pitches_stats_summary <- all_pitches_clean %>% 
  group_by(pitcher, player_name, Season, pitch_group,p_throws) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Count = n(),
            Velocity = round(mean(release_speed),1),
            Move_x = round(mean(pfx_x),3),
            Move_z = round(mean(pfx_z),3),
            Effective_Speed = round(mean(effective_speed),1),
            Spin_Rate = round(mean(release_spin_rate),3),
            Release_Extension = round(mean(release_extension),3))

all_pitches_stats_summary <- all_pitches_stats_summary %>%
  filter(pitch_type == "CH" |
         pitch_type == "CU" |
         pitch_type == "FC" |
         pitch_type == "FF" |
         pitch_type == "KC" |
         pitch_type == "SI" |
         pitch_type == "SL" |
         pitch_type == "FS" |
         pitch_type == "FT")

plate_discipline_all_pitch_type_stats <- left_join(plate_discipline_all_pitch_type,all_pitches_stats_summary, by = c("pitcher", "Season", "pitch_type"))

plate_discipline_all_pitch_type_stats$Move_x <- ifelse(plate_discipline_all_pitch_type_stats$p_throws == "R", 
                                                       plate_discipline_all_pitch_type_stats$Move_x * -1,
                                                       plate_discipline_all_pitch_type_stats$Move_x)

plate_discipline_all_pitch_type_stats$BU <- round(plate_discipline_all_pitch_type_stats$Spin_Rate/plate_discipline_all_pitch_type_stats$Velocity,2)

pitch_type_stats <- plate_discipline_all_pitch_type_stats %>%
  group_by(pitch_type) %>%
  summarise(AVG = round(mean(Score),3))

plate_discipline_all_pitch_type_stats <- left_join(plate_discipline_all_pitch_type_stats,pitch_type_stats, by = "pitch_type")
plate_discipline_all_pitch_type_stats$Stuff <- plate_discipline_all_pitch_type_stats$Score - plate_discipline_all_pitch_type_stats$AVG


#Pitch Stat Correlations----
breaking <- plate_discipline_all_pitch_type_stats %>%
  filter(pitch_type == "SL" | pitch_type == "CU" | pitch_type == "KC")
breaking <- breaking[,c("Score",
                     "Velocity",
                     "Move_x",
                     "Move_z",
                     "Spin_Rate",
                     "Effective_Speed",
                     "Release_Extension",
                     "BU")]
breakingcorr <- rcorr(as.matrix(breaking))
breakingcorr <- flattenCorrMatrix(breakingcorr$r, breakingcorr$P)

fastball <- plate_discipline_all_pitch_type_stats %>%
  filter(pitch_type == "FF" | pitch_type == "FT" | pitch_type == "FC" | pitch_type == "SI")
fastball <- fastball[,c("Score",
                     "Velocity",
                     "Move_x",
                     "Move_z",
                     "Spin_Rate",
                     "Effective_Speed",
                     "Release_Extension",
                     "BU")]
fastballcorr <- rcorr(as.matrix(fastball))
fastballcorr <- flattenCorrMatrix(fastballcorr$r, fastballcorr$P)

offspeed <- plate_discipline_all_pitch_type_stats %>%
  filter(pitch_type == "FS" | pitch_type == "CH")
offspeed <- offspeed[,c("Score",
                        "Velocity",
                        "Move_x",
                        "Move_z",
                        "Spin_Rate",
                        "Effective_Speed",
                        "Release_Extension",
                        "BU")]
offspeedcorr <- rcorr(as.matrix(offspeed))
offspeedcorr <- flattenCorrMatrix(offspeedcorr$r, offspeedcorr$P)


# wOBA Modeling----
all_pitches_trim <- readRDS("all_pitches_trim.RDS")
all_splits <- split(all_pitches_trim, with(all_pitches_trim, interaction(pitch_group,count)), drop = TRUE)
all_predict_wOBA <- all_pitches_trim[FALSE,]
all_predict_wOBA$predict_wOBA <- numeric()

all_pitches_trim_BIP <- all_pitches_trim %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

all_splits_BIP <- split(all_pitches_trim_BIP, with(all_pitches_trim_BIP, interaction(pitch_group,count)), drop = TRUE)


predict_wOBA <- function(x)
{
  
  if(nrow(all_splits_BIP[[x]]) > 100)
  {
    fit <- gam(woba_value ~ s(plate_x,plate_z), data=all_splits_BIP[[x]])
    
    all_splits[[x]]$predict_wOBA <- exp(predict(fit,all_splits[[x]]))/(1 + exp(predict(fit,all_splits[[x]])))
    
    all_predict_wOBA <- rbind(all_predict_wOBA,all_splits[[x]])
  }
}

wOBA_predict_data <- lapply(1:length(all_splits_BIP), predict_wOBA)
wOBA_predict_data <- do.call("rbind", wOBA_predict_data)

wOBA_results <- wOBA_predict_data %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)


bip_with_predicted <- wOBA_predict_data %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                      data = bip_with_predicted)

woba_pitchers <- data.frame(ranef(model_wOBA)$pitcher)
colnames(woba_pitchers) <- "In_wOBA"

wOBA_results <- merge(wOBA_results,woba_pitchers, by.x = "pitcher", by.y = "row.names")


###2019 wOBA ----
wOBA_predict_data_2019 <- wOBA_predict_data %>% 
  filter(Season == 2019)

wOBA_results_2019 <- wOBA_predict_data_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2019 <- wOBA_predict_data_2019 %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_2019 <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                   data = bip_with_predicted_2019)

woba_pitchers_2019 <- data.frame(ranef(model_wOBA_2019)$pitcher)
colnames(woba_pitchers_2019) <- "In_wOBA"

wOBA_results_2019 <- merge(wOBA_results_2019,woba_pitchers_2019, by.x = "pitcher", by.y = "row.names")

###2018 wOBA ----
wOBA_predict_data_2018 <- wOBA_predict_data %>% 
  filter(Season == 2018)

wOBA_results_2018 <- wOBA_predict_data_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2018 <- wOBA_predict_data_2018 %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_2018 <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                        data = bip_with_predicted_2018)

woba_pitchers_2018 <- data.frame(ranef(model_wOBA_2018)$pitcher)
colnames(woba_pitchers_2018) <- "In_wOBA"

wOBA_results_2018 <- merge(wOBA_results_2018,woba_pitchers_2018, by.x = "pitcher", by.y = "row.names")

###2017 wOBA ----
wOBA_predict_data_2017 <- wOBA_predict_data %>% 
  filter(Season == 2017)

wOBA_results_2017 <- wOBA_predict_data_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2017 <- wOBA_predict_data_2017 %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_2017 <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                        data = bip_with_predicted_2017)

woba_pitchers_2017 <- data.frame(ranef(model_wOBA_2017)$pitcher)
colnames(woba_pitchers_2017) <- "In_wOBA"

wOBA_results_2017 <- merge(wOBA_results_2017,woba_pitchers_2017, by.x = "pitcher", by.y = "row.names")

###2016 wOBA ----
wOBA_predict_data_2016 <- wOBA_predict_data %>% 
  filter(Season == 2016)

wOBA_results_2016 <- wOBA_predict_data_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2016 <- wOBA_predict_data_2016 %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_2016 <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                        data = bip_with_predicted_2016)

woba_pitchers_2016 <- data.frame(ranef(model_wOBA_2016)$pitcher)
colnames(woba_pitchers_2016) <- "In_wOBA"

wOBA_results_2016 <- merge(wOBA_results_2016,woba_pitchers_2016, by.x = "pitcher", by.y = "row.names")

###2015 wOBA ----
wOBA_predict_data_2015 <- wOBA_predict_data %>% 
  filter(Season == 2015)

wOBA_results_2015 <- wOBA_predict_data_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2015 <- wOBA_predict_data_2015 %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_2015 <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                        data = bip_with_predicted_2015)

woba_pitchers_2015 <- data.frame(ranef(model_wOBA_2015)$pitcher)
colnames(woba_pitchers_2015) <- "In_wOBA"

wOBA_results_2015 <- merge(wOBA_results_2015,woba_pitchers_2015, by.x = "pitcher", by.y = "row.names")

wOBA_results_season <- rbind(wOBA_results_2019, wOBA_results_2018, wOBA_results_2017, wOBA_results_2016, wOBA_results_2015)

plate_discipline_all <- readRDS("plate_discipline_all.rds")

plate_discipline_all_woba <- left_join(plate_discipline_all,wOBA_results_season[,c(1,3,5,6,7)], by = c("pitcher", "Season"))

in_zone_avg <- mean(in_zone$predict)
out_of_zone_avg <- mean(out_of_zone$predict)
whiff_Avg <- mean(whiff_predict_data$predict_whiff)
woba_avg <- mean(wOBA_predict_data$predict_wOBA)

in_zone_sd <- sd(plate_discipline_all_woba$IZ.xSwing)
out_of_zone_sd <- sd(plate_discipline_all_woba$OOZ.xSwing)
whiff_sd <- sd(plate_discipline_all_woba$xWhiff)
woba_sd <- sd(plate_discipline_all_woba$xwOBA)

plate_discipline_all_woba$Command <- ((plate_discipline_all_woba$xWhiff - whiff_Avg)/whiff_sd) +
  ((plate_discipline_all_woba$OOZ.xSwing-out_of_zone_avg)/out_of_zone_sd) + 
  (((in_zone_avg - plate_discipline_all_woba$IZ.xSwing)/in_zone_sd)) + 
  (((woba_avg - plate_discipline_all_woba$xwOBA)/woba_sd))


###Model Building ---- 
results <- read.csv("fangraphs.csv")

master <- read.csv("master.csv")
master <- master[,c("fg_id","mlb_id")]
master$fg_id <- as.numeric(as.character(master$fg_id))
master$mlb_id <- as.numeric(as.character(master$mlb_id))

results <- left_join(results,master, by = c("pitcher" = "fg_id"))
results <- results %>%
  filter(!is.na(mlb_id))

results <- left_join(results,plate_discipline_all_woba[,c(1,3,6,7,10,11,14,15,17,19,20)], by = c("Season", "mlb_id" = "pitcher"))

era_model_stuff <- lm(ERA ~ In_Whiff + IZ + OOZ + In_wOBA + Command, data = results)
results$Stuff <- predict.lm(era_model_stuff,results)

era_stuff_model_seasonal <- lmer(ERA ~ In_Whiff + IZ + OOZ + In_wOBA + Command + (1|Season),
                                 data = results)
results$Stuff <- predict(era_stuff_model_seasonal,results)


results <- results %>% 
  arrange(pitcher, Season) %>%
  group_by(pitcher) %>% 
  mutate(Stuff_Prev = dplyr::lag(Stuff, n=1, default=NA))

prev <- results %>%
  filter(!is.na(Stuff_Prev))

plate_discipline_all_woba$StuffERA <- round(predict(era_stuff_model_seasonal, newdata = plate_discipline_all_woba),3)

###2019 wOBA Breaking ----
wOBA_predict_data_2019_Breaking <- wOBA_predict_data_2019 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

wOBA_results_2019_Breaking <- wOBA_predict_data_2019_Breaking %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2019_Breaking <- wOBA_predict_data_2019_Breaking %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2019_Breaking %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2019_Breaking <- bip_with_predicted_2019_Breaking %>%
  filter(pitcher %in% countlist)

model_wOBA_2019_Breaking <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                           data = bip_with_predicted_2019_Breaking)

woba_pitchers_2019_Breaking <- data.frame(ranef(model_wOBA_2019_Breaking)$pitcher)
colnames(woba_pitchers_2019_Breaking) <- "In_wOBA"

wOBA_results_2019_Breaking <- merge(wOBA_results_2019_Breaking,woba_pitchers_2019_Breaking, by.x = "pitcher", by.y = "row.names")

###2018 wOBA Breaking ----
wOBA_predict_data_2018_Breaking <- wOBA_predict_data_2018 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

wOBA_results_2018_Breaking <- wOBA_predict_data_2018_Breaking %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2018_Breaking <- wOBA_predict_data_2018_Breaking %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2018_Breaking %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2018_Breaking <- bip_with_predicted_2018_Breaking %>%
  filter(pitcher %in% countlist)

model_wOBA_2018_Breaking <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                           data = bip_with_predicted_2018_Breaking)

woba_pitchers_2018_Breaking <- data.frame(ranef(model_wOBA_2018_Breaking)$pitcher)
colnames(woba_pitchers_2018_Breaking) <- "In_wOBA"

wOBA_results_2018_Breaking <- merge(wOBA_results_2018_Breaking,woba_pitchers_2018_Breaking, by.x = "pitcher", by.y = "row.names")

###2017 wOBA Breaking ----
wOBA_predict_data_2017_Breaking <- wOBA_predict_data_2017 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

wOBA_results_2017_Breaking <- wOBA_predict_data_2017_Breaking %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2017_Breaking <- wOBA_predict_data_2017_Breaking %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2017_Breaking %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2017_Breaking <- bip_with_predicted_2017_Breaking %>%
  filter(pitcher %in% countlist)

model_wOBA_2017_Breaking <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                           data = bip_with_predicted_2017_Breaking)

woba_pitchers_2017_Breaking <- data.frame(ranef(model_wOBA_2017_Breaking)$pitcher)
colnames(woba_pitchers_2017_Breaking) <- "In_wOBA"

wOBA_results_2017_Breaking <- merge(wOBA_results_2017_Breaking,woba_pitchers_2017_Breaking, by.x = "pitcher", by.y = "row.names")

###2016 wOBA Breaking ----
wOBA_predict_data_2016_Breaking <- wOBA_predict_data_2016 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

wOBA_results_2016_Breaking <- wOBA_predict_data_2016_Breaking %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2016_Breaking <- wOBA_predict_data_2016_Breaking %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2016_Breaking %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2016_Breaking <- bip_with_predicted_2016_Breaking %>%
  filter(pitcher %in% countlist)


model_wOBA_2016_Breaking <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                           data = bip_with_predicted_2016_Breaking)

woba_pitchers_2016_Breaking <- data.frame(ranef(model_wOBA_2016_Breaking)$pitcher)
colnames(woba_pitchers_2016_Breaking) <- "In_wOBA"

wOBA_results_2016_Breaking <- merge(wOBA_results_2016_Breaking,woba_pitchers_2016_Breaking, by.x = "pitcher", by.y = "row.names")

###2015 wOBA Breaking ----
wOBA_predict_data_2015_Breaking <- wOBA_predict_data_2015 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

wOBA_results_2015_Breaking <- wOBA_predict_data_2015_Breaking %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2015_Breaking <- wOBA_predict_data_2015_Breaking %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2015_Breaking %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2015_Breaking <- bip_with_predicted_2015_Breaking %>%
  filter(pitcher %in% countlist)


model_wOBA_2015_Breaking <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                           data = bip_with_predicted_2015_Breaking)

woba_pitchers_2015_Breaking <- data.frame(ranef(model_wOBA_2015_Breaking)$pitcher)
colnames(woba_pitchers_2015_Breaking) <- "In_wOBA"

wOBA_results_2015_Breaking <- merge(wOBA_results_2015_Breaking,woba_pitchers_2015_Breaking, by.x = "pitcher", by.y = "row.names")

wOBA_results_all_Breaking <- rbind(wOBA_results_2015_Breaking,wOBA_results_2016_Breaking,wOBA_results_2017_Breaking,wOBA_results_2018_Breaking,wOBA_results_2019_Breaking)

wOBA_results_all_Breaking$pitch_type <- "Breaking"

###2019 wOBA OffSpeed ----
wOBA_predict_data_2019_OffSpeed <- wOBA_predict_data_2019 %>% 
  filter(pitch_type %in% c("CH","FS"))

wOBA_results_2019_OffSpeed <- wOBA_predict_data_2019_OffSpeed %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2019_OffSpeed <- wOBA_predict_data_2019_OffSpeed %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2019_OffSpeed %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2019_OffSpeed <- bip_with_predicted_2019_OffSpeed %>%
  filter(pitcher %in% countlist)

model_wOBA_2019_OffSpeed <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2019_OffSpeed)

woba_pitchers_2019_OffSpeed <- data.frame(ranef(model_wOBA_2019_OffSpeed)$pitcher)
colnames(woba_pitchers_2019_OffSpeed) <- "In_wOBA"

wOBA_results_2019_OffSpeed <- merge(wOBA_results_2019_OffSpeed,woba_pitchers_2019_OffSpeed, by.x = "pitcher", by.y = "row.names")

###2018 wOBA OffSpeed ----
wOBA_predict_data_2018_OffSpeed <- wOBA_predict_data_2018 %>% 
  filter(pitch_type %in% c("CH","FS"))

wOBA_results_2018_OffSpeed <- wOBA_predict_data_2018_OffSpeed %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2018_OffSpeed <- wOBA_predict_data_2018_OffSpeed %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2018_OffSpeed %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2018_OffSpeed <- bip_with_predicted_2018_OffSpeed %>%
  filter(pitcher %in% countlist)

model_wOBA_2018_OffSpeed <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2018_OffSpeed)

woba_pitchers_2018_OffSpeed <- data.frame(ranef(model_wOBA_2018_OffSpeed)$pitcher)
colnames(woba_pitchers_2018_OffSpeed) <- "In_wOBA"

wOBA_results_2018_OffSpeed <- merge(wOBA_results_2018_OffSpeed,woba_pitchers_2018_OffSpeed, by.x = "pitcher", by.y = "row.names")

###2017 wOBA OffSpeed ----
wOBA_predict_data_2017_OffSpeed <- wOBA_predict_data_2017 %>% 
  filter(pitch_type %in% c("CH","FS"))

wOBA_results_2017_OffSpeed <- wOBA_predict_data_2017_OffSpeed %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2017_OffSpeed <- wOBA_predict_data_2017_OffSpeed %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2017_OffSpeed %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2017_OffSpeed <- bip_with_predicted_2017_OffSpeed %>%
  filter(pitcher %in% countlist)

model_wOBA_2017_OffSpeed <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2017_OffSpeed)

woba_pitchers_2017_OffSpeed <- data.frame(ranef(model_wOBA_2017_OffSpeed)$pitcher)
colnames(woba_pitchers_2017_OffSpeed) <- "In_wOBA"

wOBA_results_2017_OffSpeed <- merge(wOBA_results_2017_OffSpeed,woba_pitchers_2017_OffSpeed, by.x = "pitcher", by.y = "row.names")

###2016 wOBA OffSpeed ----
wOBA_predict_data_2016_OffSpeed <- wOBA_predict_data_2016 %>% 
  filter(pitch_type %in% c("CH","FS"))

wOBA_results_2016_OffSpeed <- wOBA_predict_data_2016_OffSpeed %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2016_OffSpeed <- wOBA_predict_data_2016_OffSpeed %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2016_OffSpeed %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2016_OffSpeed <- bip_with_predicted_2016_OffSpeed %>%
  filter(pitcher %in% countlist)


model_wOBA_2016_OffSpeed <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2016_OffSpeed)

woba_pitchers_2016_OffSpeed <- data.frame(ranef(model_wOBA_2016_OffSpeed)$pitcher)
colnames(woba_pitchers_2016_OffSpeed) <- "In_wOBA"

wOBA_results_2016_OffSpeed <- merge(wOBA_results_2016_OffSpeed,woba_pitchers_2016_OffSpeed, by.x = "pitcher", by.y = "row.names")

###2015 wOBA OffSpeed ----
wOBA_predict_data_2015_OffSpeed <- wOBA_predict_data_2015 %>% 
  filter(pitch_type %in% c("CH","FS"))

wOBA_results_2015_OffSpeed <- wOBA_predict_data_2015_OffSpeed %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2015_OffSpeed <- wOBA_predict_data_2015_OffSpeed %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2015_OffSpeed %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2015_OffSpeed <- bip_with_predicted_2015_OffSpeed %>%
  filter(pitcher %in% countlist)


model_wOBA_2015_OffSpeed <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2015_OffSpeed)

woba_pitchers_2015_OffSpeed <- data.frame(ranef(model_wOBA_2015_OffSpeed)$pitcher)
colnames(woba_pitchers_2015_OffSpeed) <- "In_wOBA"

wOBA_results_2015_OffSpeed <- merge(wOBA_results_2015_OffSpeed,woba_pitchers_2015_OffSpeed, by.x = "pitcher", by.y = "row.names")

wOBA_results_all_OffSpeed <- rbind(wOBA_results_2015_OffSpeed,wOBA_results_2016_OffSpeed,wOBA_results_2017_OffSpeed,wOBA_results_2018_OffSpeed,wOBA_results_2019_OffSpeed)

wOBA_results_all_OffSpeed$pitch_type <- "Offspeed"

###2019 wOBA Fastball ----
wOBA_predict_data_2019_Fastball <- wOBA_predict_data_2019 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

wOBA_results_2019_Fastball <- wOBA_predict_data_2019_Fastball %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2019_Fastball <- wOBA_predict_data_2019_Fastball %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2019_Fastball %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2019_Fastball <- bip_with_predicted_2019_Fastball %>%
  filter(pitcher %in% countlist)

model_wOBA_2019_Fastball <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2019_Fastball)

woba_pitchers_2019_Fastball <- data.frame(ranef(model_wOBA_2019_Fastball)$pitcher)
colnames(woba_pitchers_2019_Fastball) <- "In_wOBA"

wOBA_results_2019_Fastball <- merge(wOBA_results_2019_Fastball,woba_pitchers_2019_Fastball, by.x = "pitcher", by.y = "row.names")

###2018 wOBA Fastball ----
wOBA_predict_data_2018_Fastball <- wOBA_predict_data_2018 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

wOBA_results_2018_Fastball <- wOBA_predict_data_2018_Fastball %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2018_Fastball <- wOBA_predict_data_2018_Fastball %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2018_Fastball %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2018_Fastball <- bip_with_predicted_2018_Fastball %>%
  filter(pitcher %in% countlist)

model_wOBA_2018_Fastball <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2018_Fastball)

woba_pitchers_2018_Fastball <- data.frame(ranef(model_wOBA_2018_Fastball)$pitcher)
colnames(woba_pitchers_2018_Fastball) <- "In_wOBA"

wOBA_results_2018_Fastball <- merge(wOBA_results_2018_Fastball,woba_pitchers_2018_Fastball, by.x = "pitcher", by.y = "row.names")

###2017 wOBA Fastball ----
wOBA_predict_data_2017_Fastball <- wOBA_predict_data_2017 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

wOBA_results_2017_Fastball <- wOBA_predict_data_2017_Fastball %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2017_Fastball <- wOBA_predict_data_2017_Fastball %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2017_Fastball %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2017_Fastball <- bip_with_predicted_2017_Fastball %>%
  filter(pitcher %in% countlist)

model_wOBA_2017_Fastball <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2017_Fastball)

woba_pitchers_2017_Fastball <- data.frame(ranef(model_wOBA_2017_Fastball)$pitcher)
colnames(woba_pitchers_2017_Fastball) <- "In_wOBA"

wOBA_results_2017_Fastball <- merge(wOBA_results_2017_Fastball,woba_pitchers_2017_Fastball, by.x = "pitcher", by.y = "row.names")

###2016 wOBA Fastball ----
wOBA_predict_data_2016_Fastball <- wOBA_predict_data_2016 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

wOBA_results_2016_Fastball <- wOBA_predict_data_2016_Fastball %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2016_Fastball <- wOBA_predict_data_2016_Fastball %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2016_Fastball %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2016_Fastball <- bip_with_predicted_2016_Fastball %>%
  filter(pitcher %in% countlist)


model_wOBA_2016_Fastball <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2016_Fastball)

woba_pitchers_2016_Fastball <- data.frame(ranef(model_wOBA_2016_Fastball)$pitcher)
colnames(woba_pitchers_2016_Fastball) <- "In_wOBA"

wOBA_results_2016_Fastball <- merge(wOBA_results_2016_Fastball,woba_pitchers_2016_Fastball, by.x = "pitcher", by.y = "row.names")

###2015 wOBA Fastball ----
wOBA_predict_data_2015_Fastball <- wOBA_predict_data_2015 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

wOBA_results_2015_Fastball <- wOBA_predict_data_2015_Fastball %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_2015_Fastball <- wOBA_predict_data_2015_Fastball %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

count <- bip_with_predicted_2015_Fastball %>% 
  group_by(pitcher) %>%
  summarise(Num = n()) %>%
  filter(Num > 1)

countlist <- as.list(count$pitcher)

bip_with_predicted_2015_Fastball <- bip_with_predicted_2015_Fastball %>%
  filter(pitcher %in% countlist)


model_wOBA_2015_Fastball <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_2015_Fastball)

woba_pitchers_2015_Fastball <- data.frame(ranef(model_wOBA_2015_Fastball)$pitcher)
colnames(woba_pitchers_2015_Fastball) <- "In_wOBA"

wOBA_results_2015_Fastball <- merge(wOBA_results_2015_Fastball,woba_pitchers_2015_Fastball, by.x = "pitcher", by.y = "row.names")

wOBA_results_all_Fastball <- rbind(wOBA_results_2015_Fastball,wOBA_results_2016_Fastball,wOBA_results_2017_Fastball,wOBA_results_2018_Fastball,wOBA_results_2019_Fastball)

wOBA_results_all_Fastball$pitch_type <- "Fastball"

wOBA_results_all_pitch_type <- rbind(wOBA_results_all_Breaking,wOBA_results_all_Fastball,wOBA_results_all_OffSpeed)

### 2019 Breaking----
all_breaking_2019 <- all_data_2019 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

in_zone_breaking_2019 <- all_breaking_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_breaking_2019 <- all_breaking_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_breaking_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                               data = in_zone_breaking_2019)

model_out_of_zone_breaking_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                      data = out_of_zone_breaking_2019)

out_of_zone_pitchers_breaking_2019 <- data.frame(ranef(model_out_of_zone_breaking_2019)$pitcher)
colnames(out_of_zone_pitchers_breaking_2019) <- "OOZ"

in_zone_pitchers_breaking_2019 <- data.frame(ranef(model_zone_breaking_2019)$pitcher)
colnames(in_zone_pitchers_breaking_2019) <- "IZ"

all_results_breaking_2019 <- merge(in_zone_pitchers_breaking_2019,out_of_zone_pitchers_breaking_2019,by = "row.names")
colnames(all_results_breaking_2019) <- c("pitcher","IZ","OOZ")
all_results_breaking_2019$pitcher <- as.numeric(all_results_breaking_2019$pitcher)
all_results_breaking_2019$Season <- 2019

in_zone_data_breaking_2019 <- in_zone_breaking_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_breaking_2019$Season <- 2019

out_of_zone_data_breaking_2019 <- out_of_zone_breaking_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_breaking_2019$Season <- 2019

whiff_predict_data_breaking_2019 <- whiff_predict_data_2019 %>% filter(pitch_type %in% c("SL","CU","KC"))
model_whiff_breaking_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                data = whiff_predict_data_breaking_2019)

whiff_pitchers_breaking_2019 <- data.frame(ranef(model_whiff_breaking_2019)$pitcher)
colnames(whiff_pitchers_breaking_2019) <- "In_Whiff"

whiff_data_breaking_2019 <- whiff_predict_data_breaking_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_breaking_2019 <- merge(whiff_data_breaking_2019,whiff_pitchers_breaking_2019, by.x = "pitcher", by.y = "row.names")

### 2018 Breaking----
all_breaking_2018 <- all_data_2018 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

in_zone_breaking_2018 <- all_breaking_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_breaking_2018 <- all_breaking_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_breaking_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_breaking_2018)

model_out_of_zone_breaking_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_breaking_2018)

out_of_zone_pitchers_breaking_2018 <- data.frame(ranef(model_out_of_zone_breaking_2018)$pitcher)
colnames(out_of_zone_pitchers_breaking_2018) <- "OOZ"

in_zone_pitchers_breaking_2018 <- data.frame(ranef(model_zone_breaking_2018)$pitcher)
colnames(in_zone_pitchers_breaking_2018) <- "IZ"

all_results_breaking_2018 <- merge(in_zone_pitchers_breaking_2018,out_of_zone_pitchers_breaking_2018,by = "row.names")
colnames(all_results_breaking_2018) <- c("pitcher","IZ","OOZ")
all_results_breaking_2018$pitcher <- as.numeric(all_results_breaking_2018$pitcher)
all_results_breaking_2018$Season <- 2018

in_zone_data_breaking_2018 <- in_zone_breaking_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_breaking_2018$Season <- 2018

out_of_zone_data_breaking_2018 <- out_of_zone_breaking_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_breaking_2018$Season <- 2018

whiff_predict_data_breaking_2018 <- whiff_predict_data_2018 %>% filter(pitch_type %in% c("SL","CU","KC"))
model_whiff_breaking_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_breaking_2018)

whiff_pitchers_breaking_2018 <- data.frame(ranef(model_whiff_breaking_2018)$pitcher)
colnames(whiff_pitchers_breaking_2018) <- "In_Whiff"

whiff_data_breaking_2018 <- whiff_predict_data_breaking_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_breaking_2018 <- merge(whiff_data_breaking_2018,whiff_pitchers_breaking_2018, by.x = "pitcher", by.y = "row.names")

### 2017 Breaking----
all_breaking_2017 <- all_data_2017 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

in_zone_breaking_2017 <- all_breaking_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_breaking_2017 <- all_breaking_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_breaking_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_breaking_2017)

model_out_of_zone_breaking_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_breaking_2017)

out_of_zone_pitchers_breaking_2017 <- data.frame(ranef(model_out_of_zone_breaking_2017)$pitcher)
colnames(out_of_zone_pitchers_breaking_2017) <- "OOZ"

in_zone_pitchers_breaking_2017 <- data.frame(ranef(model_zone_breaking_2017)$pitcher)
colnames(in_zone_pitchers_breaking_2017) <- "IZ"

all_results_breaking_2017 <- merge(in_zone_pitchers_breaking_2017,out_of_zone_pitchers_breaking_2017,by = "row.names")
colnames(all_results_breaking_2017) <- c("pitcher","IZ","OOZ")
all_results_breaking_2017$pitcher <- as.numeric(all_results_breaking_2017$pitcher)
all_results_breaking_2017$Season <- 2017

in_zone_data_breaking_2017 <- in_zone_breaking_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_breaking_2017$Season <- 2017

out_of_zone_data_breaking_2017 <- out_of_zone_breaking_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_breaking_2017$Season <- 2017

whiff_predict_data_breaking_2017 <- whiff_predict_data_2017 %>% filter(pitch_type %in% c("SL","CU","KC"))
model_whiff_breaking_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_breaking_2017)

whiff_pitchers_breaking_2017 <- data.frame(ranef(model_whiff_breaking_2017)$pitcher)
colnames(whiff_pitchers_breaking_2017) <- "In_Whiff"

whiff_data_breaking_2017 <- whiff_predict_data_breaking_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_breaking_2017 <- merge(whiff_data_breaking_2017,whiff_pitchers_breaking_2017, by.x = "pitcher", by.y = "row.names")

### 2016 Breaking----
all_breaking_2016 <- all_data_2016 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

in_zone_breaking_2016 <- all_breaking_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_breaking_2016 <- all_breaking_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_breaking_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_breaking_2016)

model_out_of_zone_breaking_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_breaking_2016)

out_of_zone_pitchers_breaking_2016 <- data.frame(ranef(model_out_of_zone_breaking_2016)$pitcher)
colnames(out_of_zone_pitchers_breaking_2016) <- "OOZ"

in_zone_pitchers_breaking_2016 <- data.frame(ranef(model_zone_breaking_2016)$pitcher)
colnames(in_zone_pitchers_breaking_2016) <- "IZ"

all_results_breaking_2016 <- merge(in_zone_pitchers_breaking_2016,out_of_zone_pitchers_breaking_2016,by = "row.names")
colnames(all_results_breaking_2016) <- c("pitcher","IZ","OOZ")
all_results_breaking_2016$pitcher <- as.numeric(all_results_breaking_2016$pitcher)
all_results_breaking_2016$Season <- 2016

in_zone_data_breaking_2016 <- in_zone_breaking_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_breaking_2016$Season <- 2016

out_of_zone_data_breaking_2016 <- out_of_zone_breaking_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_breaking_2016$Season <- 2016

whiff_predict_data_breaking_2016 <- whiff_predict_data_2016 %>% filter(pitch_type %in% c("SL","CU","KC"))
model_whiff_breaking_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_breaking_2016)

whiff_pitchers_breaking_2016 <- data.frame(ranef(model_whiff_breaking_2016)$pitcher)
colnames(whiff_pitchers_breaking_2016) <- "In_Whiff"

whiff_data_breaking_2016 <- whiff_predict_data_breaking_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_breaking_2016 <- merge(whiff_data_breaking_2016,whiff_pitchers_breaking_2016, by.x = "pitcher", by.y = "row.names")

### 2015 Breaking----
all_breaking_2015 <- all_data_2015 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

in_zone_breaking_2015 <- all_breaking_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_breaking_2015 <- all_breaking_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_breaking_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_breaking_2015)

model_out_of_zone_breaking_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_breaking_2015)

out_of_zone_pitchers_breaking_2015 <- data.frame(ranef(model_out_of_zone_breaking_2015)$pitcher)
colnames(out_of_zone_pitchers_breaking_2015) <- "OOZ"

in_zone_pitchers_breaking_2015 <- data.frame(ranef(model_zone_breaking_2015)$pitcher)
colnames(in_zone_pitchers_breaking_2015) <- "IZ"

all_results_breaking_2015 <- merge(in_zone_pitchers_breaking_2015,out_of_zone_pitchers_breaking_2015,by = "row.names")
colnames(all_results_breaking_2015) <- c("pitcher","IZ","OOZ")
all_results_breaking_2015$pitcher <- as.numeric(all_results_breaking_2015$pitcher)
all_results_breaking_2015$Season <- 2015

in_zone_data_breaking_2015 <- in_zone_breaking_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_breaking_2015$Season <- 2015

out_of_zone_data_breaking_2015 <- out_of_zone_breaking_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_breaking_2015$Season <- 2015

whiff_predict_data_breaking_2015 <- whiff_predict_data_2015 %>% filter(pitch_type %in% c("SL","CU","KC"))
model_whiff_breaking_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_breaking_2015)

whiff_pitchers_breaking_2015 <- data.frame(ranef(model_whiff_breaking_2015)$pitcher)
colnames(whiff_pitchers_breaking_2015) <- "In_Whiff"

whiff_data_breaking_2015 <- whiff_predict_data_breaking_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_breaking_2015 <- merge(whiff_data_breaking_2015,whiff_pitchers_breaking_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_breaking_all <- rbind(out_of_zone_data_breaking_2015,out_of_zone_data_breaking_2016,out_of_zone_data_breaking_2017,out_of_zone_data_breaking_2018,out_of_zone_data_2019)
in_zone_data_breaking_all <- rbind(in_zone_data_breaking_2015,in_zone_data_breaking_2016,in_zone_data_breaking_2017,in_zone_data_breaking_2018,in_zone_data_breaking_2019)
all_results_breaking_all <- rbind(all_results_breaking_2015,all_results_breaking_2016,all_results_breaking_2017,all_results_breaking_2018,all_results_breaking_2019)
all_whiffs_breaking_all <- rbind(whiff_data_breaking_2015,whiff_data_breaking_2016,whiff_data_breaking_2017,whiff_data_breaking_2018,whiff_data_breaking_2019)

plate_discipline_breaking_all <- merge(all_whiffs_breaking_all,out_of_zone_data_breaking_all,by = c("pitcher","Season"))
plate_discipline_breaking_all <- merge(plate_discipline_breaking_all,in_zone_data_breaking_all,by = c("pitcher","Season"))
plate_discipline_breaking_all <- merge(plate_discipline_breaking_all,all_results_breaking_all,by = c("pitcher","Season"))

plate_discipline_breaking_all <- plate_discipline_breaking_all[,c("pitcher",
                                                              "player_name",
                                                              "Season",
                                                              "Pitches",
                                                              "Whiff",
                                                              "xWhiff",
                                                              "In_Whiff",
                                                              "OOZ.Pitches",
                                                              "OOZ.Swing",
                                                              "OOZ.xSwing",
                                                              "OOZ",
                                                              "IZ.Pitches",
                                                              "IZ.Swing",
                                                              "IZ.xSwing",
                                                              "IZ")]

plate_discipline_breaking_all$pitch_type <- "Breaking"

### 2019 Offspeed----
all_Offspeed_2019 <- all_data_2019 %>% 
  filter(pitch_type %in% c("CH","FS"))

in_zone_Offspeed_2019 <- all_Offspeed_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Offspeed_2019 <- all_Offspeed_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Offspeed_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Offspeed_2019)

model_out_of_zone_Offspeed_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Offspeed_2019)

out_of_zone_pitchers_Offspeed_2019 <- data.frame(ranef(model_out_of_zone_Offspeed_2019)$pitcher)
colnames(out_of_zone_pitchers_Offspeed_2019) <- "OOZ"

in_zone_pitchers_Offspeed_2019 <- data.frame(ranef(model_zone_Offspeed_2019)$pitcher)
colnames(in_zone_pitchers_Offspeed_2019) <- "IZ"

all_results_Offspeed_2019 <- merge(in_zone_pitchers_Offspeed_2019,out_of_zone_pitchers_Offspeed_2019,by = "row.names")
colnames(all_results_Offspeed_2019) <- c("pitcher","IZ","OOZ")
all_results_Offspeed_2019$pitcher <- as.numeric(all_results_Offspeed_2019$pitcher)
all_results_Offspeed_2019$Season <- 2019

in_zone_data_Offspeed_2019 <- in_zone_Offspeed_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Offspeed_2019$Season <- 2019

out_of_zone_data_Offspeed_2019 <- out_of_zone_Offspeed_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Offspeed_2019$Season <- 2019

whiff_predict_data_Offspeed_2019 <- whiff_predict_data_2019 %>% filter(pitch_type %in% c("CH","FS"))
model_whiff_Offspeed_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Offspeed_2019)

whiff_pitchers_Offspeed_2019 <- data.frame(ranef(model_whiff_Offspeed_2019)$pitcher)
colnames(whiff_pitchers_Offspeed_2019) <- "In_Whiff"

whiff_data_Offspeed_2019 <- whiff_predict_data_Offspeed_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Offspeed_2019 <- merge(whiff_data_Offspeed_2019,whiff_pitchers_Offspeed_2019, by.x = "pitcher", by.y = "row.names")

### 2018 Offspeed----
all_Offspeed_2018 <- all_data_2018 %>% 
  filter(pitch_type %in% c("CH","FS"))

in_zone_Offspeed_2018 <- all_Offspeed_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Offspeed_2018 <- all_Offspeed_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Offspeed_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Offspeed_2018)

model_out_of_zone_Offspeed_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Offspeed_2018)

out_of_zone_pitchers_Offspeed_2018 <- data.frame(ranef(model_out_of_zone_Offspeed_2018)$pitcher)
colnames(out_of_zone_pitchers_Offspeed_2018) <- "OOZ"

in_zone_pitchers_Offspeed_2018 <- data.frame(ranef(model_zone_Offspeed_2018)$pitcher)
colnames(in_zone_pitchers_Offspeed_2018) <- "IZ"

all_results_Offspeed_2018 <- merge(in_zone_pitchers_Offspeed_2018,out_of_zone_pitchers_Offspeed_2018,by = "row.names")
colnames(all_results_Offspeed_2018) <- c("pitcher","IZ","OOZ")
all_results_Offspeed_2018$pitcher <- as.numeric(all_results_Offspeed_2018$pitcher)
all_results_Offspeed_2018$Season <- 2018

in_zone_data_Offspeed_2018 <- in_zone_Offspeed_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Offspeed_2018$Season <- 2018

out_of_zone_data_Offspeed_2018 <- out_of_zone_Offspeed_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Offspeed_2018$Season <- 2018

whiff_predict_data_Offspeed_2018 <- whiff_predict_data_2018 %>% filter(pitch_type %in% c("CH","FS"))
model_whiff_Offspeed_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Offspeed_2018)

whiff_pitchers_Offspeed_2018 <- data.frame(ranef(model_whiff_Offspeed_2018)$pitcher)
colnames(whiff_pitchers_Offspeed_2018) <- "In_Whiff"

whiff_data_Offspeed_2018 <- whiff_predict_data_Offspeed_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Offspeed_2018 <- merge(whiff_data_Offspeed_2018,whiff_pitchers_Offspeed_2018, by.x = "pitcher", by.y = "row.names")

### 2017 Offspeed----
all_Offspeed_2017 <- all_data_2017 %>% 
  filter(pitch_type %in% c("CH","FS"))

in_zone_Offspeed_2017 <- all_Offspeed_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Offspeed_2017 <- all_Offspeed_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Offspeed_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Offspeed_2017)

model_out_of_zone_Offspeed_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Offspeed_2017)

out_of_zone_pitchers_Offspeed_2017 <- data.frame(ranef(model_out_of_zone_Offspeed_2017)$pitcher)
colnames(out_of_zone_pitchers_Offspeed_2017) <- "OOZ"

in_zone_pitchers_Offspeed_2017 <- data.frame(ranef(model_zone_Offspeed_2017)$pitcher)
colnames(in_zone_pitchers_Offspeed_2017) <- "IZ"

all_results_Offspeed_2017 <- merge(in_zone_pitchers_Offspeed_2017,out_of_zone_pitchers_Offspeed_2017,by = "row.names")
colnames(all_results_Offspeed_2017) <- c("pitcher","IZ","OOZ")
all_results_Offspeed_2017$pitcher <- as.numeric(all_results_Offspeed_2017$pitcher)
all_results_Offspeed_2017$Season <- 2017

in_zone_data_Offspeed_2017 <- in_zone_Offspeed_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Offspeed_2017$Season <- 2017

out_of_zone_data_Offspeed_2017 <- out_of_zone_Offspeed_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Offspeed_2017$Season <- 2017

whiff_predict_data_Offspeed_2017 <- whiff_predict_data_2017 %>% filter(pitch_type %in% c("CH","FS"))
model_whiff_Offspeed_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Offspeed_2017)

whiff_pitchers_Offspeed_2017 <- data.frame(ranef(model_whiff_Offspeed_2017)$pitcher)
colnames(whiff_pitchers_Offspeed_2017) <- "In_Whiff"

whiff_data_Offspeed_2017 <- whiff_predict_data_Offspeed_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Offspeed_2017 <- merge(whiff_data_Offspeed_2017,whiff_pitchers_Offspeed_2017, by.x = "pitcher", by.y = "row.names")

### 2016 Offspeed----
all_Offspeed_2016 <- all_data_2016 %>% 
  filter(pitch_type %in% c("SL","CU","KC"))

in_zone_Offspeed_2016 <- all_Offspeed_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Offspeed_2016 <- all_Offspeed_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Offspeed_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Offspeed_2016)

model_out_of_zone_Offspeed_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Offspeed_2016)

out_of_zone_pitchers_Offspeed_2016 <- data.frame(ranef(model_out_of_zone_Offspeed_2016)$pitcher)
colnames(out_of_zone_pitchers_Offspeed_2016) <- "OOZ"

in_zone_pitchers_Offspeed_2016 <- data.frame(ranef(model_zone_Offspeed_2016)$pitcher)
colnames(in_zone_pitchers_Offspeed_2016) <- "IZ"

all_results_Offspeed_2016 <- merge(in_zone_pitchers_Offspeed_2016,out_of_zone_pitchers_Offspeed_2016,by = "row.names")
colnames(all_results_Offspeed_2016) <- c("pitcher","IZ","OOZ")
all_results_Offspeed_2016$pitcher <- as.numeric(all_results_Offspeed_2016$pitcher)
all_results_Offspeed_2016$Season <- 2016

in_zone_data_Offspeed_2016 <- in_zone_Offspeed_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Offspeed_2016$Season <- 2016

out_of_zone_data_Offspeed_2016 <- out_of_zone_Offspeed_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Offspeed_2016$Season <- 2016

whiff_predict_data_Offspeed_2016 <- whiff_predict_data_2016 %>% filter(pitch_type %in% c("CH","FS"))
model_whiff_Offspeed_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Offspeed_2016)

whiff_pitchers_Offspeed_2016 <- data.frame(ranef(model_whiff_Offspeed_2016)$pitcher)
colnames(whiff_pitchers_Offspeed_2016) <- "In_Whiff"

whiff_data_Offspeed_2016 <- whiff_predict_data_Offspeed_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Offspeed_2016 <- merge(whiff_data_Offspeed_2016,whiff_pitchers_Offspeed_2016, by.x = "pitcher", by.y = "row.names")

### 2015 Offspeed----
all_Offspeed_2015 <- all_data_2015 %>% 
  filter(pitch_type %in% c("CH","FS"))

in_zone_Offspeed_2015 <- all_Offspeed_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Offspeed_2015 <- all_Offspeed_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Offspeed_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Offspeed_2015)

model_out_of_zone_Offspeed_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Offspeed_2015)

out_of_zone_pitchers_Offspeed_2015 <- data.frame(ranef(model_out_of_zone_Offspeed_2015)$pitcher)
colnames(out_of_zone_pitchers_Offspeed_2015) <- "OOZ"

in_zone_pitchers_Offspeed_2015 <- data.frame(ranef(model_zone_Offspeed_2015)$pitcher)
colnames(in_zone_pitchers_Offspeed_2015) <- "IZ"

all_results_Offspeed_2015 <- merge(in_zone_pitchers_Offspeed_2015,out_of_zone_pitchers_Offspeed_2015,by = "row.names")
colnames(all_results_Offspeed_2015) <- c("pitcher","IZ","OOZ")
all_results_Offspeed_2015$pitcher <- as.numeric(all_results_Offspeed_2015$pitcher)
all_results_Offspeed_2015$Season <- 2015

in_zone_data_Offspeed_2015 <- in_zone_Offspeed_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Offspeed_2015$Season <- 2015

out_of_zone_data_Offspeed_2015 <- out_of_zone_Offspeed_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Offspeed_2015$Season <- 2015

whiff_predict_data_Offspeed_2015 <- whiff_predict_data_2015 %>% filter(pitch_type %in% c("CH","FS"))
model_whiff_Offspeed_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Offspeed_2015)

whiff_pitchers_Offspeed_2015 <- data.frame(ranef(model_whiff_Offspeed_2015)$pitcher)
colnames(whiff_pitchers_Offspeed_2015) <- "In_Whiff"

whiff_data_Offspeed_2015 <- whiff_predict_data_Offspeed_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Offspeed_2015 <- merge(whiff_data_Offspeed_2015,whiff_pitchers_Offspeed_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_Offspeed_all <- rbind(out_of_zone_data_Offspeed_2015,out_of_zone_data_Offspeed_2016,out_of_zone_data_Offspeed_2017,out_of_zone_data_Offspeed_2018,out_of_zone_data_2019)
in_zone_data_Offspeed_all <- rbind(in_zone_data_Offspeed_2015,in_zone_data_Offspeed_2016,in_zone_data_Offspeed_2017,in_zone_data_Offspeed_2018,in_zone_data_Offspeed_2019)
all_results_Offspeed_all <- rbind(all_results_Offspeed_2015,all_results_Offspeed_2016,all_results_Offspeed_2017,all_results_Offspeed_2018,all_results_Offspeed_2019)
all_whiffs_Offspeed_all <- rbind(whiff_data_Offspeed_2015,whiff_data_Offspeed_2016,whiff_data_Offspeed_2017,whiff_data_Offspeed_2018,whiff_data_Offspeed_2019)

plate_discipline_Offspeed_all <- merge(all_whiffs_Offspeed_all,out_of_zone_data_Offspeed_all,by = c("pitcher","Season"))
plate_discipline_Offspeed_all <- merge(plate_discipline_Offspeed_all,in_zone_data_Offspeed_all,by = c("pitcher","Season"))
plate_discipline_Offspeed_all <- merge(plate_discipline_Offspeed_all,all_results_Offspeed_all,by = c("pitcher","Season"))

plate_discipline_Offspeed_all <- plate_discipline_Offspeed_all[,c("pitcher",
                                                                  "player_name",
                                                                  "Season",
                                                                  "Pitches",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "OOZ.Pitches",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "IZ.Pitches",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ")]

plate_discipline_Offspeed_all$pitch_type <- "Offspeed"

### 2019 Fastball----
all_Fastball_2019 <- all_data_2019 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

in_zone_Fastball_2019 <- all_Fastball_2019 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Fastball_2019 <- all_Fastball_2019 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Fastball_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Fastball_2019)

model_out_of_zone_Fastball_2019 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Fastball_2019)

out_of_zone_pitchers_Fastball_2019 <- data.frame(ranef(model_out_of_zone_Fastball_2019)$pitcher)
colnames(out_of_zone_pitchers_Fastball_2019) <- "OOZ"

in_zone_pitchers_Fastball_2019 <- data.frame(ranef(model_zone_Fastball_2019)$pitcher)
colnames(in_zone_pitchers_Fastball_2019) <- "IZ"

all_results_Fastball_2019 <- merge(in_zone_pitchers_Fastball_2019,out_of_zone_pitchers_Fastball_2019,by = "row.names")
colnames(all_results_Fastball_2019) <- c("pitcher","IZ","OOZ")
all_results_Fastball_2019$pitcher <- as.numeric(all_results_Fastball_2019$pitcher)
all_results_Fastball_2019$Season <- 2019

in_zone_data_Fastball_2019 <- in_zone_Fastball_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Fastball_2019$Season <- 2019

out_of_zone_data_Fastball_2019 <- out_of_zone_Fastball_2019 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Fastball_2019$Season <- 2019

whiff_predict_data_Fastball_2019 <- whiff_predict_data_2019 %>% filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))
model_whiff_Fastball_2019 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Fastball_2019)

whiff_pitchers_Fastball_2019 <- data.frame(ranef(model_whiff_Fastball_2019)$pitcher)
colnames(whiff_pitchers_Fastball_2019) <- "In_Whiff"

whiff_data_Fastball_2019 <- whiff_predict_data_Fastball_2019 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Fastball_2019 <- merge(whiff_data_Fastball_2019,whiff_pitchers_Fastball_2019, by.x = "pitcher", by.y = "row.names")

### 2018 Fastball----
all_Fastball_2018 <- all_data_2018 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

in_zone_Fastball_2018 <- all_Fastball_2018 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Fastball_2018 <- all_Fastball_2018 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Fastball_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Fastball_2018)

model_out_of_zone_Fastball_2018 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Fastball_2018)

out_of_zone_pitchers_Fastball_2018 <- data.frame(ranef(model_out_of_zone_Fastball_2018)$pitcher)
colnames(out_of_zone_pitchers_Fastball_2018) <- "OOZ"

in_zone_pitchers_Fastball_2018 <- data.frame(ranef(model_zone_Fastball_2018)$pitcher)
colnames(in_zone_pitchers_Fastball_2018) <- "IZ"

all_results_Fastball_2018 <- merge(in_zone_pitchers_Fastball_2018,out_of_zone_pitchers_Fastball_2018,by = "row.names")
colnames(all_results_Fastball_2018) <- c("pitcher","IZ","OOZ")
all_results_Fastball_2018$pitcher <- as.numeric(all_results_Fastball_2018$pitcher)
all_results_Fastball_2018$Season <- 2018

in_zone_data_Fastball_2018 <- in_zone_Fastball_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Fastball_2018$Season <- 2018

out_of_zone_data_Fastball_2018 <- out_of_zone_Fastball_2018 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Fastball_2018$Season <- 2018

whiff_predict_data_Fastball_2018 <- whiff_predict_data_2018 %>% filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))
model_whiff_Fastball_2018 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Fastball_2018)

whiff_pitchers_Fastball_2018 <- data.frame(ranef(model_whiff_Fastball_2018)$pitcher)
colnames(whiff_pitchers_Fastball_2018) <- "In_Whiff"

whiff_data_Fastball_2018 <- whiff_predict_data_Fastball_2018 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Fastball_2018 <- merge(whiff_data_Fastball_2018,whiff_pitchers_Fastball_2018, by.x = "pitcher", by.y = "row.names")

### 2017 Fastball----
all_Fastball_2017 <- all_data_2017 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

in_zone_Fastball_2017 <- all_Fastball_2017 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Fastball_2017 <- all_Fastball_2017 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Fastball_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Fastball_2017)

model_out_of_zone_Fastball_2017 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Fastball_2017)

out_of_zone_pitchers_Fastball_2017 <- data.frame(ranef(model_out_of_zone_Fastball_2017)$pitcher)
colnames(out_of_zone_pitchers_Fastball_2017) <- "OOZ"

in_zone_pitchers_Fastball_2017 <- data.frame(ranef(model_zone_Fastball_2017)$pitcher)
colnames(in_zone_pitchers_Fastball_2017) <- "IZ"

all_results_Fastball_2017 <- merge(in_zone_pitchers_Fastball_2017,out_of_zone_pitchers_Fastball_2017,by = "row.names")
colnames(all_results_Fastball_2017) <- c("pitcher","IZ","OOZ")
all_results_Fastball_2017$pitcher <- as.numeric(all_results_Fastball_2017$pitcher)
all_results_Fastball_2017$Season <- 2017

in_zone_data_Fastball_2017 <- in_zone_Fastball_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Fastball_2017$Season <- 2017

out_of_zone_data_Fastball_2017 <- out_of_zone_Fastball_2017 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Fastball_2017$Season <- 2017

whiff_predict_data_Fastball_2017 <- whiff_predict_data_2017 %>% filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))
model_whiff_Fastball_2017 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Fastball_2017)

whiff_pitchers_Fastball_2017 <- data.frame(ranef(model_whiff_Fastball_2017)$pitcher)
colnames(whiff_pitchers_Fastball_2017) <- "In_Whiff"

whiff_data_Fastball_2017 <- whiff_predict_data_Fastball_2017 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Fastball_2017 <- merge(whiff_data_Fastball_2017,whiff_pitchers_Fastball_2017, by.x = "pitcher", by.y = "row.names")

### 2016 Fastball----
all_Fastball_2016 <- all_data_2016 %>% 
  filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))

in_zone_Fastball_2016 <- all_Fastball_2016 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Fastball_2016 <- all_Fastball_2016 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Fastball_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Fastball_2016)

model_out_of_zone_Fastball_2016 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Fastball_2016)

out_of_zone_pitchers_Fastball_2016 <- data.frame(ranef(model_out_of_zone_Fastball_2016)$pitcher)
colnames(out_of_zone_pitchers_Fastball_2016) <- "OOZ"

in_zone_pitchers_Fastball_2016 <- data.frame(ranef(model_zone_Fastball_2016)$pitcher)
colnames(in_zone_pitchers_Fastball_2016) <- "IZ"

all_results_Fastball_2016 <- merge(in_zone_pitchers_Fastball_2016,out_of_zone_pitchers_Fastball_2016,by = "row.names")
colnames(all_results_Fastball_2016) <- c("pitcher","IZ","OOZ")
all_results_Fastball_2016$pitcher <- as.numeric(all_results_Fastball_2016$pitcher)
all_results_Fastball_2016$Season <- 2016

in_zone_data_Fastball_2016 <- in_zone_Fastball_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Fastball_2016$Season <- 2016

out_of_zone_data_Fastball_2016 <- out_of_zone_Fastball_2016 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Fastball_2016$Season <- 2016

whiff_predict_data_Fastball_2016 <- whiff_predict_data_2016 %>% filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))
model_whiff_Fastball_2016 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Fastball_2016)

whiff_pitchers_Fastball_2016 <- data.frame(ranef(model_whiff_Fastball_2016)$pitcher)
colnames(whiff_pitchers_Fastball_2016) <- "In_Whiff"

whiff_data_Fastball_2016 <- whiff_predict_data_Fastball_2016 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Fastball_2016 <- merge(whiff_data_Fastball_2016,whiff_pitchers_Fastball_2016, by.x = "pitcher", by.y = "row.names")

### 2015 Fastball----
all_Fastball_2015 <- all_data_2015 %>% 
  filter(pitch_type %in% c("FF","FT","FC", "SI", "FA"))

in_zone_Fastball_2015 <- all_Fastball_2015 %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Fastball_2015 <- all_Fastball_2015 %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Fastball_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Fastball_2015)

model_out_of_zone_Fastball_2015 <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Fastball_2015)

out_of_zone_pitchers_Fastball_2015 <- data.frame(ranef(model_out_of_zone_Fastball_2015)$pitcher)
colnames(out_of_zone_pitchers_Fastball_2015) <- "OOZ"

in_zone_pitchers_Fastball_2015 <- data.frame(ranef(model_zone_Fastball_2015)$pitcher)
colnames(in_zone_pitchers_Fastball_2015) <- "IZ"

all_results_Fastball_2015 <- merge(in_zone_pitchers_Fastball_2015,out_of_zone_pitchers_Fastball_2015,by = "row.names")
colnames(all_results_Fastball_2015) <- c("pitcher","IZ","OOZ")
all_results_Fastball_2015$pitcher <- as.numeric(all_results_Fastball_2015$pitcher)
all_results_Fastball_2015$Season <- 2015

in_zone_data_Fastball_2015 <- in_zone_Fastball_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)
in_zone_data_Fastball_2015$Season <- 2015

out_of_zone_data_Fastball_2015 <- out_of_zone_Fastball_2015 %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)
out_of_zone_data_Fastball_2015$Season <- 2015

whiff_predict_data_Fastball_2015 <- whiff_predict_data_2015 %>%   filter(pitch_type %in% c("FF", "FT","FC", "SI", "FA"))
model_whiff_Fastball_2015 <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Fastball_2015)

whiff_pitchers_Fastball_2015 <- data.frame(ranef(model_whiff_Fastball_2015)$pitcher)
colnames(whiff_pitchers_Fastball_2015) <- "In_Whiff"

whiff_data_Fastball_2015 <- whiff_predict_data_Fastball_2015 %>%
  group_by(pitcher, player_name, Season) %>% 
  arrange(pitcher, player_name,Season) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Fastball_2015 <- merge(whiff_data_Fastball_2015,whiff_pitchers_Fastball_2015, by.x = "pitcher", by.y = "row.names")

out_of_zone_data_Fastball_all <- rbind(out_of_zone_data_Fastball_2015,out_of_zone_data_Fastball_2016,out_of_zone_data_Fastball_2017,out_of_zone_data_Fastball_2018,out_of_zone_data_2019)
in_zone_data_Fastball_all <- rbind(in_zone_data_Fastball_2015,in_zone_data_Fastball_2016,in_zone_data_Fastball_2017,in_zone_data_Fastball_2018,in_zone_data_Fastball_2019)
all_results_Fastball_all <- rbind(all_results_Fastball_2015,all_results_Fastball_2016,all_results_Fastball_2017,all_results_Fastball_2018,all_results_Fastball_2019)
all_whiffs_Fastball_all <- rbind(whiff_data_Fastball_2015,whiff_data_Fastball_2016,whiff_data_Fastball_2017,whiff_data_Fastball_2018,whiff_data_Fastball_2019)

plate_discipline_Fastball_all <- merge(all_whiffs_Fastball_all,out_of_zone_data_Fastball_all,by = c("pitcher","Season"))
plate_discipline_Fastball_all <- merge(plate_discipline_Fastball_all,in_zone_data_Fastball_all,by = c("pitcher","Season"))
plate_discipline_Fastball_all <- merge(plate_discipline_Fastball_all,all_results_Fastball_all,by = c("pitcher","Season"))

plate_discipline_Fastball_all <- plate_discipline_Fastball_all[,c("pitcher",
                                                                  "player_name",
                                                                  "Season",
                                                                  "Pitches",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "OOZ.Pitches",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "IZ.Pitches",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ")]

plate_discipline_Fastball_all$pitch_type <- "Fastball"

plate_discipline_all_pitch_group <- rbind(plate_discipline_breaking_all,
                                         plate_discipline_Fastball_all,
                                         plate_discipline_Offspeed_all)

plate_discipline_all_pitch_group <- plate_discipline_all_pitch_group[,c("pitcher",
                                                                      "player_name",
                                                                      "Season",
                                                                      "pitch_type",
                                                                      "Pitches",
                                                                      "Whiff",
                                                                      "xWhiff",
                                                                      "In_Whiff",
                                                                      "OOZ.Pitches",
                                                                      "OOZ.Swing",
                                                                      "OOZ.xSwing",
                                                                      "OOZ",
                                                                      "IZ.Pitches",
                                                                      "IZ.Swing",
                                                                      "IZ.xSwing",
                                                                      "IZ")]

plate_discipline_all_pitch_group <- left_join(plate_discipline_all_pitch_group,wOBA_results_all_pitch_type[,c(1,3,5,6,7,8)], by = c("pitcher","Season","pitch_type"))


in_zone_avg_pitch_group <- in_zone %>%
  group_by(pitch_group) %>%
  summarise(
    avg = mean(predict),
    sd = sd(predict)
  )

out_of_zone_avg_pitch_group <- out_of_zone %>%
  group_by(pitch_group) %>%
  summarise(
    avg = mean(predict),
    sd = sd(predict)
  )

whiff_avg_pitch_group <- whiff_predict_data %>%
  group_by(pitch_group) %>%
  summarise(
    avg = mean(predict_whiff),
    sd = sd(predict_whiff)
  )

woba_avg_pitch_group <- wOBA_predict_data %>%
  group_by(pitch_group) %>%
  summarise(
    avg = mean(predict_wOBA),
    sd = sd(predict_wOBA)
  )

plate_discipline_all_pitch_group$Command <- ifelse(plate_discipline_all_pitch_group$pitch_type == "Breaking",
  (((plate_discipline_all_pitch_group$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
    ((plate_discipline_all_pitch_group$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
    (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - plate_discipline_all_pitch_group$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) + 
    (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - plate_discipline_all_pitch_group$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","sd"][[1]]))),
  (ifelse((plate_discipline_all_pitch_group$pitch_type == "Offspeed"),
    ((plate_discipline_all_pitch_group$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) +
      ((plate_discipline_all_pitch_group$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) +
      (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]] - plate_discipline_all_pitch_group$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) + 
      (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]] - plate_discipline_all_pitch_group$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])),
    ((plate_discipline_all_pitch_group$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
      ((plate_discipline_all_pitch_group$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
      (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - plate_discipline_all_pitch_group$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) + 
      (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - plate_discipline_all_pitch_group$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])))))

plate_discipline_all_pitch_group$Stuff <- predict.lm(era_model_stuff,plate_discipline_all_pitch_group)


plate_discipline_all_pitch_group <- left_join(plate_discipline_all_pitch_group, all_pitches_stats_summary[,c(1,3,4,5,7,8,9,10,11,12)], by = c("Season", "pitcher", "pitch_type" = "pitch_group"))

plate_discipline_all_pitch_group$Move_x <- ifelse(plate_discipline_all_pitch_type_stats$p_throws == "R", 
                                                       plate_discipline_all_pitch_type_stats$Move_x * -1,
                                                       plate_discipline_all_pitch_type_stats$Move_x)
plate_discipline_all_pitch_group$BU <- round(plate_discipline_all_pitch_group$Spin_Rate/plate_discipline_all_pitch_group$Velocity,2)


### All Fastball----
all_Fastball <- all_data %>% 
  filter(pitch_type %in% c("FF"))

in_zone_Fastball <- all_Fastball %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Fastball <- all_Fastball %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Fastball <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = in_zone_Fastball)

model_out_of_zone_Fastball <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                        data = out_of_zone_Fastball)

out_of_zone_pitchers_Fastball <- data.frame(ranef(model_out_of_zone_Fastball)$pitcher)
colnames(out_of_zone_pitchers_Fastball) <- "OOZ"

in_zone_pitchers_Fastball <- data.frame(ranef(model_zone_Fastball)$pitcher)
colnames(in_zone_pitchers_Fastball) <- "IZ"

all_results_Fastball <- merge(in_zone_pitchers_Fastball,out_of_zone_pitchers_Fastball,by = "row.names")
colnames(all_results_Fastball) <- c("pitcher","IZ","OOZ")
all_results_Fastball$pitcher <- as.numeric(all_results_Fastball$pitcher)

in_zone_data_Fastball <- in_zone_Fastball %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Fastball <- out_of_zone_Fastball %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Fastball <- whiff_predict_data %>% 
  filter(pitch_type %in% c("FF"))

model_whiff_Fastball <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                                  data = whiff_predict_data_Fastball)

whiff_pitchers_Fastball <- data.frame(ranef(model_whiff_Fastball)$pitcher)
colnames(whiff_pitchers_Fastball) <- "In_Whiff"

whiff_data_Fastball <- whiff_predict_data_Fastball %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Fastball <- merge(whiff_data_Fastball,whiff_pitchers_Fastball, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Fastball <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("FF"))

wOBA_results_Fastball <- wOBA_predict_data_Fastball %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Fastball <- wOBA_predict_data_Fastball %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

# count <- bip_with_predicted_Fastball %>% 
#   group_by(pitcher) %>%
#   summarise(Num = n()) %>%
#   filter(Num > 1)
# 
# countlist <- as.list(count$pitcher)
# 
# bip_with_predicted_Fastball <- bip_with_predicted_2017_Fastball %>%
#   filter(pitcher %in% countlist)

model_wOBA_Fastball <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                                 data = bip_with_predicted_Fastball)

woba_pitchers_Fastball <- data.frame(ranef(model_wOBA_Fastball)$pitcher)
colnames(woba_pitchers_Fastball) <- "In_wOBA"

wOBA_results_Fastball <- merge(wOBA_results_Fastball,woba_pitchers_Fastball, by.x = "pitcher", by.y = "row.names")

all_results_Fastball_allseasons <- merge(in_zone_data_Fastball,out_of_zone_data_Fastball, by = c("pitcher", "player_name")) 
all_results_Fastball_allseasons <- merge(all_results_Fastball_allseasons,all_results_Fastball, by = c("pitcher")) 
all_results_Fastball_allseasons <- merge(all_results_Fastball_allseasons,whiff_data_Fastball, by = c("pitcher", "player_name")) 
all_results_Fastball_allseasons <- merge(all_results_Fastball_allseasons,wOBA_results_Fastball, by = c("pitcher", "player_name")) 


all_results_Fastball_allseasons <- all_results_Fastball_allseasons[,c("pitcher",
                                                                      "player_name",
                                                                      "Pitches.x",
                                                                      "IZ.Swing",
                                                                      "IZ.xSwing",
                                                                      "IZ",
                                                                      "OOZ.Swing",
                                                                      "OOZ.xSwing",
                                                                      "OOZ",
                                                                      "Whiff",
                                                                      "xWhiff",
                                                                      "In_Whiff",
                                                                      "xwOBA",
                                                                      "wOBA",
                                                                      "In_wOBA" )]

all_results_Fastball_allseasons$pitch_type = "FF"


all_results_Fastball_allseasons$Command <- ((all_results_Fastball_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
                                                             ((all_results_Fastball_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
                                                             (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_Fastball_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) + 
                                                             (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_Fastball_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","sd"][[1]]))

all_results_Fastball_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Fastball_allseasons)

### All Cutter----
all_Cutter <- all_data %>% 
  filter(pitch_type %in% c("FC"))

in_zone_Cutter <- all_Cutter %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Cutter <- all_Cutter %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Cutter <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                            data = in_zone_Cutter)

model_out_of_zone_Cutter <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                   data = out_of_zone_Cutter)

out_of_zone_pitchers_Cutter <- data.frame(ranef(model_out_of_zone_Cutter)$pitcher)
colnames(out_of_zone_pitchers_Cutter) <- "OOZ"

in_zone_pitchers_Cutter <- data.frame(ranef(model_zone_Cutter)$pitcher)
colnames(in_zone_pitchers_Cutter) <- "IZ"

all_results_Cutter <- merge(in_zone_pitchers_Cutter,out_of_zone_pitchers_Cutter,by = "row.names")
colnames(all_results_Cutter) <- c("pitcher","IZ","OOZ")
all_results_Cutter$pitcher <- as.numeric(all_results_Cutter$pitcher)

in_zone_data_Cutter <- in_zone_Cutter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Cutter <- out_of_zone_Cutter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Cutter <- whiff_predict_data %>% 
  filter(pitch_type %in% c("FC"))

model_whiff_Cutter <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                             data = whiff_predict_data_Cutter)

whiff_pitchers_Cutter <- data.frame(ranef(model_whiff_Cutter)$pitcher)
colnames(whiff_pitchers_Cutter) <- "In_Whiff"

whiff_data_Cutter <- whiff_predict_data_Cutter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Cutter <- merge(whiff_data_Cutter,whiff_pitchers_Cutter, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Cutter <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("FC"))

wOBA_results_Cutter <- wOBA_predict_data_Cutter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Cutter <- wOBA_predict_data_Cutter %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

# count <- bip_with_predicted_Cutter %>% 
#   group_by(pitcher) %>%
#   summarise(Num = n()) %>%
#   filter(Num > 1)
# 
# countlist <- as.list(count$pitcher)
# 
# bip_with_predicted_Cutter <- bip_with_predicted_2017_Cutter %>%
#   filter(pitcher %in% countlist)

model_wOBA_Cutter <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                            data = bip_with_predicted_Cutter)

woba_pitchers_Cutter <- data.frame(ranef(model_wOBA_Cutter)$pitcher)
colnames(woba_pitchers_Cutter) <- "In_wOBA"

wOBA_results_Cutter <- merge(wOBA_results_Cutter,woba_pitchers_Cutter, by.x = "pitcher", by.y = "row.names")

all_results_Cutter_allseasons <- merge(in_zone_data_Cutter,out_of_zone_data_Cutter, by = c("pitcher", "player_name")) 
all_results_Cutter_allseasons <- merge(all_results_Cutter_allseasons,all_results_Cutter, by = c("pitcher")) 
all_results_Cutter_allseasons <- merge(all_results_Cutter_allseasons,whiff_data_Cutter, by = c("pitcher", "player_name")) 
all_results_Cutter_allseasons <- merge(all_results_Cutter_allseasons,wOBA_results_Cutter, by = c("pitcher", "player_name")) 


all_results_Cutter_allseasons <- all_results_Cutter_allseasons[,c("pitcher",
                                                                      "player_name",
                                                                      "Pitches.x",
                                                                      "IZ.Swing",
                                                                      "IZ.xSwing",
                                                                      "IZ",
                                                                      "OOZ.Swing",
                                                                      "OOZ.xSwing",
                                                                      "OOZ",
                                                                      "Whiff",
                                                                      "xWhiff",
                                                                      "In_Whiff",
                                                                      "xwOBA",
                                                                      "wOBA",
                                                                      "In_wOBA" )]

all_results_Cutter_allseasons$pitch_type = "FC"


all_results_Cutter_allseasons$Command <- ((all_results_Cutter_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
  ((all_results_Cutter_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_Cutter_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_Cutter_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","sd"][[1]]))

all_results_Cutter_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Cutter_allseasons)

### All Sinker----
all_Sinker <- all_data %>% 
  filter(pitch_type %in% c("SI"))

in_zone_Sinker <- all_Sinker %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Sinker <- all_Sinker %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Sinker <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = in_zone_Sinker)

model_out_of_zone_Sinker <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = out_of_zone_Sinker)

out_of_zone_pitchers_Sinker <- data.frame(ranef(model_out_of_zone_Sinker)$pitcher)
colnames(out_of_zone_pitchers_Sinker) <- "OOZ"

in_zone_pitchers_Sinker <- data.frame(ranef(model_zone_Sinker)$pitcher)
colnames(in_zone_pitchers_Sinker) <- "IZ"

all_results_Sinker <- merge(in_zone_pitchers_Sinker,out_of_zone_pitchers_Sinker,by = "row.names")
colnames(all_results_Sinker) <- c("pitcher","IZ","OOZ")
all_results_Sinker$pitcher <- as.numeric(all_results_Sinker$pitcher)

in_zone_data_Sinker <- in_zone_Sinker %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Sinker <- out_of_zone_Sinker %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Sinker <- whiff_predict_data %>% 
  filter(pitch_type %in% c("SI"))

model_whiff_Sinker <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                           data = whiff_predict_data_Sinker)

whiff_pitchers_Sinker <- data.frame(ranef(model_whiff_Sinker)$pitcher)
colnames(whiff_pitchers_Sinker) <- "In_Whiff"

whiff_data_Sinker <- whiff_predict_data_Sinker %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Sinker <- merge(whiff_data_Sinker,whiff_pitchers_Sinker, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Sinker <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("SI"))

wOBA_results_Sinker <- wOBA_predict_data_Sinker %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Sinker <- wOBA_predict_data_Sinker %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

# count <- bip_with_predicted_Sinker %>% 
#   group_by(pitcher) %>%
#   summarise(Num = n()) %>%
#   filter(Num > 1)
# 
# countlist <- as.list(count$pitcher)
# 
# bip_with_predicted_Sinker <- bip_with_predicted_2017_Sinker %>%
#   filter(pitcher %in% countlist)

model_wOBA_Sinker <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                          data = bip_with_predicted_Sinker)

woba_pitchers_Sinker <- data.frame(ranef(model_wOBA_Sinker)$pitcher)
colnames(woba_pitchers_Sinker) <- "In_wOBA"

wOBA_results_Sinker <- merge(wOBA_results_Sinker,woba_pitchers_Sinker, by.x = "pitcher", by.y = "row.names")

all_results_Sinker_allseasons <- merge(in_zone_data_Sinker,out_of_zone_data_Sinker, by = c("pitcher", "player_name")) 
all_results_Sinker_allseasons <- merge(all_results_Sinker_allseasons,all_results_Sinker, by = c("pitcher")) 
all_results_Sinker_allseasons <- merge(all_results_Sinker_allseasons,whiff_data_Sinker, by = c("pitcher", "player_name")) 
all_results_Sinker_allseasons <- merge(all_results_Sinker_allseasons,wOBA_results_Sinker, by = c("pitcher", "player_name")) 


all_results_Sinker_allseasons <- all_results_Sinker_allseasons[,c("pitcher",
                                                                  "player_name",
                                                                  "Pitches.x",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "xwOBA",
                                                                  "wOBA",
                                                                  "In_wOBA" )]

all_results_Sinker_allseasons$pitch_type = "SI"


all_results_Sinker_allseasons$Command <- ((all_results_Sinker_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
  ((all_results_Sinker_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_Sinker_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_Sinker_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","sd"][[1]]))

all_results_Sinker_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Sinker_allseasons)

### All TwoSeam----
all_TwoSeam <- all_data %>% 
  filter(pitch_type %in% c("FT"))

in_zone_TwoSeam <- all_TwoSeam %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_TwoSeam <- all_TwoSeam %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_TwoSeam <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = in_zone_TwoSeam)

model_out_of_zone_TwoSeam <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = out_of_zone_TwoSeam)

out_of_zone_pitchers_TwoSeam <- data.frame(ranef(model_out_of_zone_TwoSeam)$pitcher)
colnames(out_of_zone_pitchers_TwoSeam) <- "OOZ"

in_zone_pitchers_TwoSeam <- data.frame(ranef(model_zone_TwoSeam)$pitcher)
colnames(in_zone_pitchers_TwoSeam) <- "IZ"

all_results_TwoSeam <- merge(in_zone_pitchers_TwoSeam,out_of_zone_pitchers_TwoSeam,by = "row.names")
colnames(all_results_TwoSeam) <- c("pitcher","IZ","OOZ")
all_results_TwoSeam$pitcher <- as.numeric(all_results_TwoSeam$pitcher)

in_zone_data_TwoSeam <- in_zone_TwoSeam %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_TwoSeam <- out_of_zone_TwoSeam %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_TwoSeam <- whiff_predict_data %>% 
  filter(pitch_type %in% c("FT"))

model_whiff_TwoSeam <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                           data = whiff_predict_data_TwoSeam)

whiff_pitchers_TwoSeam <- data.frame(ranef(model_whiff_TwoSeam)$pitcher)
colnames(whiff_pitchers_TwoSeam) <- "In_Whiff"

whiff_data_TwoSeam <- whiff_predict_data_TwoSeam %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_TwoSeam <- merge(whiff_data_TwoSeam,whiff_pitchers_TwoSeam, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_TwoSeam <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("FT"))

wOBA_results_TwoSeam <- wOBA_predict_data_TwoSeam %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_TwoSeam <- wOBA_predict_data_TwoSeam %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

# count <- bip_with_predicted_TwoSeam %>% 
#   group_by(pitcher) %>%
#   summarise(Num = n()) %>%
#   filter(Num > 1)
# 
# countlist <- as.list(count$pitcher)
# 
# bip_with_predicted_TwoSeam <- bip_with_predicted_2017_TwoSeam %>%
#   filter(pitcher %in% countlist)

model_wOBA_TwoSeam <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                          data = bip_with_predicted_TwoSeam)

woba_pitchers_TwoSeam <- data.frame(ranef(model_wOBA_TwoSeam)$pitcher)
colnames(woba_pitchers_TwoSeam) <- "In_wOBA"

wOBA_results_TwoSeam <- merge(wOBA_results_TwoSeam,woba_pitchers_TwoSeam, by.x = "pitcher", by.y = "row.names")

all_results_TwoSeam_allseasons <- merge(in_zone_data_TwoSeam,out_of_zone_data_TwoSeam, by = c("pitcher", "player_name")) 
all_results_TwoSeam_allseasons <- merge(all_results_TwoSeam_allseasons,all_results_TwoSeam, by = c("pitcher")) 
all_results_TwoSeam_allseasons <- merge(all_results_TwoSeam_allseasons,whiff_data_TwoSeam, by = c("pitcher", "player_name")) 
all_results_TwoSeam_allseasons <- merge(all_results_TwoSeam_allseasons,wOBA_results_TwoSeam, by = c("pitcher", "player_name")) 


all_results_TwoSeam_allseasons <- all_results_TwoSeam_allseasons[,c("pitcher",
                                                                  "player_name",
                                                                  "Pitches.x",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "xwOBA",
                                                                  "wOBA",
                                                                  "In_wOBA" )]

all_results_TwoSeam_allseasons$pitch_type = "FT"


all_results_TwoSeam_allseasons$Command <- ((all_results_TwoSeam_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
  ((all_results_TwoSeam_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_TwoSeam_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Fastball","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","avg"][[1]] - all_results_TwoSeam_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Fastball","sd"][[1]]))

all_results_TwoSeam_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_TwoSeam_allseasons)

### All Slider----
all_Slider <- all_data %>% 
  filter(pitch_type %in% c("SL"))

in_zone_Slider <- all_Slider %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Slider <- all_Slider %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Slider <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = in_zone_Slider)

model_out_of_zone_Slider <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = out_of_zone_Slider)

out_of_zone_pitchers_Slider <- data.frame(ranef(model_out_of_zone_Slider)$pitcher)
colnames(out_of_zone_pitchers_Slider) <- "OOZ"

in_zone_pitchers_Slider <- data.frame(ranef(model_zone_Slider)$pitcher)
colnames(in_zone_pitchers_Slider) <- "IZ"

all_results_Slider <- merge(in_zone_pitchers_Slider,out_of_zone_pitchers_Slider,by = "row.names")
colnames(all_results_Slider) <- c("pitcher","IZ","OOZ")
all_results_Slider$pitcher <- as.numeric(all_results_Slider$pitcher)

in_zone_data_Slider <- in_zone_Slider %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Slider <- out_of_zone_Slider %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Slider <- whiff_predict_data %>% 
  filter(pitch_type %in% c("SL"))

model_whiff_Slider <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                           data = whiff_predict_data_Slider)

whiff_pitchers_Slider <- data.frame(ranef(model_whiff_Slider)$pitcher)
colnames(whiff_pitchers_Slider) <- "In_Whiff"

whiff_data_Slider <- whiff_predict_data_Slider %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Slider <- merge(whiff_data_Slider,whiff_pitchers_Slider, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Slider <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("SL"))

wOBA_results_Slider <- wOBA_predict_data_Slider %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Slider <- wOBA_predict_data_Slider %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_Slider <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                          data = bip_with_predicted_Slider)

woba_pitchers_Slider <- data.frame(ranef(model_wOBA_Slider)$pitcher)
colnames(woba_pitchers_Slider) <- "In_wOBA"

wOBA_results_Slider <- merge(wOBA_results_Slider,woba_pitchers_Slider, by.x = "pitcher", by.y = "row.names")

all_results_Slider_allseasons <- merge(in_zone_data_Slider,out_of_zone_data_Slider, by = c("pitcher", "player_name")) 
all_results_Slider_allseasons <- merge(all_results_Slider_allseasons,all_results_Slider, by = c("pitcher")) 
all_results_Slider_allseasons <- merge(all_results_Slider_allseasons,whiff_data_Slider, by = c("pitcher", "player_name")) 
all_results_Slider_allseasons <- merge(all_results_Slider_allseasons,wOBA_results_Slider, by = c("pitcher", "player_name")) 


all_results_Slider_allseasons <- all_results_Slider_allseasons[,c("pitcher",
                                                                  "player_name",
                                                                  "Pitches.x",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "xwOBA",
                                                                  "wOBA",
                                                                  "In_wOBA" )]

all_results_Slider_allseasons$pitch_type = "SL"


all_results_Slider_allseasons$Command <- ((all_results_Slider_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
  ((all_results_Slider_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - all_results_Slider_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - all_results_Slider_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","sd"][[1]]))

all_results_Slider_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Slider_allseasons)



### All Curve----
all_Curve <- all_data %>% 
  filter(pitch_type %in% c("CU"))

in_zone_Curve <- all_Curve %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Curve <- all_Curve %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Curve <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = in_zone_Curve)

model_out_of_zone_Curve <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = out_of_zone_Curve)

out_of_zone_pitchers_Curve <- data.frame(ranef(model_out_of_zone_Curve)$pitcher)
colnames(out_of_zone_pitchers_Curve) <- "OOZ"

in_zone_pitchers_Curve <- data.frame(ranef(model_zone_Curve)$pitcher)
colnames(in_zone_pitchers_Curve) <- "IZ"

all_results_Curve <- merge(in_zone_pitchers_Curve,out_of_zone_pitchers_Curve,by = "row.names")
colnames(all_results_Curve) <- c("pitcher","IZ","OOZ")
all_results_Curve$pitcher <- as.numeric(all_results_Curve$pitcher)

in_zone_data_Curve <- in_zone_Curve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Curve <- out_of_zone_Curve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Curve <- whiff_predict_data %>% 
  filter(pitch_type %in% c("CU"))

model_whiff_Curve <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                           data = whiff_predict_data_Curve)

whiff_pitchers_Curve <- data.frame(ranef(model_whiff_Curve)$pitcher)
colnames(whiff_pitchers_Curve) <- "In_Whiff"

whiff_data_Curve <- whiff_predict_data_Curve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Curve <- merge(whiff_data_Curve,whiff_pitchers_Curve, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Curve <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("CU"))

wOBA_results_Curve <- wOBA_predict_data_Curve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Curve <- wOBA_predict_data_Curve %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_Curve <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                          data = bip_with_predicted_Curve)

woba_pitchers_Curve <- data.frame(ranef(model_wOBA_Curve)$pitcher)
colnames(woba_pitchers_Curve) <- "In_wOBA"

wOBA_results_Curve <- merge(wOBA_results_Curve,woba_pitchers_Curve, by.x = "pitcher", by.y = "row.names")

all_results_Curve_allseasons <- merge(in_zone_data_Curve,out_of_zone_data_Curve, by = c("pitcher", "player_name")) 
all_results_Curve_allseasons <- merge(all_results_Curve_allseasons,all_results_Curve, by = c("pitcher")) 
all_results_Curve_allseasons <- merge(all_results_Curve_allseasons,whiff_data_Curve, by = c("pitcher", "player_name")) 
all_results_Curve_allseasons <- merge(all_results_Curve_allseasons,wOBA_results_Curve, by = c("pitcher", "player_name")) 


all_results_Curve_allseasons <- all_results_Curve_allseasons[,c("pitcher",
                                                                  "player_name",
                                                                  "Pitches.x",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "xwOBA",
                                                                  "wOBA",
                                                                  "In_wOBA" )]

all_results_Curve_allseasons$pitch_type = "CU"


all_results_Curve_allseasons$Command <- ((all_results_Curve_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
  ((all_results_Curve_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - all_results_Curve_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - all_results_Curve_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","sd"][[1]]))

all_results_Curve_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Curve_allseasons)

### All KCurve----
all_KCurve <- all_data %>% 
  filter(pitch_type %in% c("KC"))

in_zone_KCurve <- all_KCurve %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_KCurve <- all_KCurve %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_KCurve <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                         data = in_zone_KCurve)

model_out_of_zone_KCurve <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                data = out_of_zone_KCurve)

out_of_zone_pitchers_KCurve <- data.frame(ranef(model_out_of_zone_KCurve)$pitcher)
colnames(out_of_zone_pitchers_KCurve) <- "OOZ"

in_zone_pitchers_KCurve <- data.frame(ranef(model_zone_KCurve)$pitcher)
colnames(in_zone_pitchers_KCurve) <- "IZ"

all_results_KCurve <- merge(in_zone_pitchers_KCurve,out_of_zone_pitchers_KCurve,by = "row.names")
colnames(all_results_KCurve) <- c("pitcher","IZ","OOZ")
all_results_KCurve$pitcher <- as.numeric(all_results_KCurve$pitcher)

in_zone_data_KCurve <- in_zone_KCurve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_KCurve <- out_of_zone_KCurve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_KCurve <- whiff_predict_data %>% 
  filter(pitch_type %in% c("KC"))

model_whiff_KCurve <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                          data = whiff_predict_data_KCurve)

whiff_pitchers_KCurve <- data.frame(ranef(model_whiff_KCurve)$pitcher)
colnames(whiff_pitchers_KCurve) <- "In_Whiff"

whiff_data_KCurve <- whiff_predict_data_KCurve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_KCurve <- merge(whiff_data_KCurve,whiff_pitchers_KCurve, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_KCurve <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("KC"))

wOBA_results_KCurve <- wOBA_predict_data_KCurve %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_KCurve <- wOBA_predict_data_KCurve %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_KCurve <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                         data = bip_with_predicted_KCurve)

woba_pitchers_KCurve <- data.frame(ranef(model_wOBA_KCurve)$pitcher)
colnames(woba_pitchers_KCurve) <- "In_wOBA"

wOBA_results_KCurve <- merge(wOBA_results_KCurve,woba_pitchers_KCurve, by.x = "pitcher", by.y = "row.names")

all_results_KCurve_allseasons <- merge(in_zone_data_KCurve,out_of_zone_data_KCurve, by = c("pitcher", "player_name")) 
all_results_KCurve_allseasons <- merge(all_results_KCurve_allseasons,all_results_KCurve, by = c("pitcher")) 
all_results_KCurve_allseasons <- merge(all_results_KCurve_allseasons,whiff_data_KCurve, by = c("pitcher", "player_name")) 
all_results_KCurve_allseasons <- merge(all_results_KCurve_allseasons,wOBA_results_KCurve, by = c("pitcher", "player_name")) 


all_results_KCurve_allseasons <- all_results_KCurve_allseasons[,c("pitcher",
                                                                "player_name",
                                                                "Pitches.x",
                                                                "IZ.Swing",
                                                                "IZ.xSwing",
                                                                "IZ",
                                                                "OOZ.Swing",
                                                                "OOZ.xSwing",
                                                                "OOZ",
                                                                "Whiff",
                                                                "xWhiff",
                                                                "In_Whiff",
                                                                "xwOBA",
                                                                "wOBA",
                                                                "In_wOBA" )]

all_results_KCurve_allseasons$pitch_type = "KC"


all_results_KCurve_allseasons$Command <- ((all_results_KCurve_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
  ((all_results_KCurve_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - all_results_KCurve_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "Breaking","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","avg"][[1]] - all_results_KCurve_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "Breaking","sd"][[1]]))

all_results_KCurve_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_KCurve_allseasons)

### All Change----
all_Change <- all_data %>% 
  filter(pitch_type %in% c("CH"))

in_zone_Change <- all_Change %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Change <- all_Change %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Change <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = in_zone_Change)

model_out_of_zone_Change <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = out_of_zone_Change)

out_of_zone_pitchers_Change <- data.frame(ranef(model_out_of_zone_Change)$pitcher)
colnames(out_of_zone_pitchers_Change) <- "OOZ"

in_zone_pitchers_Change <- data.frame(ranef(model_zone_Change)$pitcher)
colnames(in_zone_pitchers_Change) <- "IZ"

all_results_Change <- merge(in_zone_pitchers_Change,out_of_zone_pitchers_Change,by = "row.names")
colnames(all_results_Change) <- c("pitcher","IZ","OOZ")
all_results_Change$pitcher <- as.numeric(all_results_Change$pitcher)

in_zone_data_Change <- in_zone_Change %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Change <- out_of_zone_Change %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Change <- whiff_predict_data %>% 
  filter(pitch_type %in% c("CH"))

model_whiff_Change <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                           data = whiff_predict_data_Change)

whiff_pitchers_Change <- data.frame(ranef(model_whiff_Change)$pitcher)
colnames(whiff_pitchers_Change) <- "In_Whiff"

whiff_data_Change <- whiff_predict_data_Change %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Change <- merge(whiff_data_Change,whiff_pitchers_Change, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Change <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("CH"))

wOBA_results_Change <- wOBA_predict_data_Change %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Change <- wOBA_predict_data_Change %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_Change <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                          data = bip_with_predicted_Change)

woba_pitchers_Change <- data.frame(ranef(model_wOBA_Change)$pitcher)
colnames(woba_pitchers_Change) <- "In_wOBA"

wOBA_results_Change <- merge(wOBA_results_Change,woba_pitchers_Change, by.x = "pitcher", by.y = "row.names")

all_results_Change_allseasons <- merge(in_zone_data_Change,out_of_zone_data_Change, by = c("pitcher", "player_name")) 
all_results_Change_allseasons <- merge(all_results_Change_allseasons,all_results_Change, by = c("pitcher")) 
all_results_Change_allseasons <- merge(all_results_Change_allseasons,whiff_data_Change, by = c("pitcher", "player_name")) 
all_results_Change_allseasons <- merge(all_results_Change_allseasons,wOBA_results_Change, by = c("pitcher", "player_name")) 


all_results_Change_allseasons <- all_results_Change_allseasons[,c("pitcher",
                                                                  "player_name",
                                                                  "Pitches.x",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "xwOBA",
                                                                  "wOBA",
                                                                  "In_wOBA" )]

all_results_Change_allseasons$pitch_type = "CH"


all_results_Change_allseasons$Command <- ((all_results_Change_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) +
  ((all_results_Change_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]] - all_results_Change_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]] - all_results_Change_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]]))

all_results_Change_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Change_allseasons)

### All Splitter----
all_Splitter <- all_data %>% 
  filter(pitch_type %in% c("FS"))

in_zone_Splitter <- all_Splitter %>% filter(zone == 1 | zone == 2 | zone == 3 | zone == 4 | zone == 5 | zone == 6 | zone == 7 | zone == 8 |zone == 9)

out_of_zone_Splitter <- all_Splitter %>% filter(zone == 10 | zone == 11 | zone == 12 | zone == 13)


model_zone_Splitter <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                          data = in_zone_Splitter)

model_out_of_zone_Splitter <- lmer(swing ~ predict + (1|pitcher) + (1|batter),
                                 data = out_of_zone_Splitter)

out_of_zone_pitchers_Splitter <- data.frame(ranef(model_out_of_zone_Splitter)$pitcher)
colnames(out_of_zone_pitchers_Splitter) <- "OOZ"

in_zone_pitchers_Splitter <- data.frame(ranef(model_zone_Splitter)$pitcher)
colnames(in_zone_pitchers_Splitter) <- "IZ"

all_results_Splitter <- merge(in_zone_pitchers_Splitter,out_of_zone_pitchers_Splitter,by = "row.names")
colnames(all_results_Splitter) <- c("pitcher","IZ","OOZ")
all_results_Splitter$pitcher <- as.numeric(all_results_Splitter$pitcher)

in_zone_data_Splitter <- in_zone_Splitter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 50)

out_of_zone_data_Splitter <- out_of_zone_Splitter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 50)

whiff_predict_data_Splitter <- whiff_predict_data %>% 
  filter(pitch_type %in% c("FS"))

model_whiff_Splitter <- lmer(whiff ~ predict_whiff + (1|pitcher) + (1|batter),
                           data = whiff_predict_data_Splitter)

whiff_pitchers_Splitter <- data.frame(ranef(model_whiff_Splitter)$pitcher)
colnames(whiff_pitchers_Splitter) <- "In_Whiff"

whiff_data_Splitter <- whiff_predict_data_Splitter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict_whiff),5)) %>%
  filter(Pitches >= 50)

whiff_data_Splitter <- merge(whiff_data_Splitter,whiff_pitchers_Splitter, by.x = "pitcher", by.y = "row.names")

wOBA_predict_data_Splitter <- wOBA_predict_data %>% 
  filter(pitch_type %in% c("FS"))

wOBA_results_Splitter <- wOBA_predict_data_Splitter %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            xwOBA = round(mean(predict_wOBA),4),
            wOBA = round(mean(woba_value, na.rm=TRUE),4)) %>%
  filter(Pitches > 50)

bip_with_predicted_Splitter <- wOBA_predict_data_Splitter %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

model_wOBA_Splitter <- lmer(woba_value ~ predict_wOBA + (1|pitcher) + (1|batter),
                          data = bip_with_predicted_Splitter)

woba_pitchers_Splitter <- data.frame(ranef(model_wOBA_Splitter)$pitcher)
colnames(woba_pitchers_Splitter) <- "In_wOBA"

wOBA_results_Splitter <- merge(wOBA_results_Splitter,woba_pitchers_Splitter, by.x = "pitcher", by.y = "row.names")

all_results_Splitter_allseasons <- merge(in_zone_data_Splitter,out_of_zone_data_Splitter, by = c("pitcher", "player_name")) 
all_results_Splitter_allseasons <- merge(all_results_Splitter_allseasons,all_results_Splitter, by = c("pitcher")) 
all_results_Splitter_allseasons <- merge(all_results_Splitter_allseasons,whiff_data_Splitter, by = c("pitcher", "player_name")) 
all_results_Splitter_allseasons <- merge(all_results_Splitter_allseasons,wOBA_results_Splitter, by = c("pitcher", "player_name")) 


all_results_Splitter_allseasons <- all_results_Splitter_allseasons[,c("pitcher",
                                                                  "player_name",
                                                                  "Pitches.x",
                                                                  "IZ.Swing",
                                                                  "IZ.xSwing",
                                                                  "IZ",
                                                                  "OOZ.Swing",
                                                                  "OOZ.xSwing",
                                                                  "OOZ",
                                                                  "Whiff",
                                                                  "xWhiff",
                                                                  "In_Whiff",
                                                                  "xwOBA",
                                                                  "wOBA",
                                                                  "In_wOBA" )]

all_results_Splitter_allseasons$pitch_type = "FS"


all_results_Splitter_allseasons$Command <- ((all_results_Splitter_allseasons$xWhiff - (whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]]))/(whiff_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) +
  ((all_results_Splitter_allseasons$OOZ.xSwing - (out_of_zone_avg_pitch_group[out_of_zone_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]]))/(out_of_zone_avg_pitch_group[whiff_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) +
  (((in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]] - all_results_Splitter_allseasons$IZ.xSwing)/in_zone_avg_pitch_group[in_zone_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]])) + 
  (((woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "OffSpeed","avg"][[1]] - all_results_Splitter_allseasons$xwOBA)/woba_avg_pitch_group[woba_avg_pitch_group$pitch_group == "OffSpeed","sd"][[1]]))

all_results_Splitter_allseasons$Stuff <- predict.lm(era_model_stuff,all_results_Splitter_allseasons)

all_results_allpitches_allseasons <- rbind(all_results_Fastball_allseasons,
                                           all_results_TwoSeam_allseasons,
                                           all_results_Sinker_allseasons,
                                           all_results_Cutter_allseasons,
                                           all_results_Change_allseasons,
                                           all_results_Splitter_allseasons,
                                           all_results_Curve_allseasons,
                                           all_results_Slider_allseasons,
                                           all_results_KCurve_allseasons)

saveRDS(all_results_allpitches_allseasons, "all_results_allpitches_allseasons.rds")
saveRDS((plate_discipline_all_woba), "plate_discipline_all_woba.rds")

### Pitch Type Correlations----
all_pitches_stats <- readRDS("all_pitches_stats.rds")

all_pitches_clean <- all_pitches_stats[complete.cases(all_pitches_stats),]

all_pitches_stats_summary <- all_pitches_clean %>% 
  group_by(pitcher, player_name, pitch_type,p_throws) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Count = n(),
            Velocity = round(mean(release_speed),1),
            Move_x = round(mean(pfx_x),3),
            Move_z = round(mean(pfx_z),3),
            Effective_Speed = round(mean(effective_speed),1),
            Spin_Rate = round(mean(release_spin_rate),3),
            Release_Extension = round(mean(release_extension),3))

all_pitches_stats_summary <- all_pitches_stats_summary %>%
  filter(pitch_type == "CH" |
           pitch_type == "CU" |
           pitch_type == "FC" |
           pitch_type == "FF" |
           pitch_type == "KC" |
           pitch_type == "SI" |
           pitch_type == "SL" |
           pitch_type == "FS" |
           pitch_type == "FT")

all_results_pitches_withstats <- left_join(all_results_allpitches_allseasons,all_pitches_stats_summary, by = c("pitcher", "player_name", "pitch_type"))
all_results_pitches_withstats$BU <- round(all_results_pitches_withstats$Spin_Rate/all_results_pitches_withstats$Velocity,2)
all_results_pitches_withstats$Move_Angle <- rad2deg(atan2(all_results_pitches_withstats$Move_z,all_results_pitches_withstats$Move_x))

pitch_correlations <- all_results_pitches_withstats %>%
  group_by(pitch_type, p_throws) %>%
  summarise(
    Num = n(),
    Velo = round(cor(Velocity,Stuff),3),
    Movex = round(cor(Move_x,Stuff),3),
    Movez = round(cor(Move_z,Stuff),3),
    E_Speed = round(cor(Effective_Speed,Stuff),3),
    Spin = round(cor(Spin_Rate,Stuff),3),
    Ext = round(cor(Release_Extension,Stuff),3),
    BU = round(cor(BU,Stuff),3),
    Move_Angle = round(cor(Move_Angle,Stuff),3))

fastball_stats <- all_pitches_stats_summary %>%
  filter(pitch_type == "FF")
fastball_stats$BU <- round(fastball_stats$Spin_Rate/fastball_stats$Velocity,2)
fastball_stats$Move_Angle <- rad2deg(atan2(fastball_stats$Move_z,fastball_stats$Move_x))

fastball_stats <- fastball_stats[,c("pitcher",
                                "BU",
                                "Move_Angle")]

colnames(fastball_stats) <- c("pitcher", "BU_FF", "Move_Angle_FF")

breaking <- all_results_pitches_withstats %>%
  filter(pitch_type %in% c("CU", "KC", "SL"))

breaking_fastball <- left_join(breaking, fastball_stats, by = "pitcher")

breaking_fastball <- breaking_fastball %>%
  filter(!is.na(BU_FF))

#Correlation Results By Inning Break ----
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
results_all_seasons_NA <- left_join(results_all_seasons_NA, plate_discipline_all_woba[,c("pitcher", "player_name")], by = c("Name" = "player_name"))
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

plate_discipline_all_woba$StuffERA2 <- predict(era_stuff_model_seasonal, newdata = plate_discipline_all_woba)
results_all_seasons_Stuff <- left_join(results_all_seasons,plate_discipline_all_woba[,c("pitcher","Season", "IZ","OOZ","In_Whiff","In_wOBA","Command", "StuffERA", "StuffERA2")], by = c("mlb_id" = "pitcher", "Seasons" = "Season"))


get_correlations <- function(innings){
  sample <- results_all_seasons_Stuff %>%
    filter(IP >= innings)
  
  correlations <- data.frame("IP" = innings,
                             "FIP_Cor" = round(cor(sample$ERA,sample$FIP),3), 
                             "Stuff_Cor" = round(cor(sample$ERA,sample$StuffERA2),3))
  correlations
}

innings_list <- seq(20, 160, 20)

all_correlations <- lapply(innings_list, get_correlations)
all_correlations <- do.call("rbind", all_correlations)

all_correlations <- all_correlations %>%
  gather(key = "variable", value = "value", -IP)

corr_plot <- ggplot(all_correlations, aes(x = IP, y = value)) + 
  geom_line(aes(color = variable), lwd=1.5) + 
  geom_point(aes(color = variable), size = 4) +
  scale_color_manual(values = c("green", "blue")) +
  theme_bw() +
  labs(x = "Innings Pitched (Min)",
       y = "Correlation",
       caption = "@pmamminofantasy",
       title = "Correlations To ERA By IP Totals",
       subtitle = "FIP and Stuff",
       col = NULL) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10))
corr_plot


ggplotly(corr_plot) %>%
  layout(title = list(text = paste0('Correlations To ERA By IP Totals',
                                    '<br>',
                                    '<sup>',
                                    'FIP And Stuff-Descriptive',
                                    '</sup>')), hovermode = 'compare',
         annotations = 
           list(x = 1, y = -0.1, text = "@pmamminofantasy", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="black")))

get_correlations_prev <- function(innings){
  results_prev <- results_all_seasons_Stuff %>% 
    filter(IP >= innings) %>%
    arrange(playerid, Seasons) %>%
    group_by(playerid) %>% 
    mutate(IZ = dplyr::lag(IZ, n=1, default=NA)) %>%
    mutate(OOZ = dplyr::lag(OOZ, n=1, default=NA)) %>%
    mutate(In_Whiff = dplyr::lag(In_Whiff, n=1, default=NA)) %>%
    mutate(In_wOBA = dplyr::lag(In_wOBA, n=1, default=NA)) %>%
    mutate(Command = dplyr::lag(Command, n=1, default=NA)) %>%
    mutate(FIP_Prev = dplyr::lag(FIP, n=1, default=NA))

  results_prev <- results_prev %>%
    filter(!is.na(FIP_Prev))
  
  results_prev <- plyr::rename(results_prev,c("Seasons" = "Season"))
  results_prev$Stuff_Prev <- predict(era_stuff_model_seasonal, newdata = results_prev)
  
  correlations <- data.frame("IP" = innings,
                             "PrevFIP_Cor" = round(cor(results_prev$ERA,results_prev$FIP_Prev),3), 
                             "PrevStuff_Cor" = round(cor(results_prev$ERA,results_prev$Stuff_Prev),3))
  correlations
   
}

all_correlations_prev <- lapply(innings_list, get_correlations_prev)
all_correlations_prev <- do.call("rbind", all_correlations_prev)

all_correlations_prev <- all_correlations_prev %>%
  gather(key = "variable", value = "value", -IP)

corr_prev_plot <- ggplot(all_correlations_prev, aes(x = IP, y = value)) + 
  geom_line(aes(color = variable), lwd=1.5) + 
  geom_point(aes(color = variable), size = 4) +
  scale_color_manual(values = c("green", "blue")) +
  theme_bw() +
  labs(x = "Innings Pitched (Min)",
       y = "Correlation",
       caption = "@pmamminofantasy",
       title = "Correlations To Future ERA By IP Totals",
       subtitle = "FIP and Stuff") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10))
corr_prev_plot

test <- results_all_seasons_Stuff %>% 
  filter(IP >= 100) %>%
  arrange(playerid, Seasons) %>%
  group_by(playerid) %>% 
  mutate(IZ_Prev = dplyr::lag(IZ, n=1, default=NA)) %>%
  mutate(OOZ_Prev = dplyr::lag(OOZ, n=1, default=NA)) %>% 
  mutate(Whiff_Prev = dplyr::lag(In_Whiff, n=1, default=NA)) %>%
  mutate(wOBA_Prev = dplyr::lag(In_wOBA, n=1, default=NA)) %>% 
  mutate(Command_Prev = dplyr::lag(Command, n=1, default=NA))

test <- test %>%
  filter(!is.na(Command_Prev))

cor(test$IZ,test$IZ_Prev)
cor(test$OOZ,test$OOZ_Prev)
cor(test$In_Whiff,test$Whiff_Prev)
cor(test$In_wOBA,test$wOBA_Prev)
cor(test$Command,test$Command_Prev)


