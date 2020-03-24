install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mgcv")
install.packages("REdaS")
install.packages("gridExtra")
install.packages("lme4")
install.packages('reshape')
install.packages("Hmisc")
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

#Helper Functions -----
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

all_pitches_stats <- all_pitches[,c("Season",
                                    "pitch_type",
                                    "player_name",
                                    "pitcher",
                                    "p_throws",
                                    "release_speed",
                                    "pfx_x",
                                    "pfx_z",
                                    "effective_speed",
                                    "release_spin_rate",
                                    "release_extension")]

saveRDS(all_pitches_stats,"all_pitches_stats.rds")                                    

all_pitches_trim <- all_pitches[,c("Season",
                                 "pitch_type",
                                 "player_name",
                                 "batter",
                                 "pitcher",
                                 "description",
                                 "zone",
                                 "stand",
                                 "p_throws",
                                 "balls",
                                 "strikes",
                                 "plate_x",
                                 "plate_z",
                                 "fielder_2",
                                 "woba_value")]

all_pitches_trim <- mutate(all_pitches_trim, swing=ifelse(description %in% c("hit_into_play", "foul","swinging_strike", "hit_into_play_score", "hit_into_play_no_out", "foul_tip", "swinging_strike_blocked"),
                                             1, 0))
all_pitches_trim <-mutate(all_pitches_trim, whiff=ifelse(description %in% c("swinging_strike","swinging_strike_blocked"),
                                    1, 0))
all_pitches_trim <- all_pitches_trim[!(is.na(all_pitches_trim$plate_x)) | !(is.na(all_pitches_trim$plate_z)),]
all_pitches_trim <- all_pitches_trim[!(is.na(all_pitches_trim$pitch_type)),]

all_pitches_trim <- mutate(all_pitches_trim, pitch_group=ifelse(pitch_type %in% c("FF", "FT","FC", "SI", "FA"),
                                                           "Fastball", ifelse(pitch_type %in% c("SL", "EP","CU", "KN", "KC"),
                                                                              "Breaking", "OffSpeed")))

all_pitches_trim$count <- paste(all_pitches_trim$balls,"-",all_pitches_trim$strikes)

all_splits <- split(all_pitches_trim, with(all_pitches_trim, interaction(pitch_group,count)), drop = TRUE)
list2env(all_splits,envir=.GlobalEnv)

#Swing Predict ----
all_predict <- all_pitches_trim[FALSE,]
all_predict$predict <- numeric()

predict_swing <- function(x)
{
  if(nrow(all_splits[[x]]) > 100)
   {
     fit <- gam(swing ~ s(plate_x,plate_z), family=binomial, data=all_splits[[x]])

     all_splits[[x]]$predict <- exp(predict(fit,all_splits[[x]]))/(1 + exp(predict(fit,all_splits[[x]])))

     all_predict <- rbind(all_predict,all_splits[[x]])
   }
 }



all_data <- lapply(1:length(all_splits), predict_swing)
all_data <- do.call("rbind", all_data)

saveRDS(all_pitches_trim,"all_pitches_trim.rds")
saveRDS(all_data,"all_data.rds")

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
all_predict_whiff_predict <- all_pitches_trim[FALSE,]
all_predict_whiff_predict$predict_whiff <- numeric()
 
predict_whiff <- function(x)
  {
 if(nrow(all_splits[[x]]) > 100)
   {
   fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=all_splits[[x]])
     
   all_splits[[x]]$predict_whiff <- exp(predict(fit,all_splits[[x]]))/(1 + exp(predict(fit,all_splits[[x]])))
     
   all_splits <- rbind(all_predict_whiff_predict,all_splits[[x]])
  }
}
 
whiff_predict_data <- lapply(1:length(all_splits), predict_whiff)
whiff_predict_data <- do.call("rbind", whiff_predict_data)


saveRDS(whiff_predict_data,"whiff_predict_data.rds")

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

out_of_zone_2016 <- c


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

all_pitches_clean <- all_pitches_stats[complete.cases(all_pitches_stats),]

all_pitches_stats_summary <- all_pitches_clean %>% 
  group_by(pitcher, player_name, Season, pitch_type,p_throws) %>% 
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
                        "BU")]
offspeedcorr <- rcorr(as.matrix(offspeed))
offspeedcorr <- flattenCorrMatrix(offspeedcorr$r, offspeedcorr$P)
