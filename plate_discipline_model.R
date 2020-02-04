install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mgcv")
install.packages("REdaS")
install.packages("gridExtra")
install.packages("lme4")
library(devtools)
library(tidyverse)
install_github("BillPetti/baseballr")
library(baseballr)
library(ggplot2)
library(mgcv)
library(REdaS)
library(gridExtra)
library(lme4)


date <- as.Date("2019-03-19")

all_pitches <- scrape_statcast_savant(start_date = date,
                       end_date = date, player_type = "pitcher")

while(date < as.Date("2019-09-30"))
{
  test <- scrape_statcast_savant(start_date = date,
                                        end_date = date + 10, player_type = "pitcher")
  all_pitches <- rbind(all_pitches,test)
  date <- date + 10
}

all_pitches <- all_pitches %>% distinct()

all_pitches_trim <- all_pitches[,c("pitch_type",
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
all_pitches_trim <- all_pitches_trim[!(is.na(all_pitches_trim$plate_x)) | !(is.na(all_pitches_trim$plate_z)),]
all_pitches_trim <- all_pitches_trim[!(is.na(all_pitches_trim$pitch_type)),]

all_pitches_trim <- mutate(all_pitches_trim, pitch_group=ifelse(pitch_type %in% c("FF", "FT","FC", "SI"),
                                                          "Fastball", ifelse(pitch_type %in% c("SL", "EP","CU", "KN", "KC"),
                                                                             "Breaking", "OffSpeed")))

all_pitches_trim$count <- paste(all_pitches_trim$balls,"-",all_pitches_trim$strikes)

all_splits <- split(all_pitches_trim, with(all_pitches_trim, interaction(pitch_group,count)), drop = TRUE)
list2env(all_splits,envir=.GlobalEnv)

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

#fit <- gam(swing ~ s(plate_x,plate_z), family=binomial, data=all_splits[[1]])

# find predicted probabilities over a 50 x 50 grid
# plate_x <- seq(-1.5, 1.5, length.out=100)
# plate_z <- seq(1.4, 3.75, length.out=100)
# data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
#                            plate_z = c(outer(plate_x * 0 + 1, plate_z)))
# lp <- predict(fit, data.predict)
# data.predict$Probability <- exp(lp) / (1 + exp(lp))
# 
# ggplot(kZone, aes(x, y)) +
#   geom_tile(data=data.predict, 
#             aes(x=plate_x, y=plate_z, fill= Probability)) +
#   scale_fill_distiller(palette = "Spectral") +
#   geom_path(lwd=1.5, col="black") +
#   coord_fixed()+labs(title="Swing Rates")
# 
# all_splits[[1]]$predict <- exp(predict(fit,all_splits[[1]]))/(1 + exp(predict(fit,all_splits[[1]])))
# 
# all_predict <- rbind(all_predict,all_splits[[1]])

all_data <- lapply(1:length(all_splits), predict_swing)
all_data <- do.call("rbind", all_data)

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
colnames(all_results) <- c("player_id","IZ","OOZ")
all_results$player_id <- as.numeric(all_results$player_id)

in_zone_data <- in_zone %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(IZ.Pitches = n(),
            IZ.Swing = round(mean(swing),5),
            IZ.xSwing = round(mean(predict),5)) %>%
  filter(IZ.Pitches >= 500)

out_of_zone_data <- out_of_zone %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(OOZ.Pitches = n(),
            OOZ.Swing = round(mean(swing),5),
            OOZ.xSwing = round(mean(predict),5)) %>%
  filter(OOZ.Pitches >= 300)

pitchers_2019 <- left_join(in_zone_data,out_of_zone_data, by = "pitcher")
pitchers_2019 <- pitchers_2019[,c(1,2,3,4,5,7,8,9)]
colnames(pitchers_2019) <- c("player_id","player_name","IZ.Pitches","IZ.Swing","IZ.xSwing","OOZ.Pitches","OOZ.Swing","OOZ.xSwing")

pitchers_2019 <- left_join(pitchers_2019,all_results,by = "player_id")
pitchers_2019$PD <- round(-1*pitchers_2019$IZ + pitchers_2019$OOZ,5)
pitchers_2019$IZ <- round(pitchers_2019$IZ,5)
pitchers_2019$OOZ <- round(pitchers_2019$OOZ,5)
pitchers_2019 <- pitchers_2019[complete.cases(pitchers_2019),]
pitchers_2019 <- pitchers_2019[,c("player_name","player_id","IZ.Pitches","IZ.Swing","IZ.xSwing","IZ","OOZ.Pitches","OOZ.Swing","OOZ.xSwing", "OOZ", "PD")]
saveRDS(pitchers_2019, file = "pitchers_2019.rds")

master <- read.csv("master.csv")
fg_2019 <- read.csv("fg_2019.csv")
fg_2019 <- fg_2019[,c("playerid","K.","BB.", "SwStr.")]
pitchers_2019 <- left_join(pitchers_2019,master[,c("mlb_id","fg_id")],by = c("player_id"= "mlb_id"))
pitchers_2019 <- pitchers_2019[,c("player_name","fg_id","player_id","IZ.Pitches","IZ.Swing","IZ.xSwing","IZ","OOZ.Pitches","OOZ.Swing","OOZ.xSwing", "OOZ", "PD")]
pitchers_2019$fg_id <- as.numeric(as.character(pitchers_2019$fg_id))
pitchers_2019 <- left_join(pitchers_2019,fg_2019, by = c("fg_id" = "playerid"))
pitchers_2019$K. = as.numeric(gsub("[\\%,]", "", pitchers_2019$K.))
pitchers_2019$BB. = as.numeric(gsub("[\\%,]", "", pitchers_2019$BB.))
pitchers_2019$SwStr. = as.numeric(gsub("[\\%,]", "", pitchers_2019$SwStr.))


whiff_predict <- all_pitches[,c("pitch_type",
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
                              "plate_z")]

whiff_predict <- mutate(whiff_predict, whiff=ifelse(description %in% c("swinging_strike","swinging_strike_blocked"),
                                                          1, 0))

whiff_predict <- whiff_predict[!(is.na(whiff_predict$plate_x)) | !(is.na(whiff_predict$plate_z)),]
whiff_predict <- whiff_predict[!(is.na(whiff_predict$pitch_type)),]

whiff_predict <- mutate(whiff_predict, pitch_group=ifelse(pitch_type %in% c("FF", "FT","FC", "SI"),
                                                                "Fastball", ifelse(pitch_type %in% c("SL", "EP","CU", "KN", "KC"),
                                                                                   "Breaking", "OffSpeed")))

whiff_predict$count <- paste(whiff_predict$balls,"-",whiff_predict$strikes)

all_splits_whiff_predict <- split(whiff_predict, with(whiff_predict, interaction(pitch_group,count)), drop = TRUE)

all_predict_whiff_predict <- whiff_predict[FALSE,]
all_predict_whiff_predict$predict <- numeric()

predict_whiff <- function(x)
{
  if(nrow(all_splits_whiff_predict[[x]]) > 100)
  {
    fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=all_splits_whiff_predict[[x]])
    
    all_splits_whiff_predict[[x]]$predict <- exp(predict(fit,all_splits_whiff_predict[[x]]))/(1 + exp(predict(fit,all_splits_whiff_predict[[x]])))
    
    all_predict_whiff_predict <- rbind(all_predict_whiff_predict,all_splits_whiff_predict[[x]])
  }
}

whiff_predict_data <- lapply(1:length(all_splits_whiff_predict), predict_whiff)
whiff_predict_data <- do.call("rbind", whiff_predict_data)

model_whiff <- lmer(whiff ~ predict + (1|pitcher) + (1|batter),
                   data = whiff_predict_data)

whiff_pitchers <- data.frame(ranef(model_whiff)$pitcher)
colnames(whiff_pitchers) <- "In_Whiff"

whiff_data <- whiff_predict_data %>%
  group_by(pitcher, player_name) %>% 
  arrange(pitcher, player_name) %>%
  summarise(Pitches = n(),
            Whiff = round(mean(whiff),5),
            xWhiff = round(mean(predict),5)) %>%
  filter(Pitches >= 500)

whiff_data <- merge(whiff_data,whiff_pitchers, by.x = "pitcher", by.y = "row.names")
