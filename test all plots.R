install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mgcv")
install.packages("REdaS")
install.packages("gridExtra")
install.packages("lme4")
install.packages('reshape')
install.packages("yaml")
library(reshape)
library(devtools)
library(tidyverse)
# install_github("BillPetti/baseballr")
# library(baseballr)
library(ggplot2)
library(mgcv)
library(REdaS)
library(gridExtra)
library(lme4)
options(scipen = 999)
memory.limit(10000)

all_pitches_trim <- readRDS("all_pitches_trim.rds")
all_splits <- split(all_pitches_trim, with(all_pitches_trim, interaction(pitch_group,count)), drop = TRUE)


#Whiff Predict ----
predict_whiff_model <- function(x)
{
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )  
  
  if(nrow(all_splits[[x]]) > 100)
  {
    fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=all_splits[[x]])
    
    plate_x <- seq(-1.5, 1.5, length.out=100)
    plate_z <- seq(1.4, 3.75, length.out=100)
    data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                               plate_z = c(outer(plate_x * 0 + 1, plate_z)))
    lp <- predict(fit, data.predict)
    data.predict$Probability <- exp(lp) / (1 + exp(lp))
    
    # construct the plot V Same
    Plot <- ggplot(kZone, aes(x, y)) +
      theme_void() +
      geom_tile(data=data.predict, 
                aes(x=plate_x, y=plate_z, fill= Probability)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") +
      coord_fixed()+labs(title=paste(ls(all_splits[x]),"Whiffs"))
    Plot
  }
}

predict_whiff_model(10)

breaking_00_whiff <- predict_whiff_model(1)
breaking_01_whiff <- predict_whiff_model(4)
breaking_02_whiff <- predict_whiff_model(7)
breaking_10_whiff <- predict_whiff_model(10)
breaking_11_whiff <- predict_whiff_model(13)
breaking_12_whiff <- predict_whiff_model(16)
breaking_20_whiff <- predict_whiff_model(19)
breaking_21_whiff <- predict_whiff_model(22)
breaking_22_whiff <- predict_whiff_model(25)
breaking_30_whiff <- predict_whiff_model(28)
breaking_31_whiff <- predict_whiff_model(31)
breaking_32_whiff <- predict_whiff_model(34)

saveRDS(predict_whiff_model(1), "breaking_00_whiff.rds")
saveRDS(predict_whiff_model(4), "breaking_01_whiff.rds")
saveRDS(predict_whiff_model(7), "breaking_02_whiff.rds")
saveRDS(predict_whiff_model(10), "breaking_10_whiff.rds")
saveRDS(predict_whiff_model(13), "breaking_11_whiff.rds")
saveRDS(predict_whiff_model(16), "breaking_12_whiff.rds")
saveRDS(predict_whiff_model(19), "breaking_20_whiff.rds")
saveRDS(predict_whiff_model(22), "breaking_21_whiff.rds")
saveRDS(predict_whiff_model(25), "breaking_22_whiff.rds")
saveRDS(predict_whiff_model(28), "breaking_30_whiff.rds")
saveRDS(predict_whiff_model(31), "breaking_31_whiff.rds")
saveRDS(predict_whiff_model(34), "breaking_32_whiff.rds")

saveRDS(predict_whiff_model(2), "fastball_00_whiff.rds")
saveRDS(predict_whiff_model(5), "fastball_01_whiff.rds")
saveRDS(predict_whiff_model(8), "fastball_02_whiff.rds")
saveRDS(predict_whiff_model(11), "fastball_10_whiff.rds")
saveRDS(predict_whiff_model(14), "fastball_11_whiff.rds")
saveRDS(predict_whiff_model(17), "fastball_12_whiff.rds")
saveRDS(predict_whiff_model(20), "fastball_20_whiff.rds")
saveRDS(predict_whiff_model(23), "fastball_21_whiff.rds")
saveRDS(predict_whiff_model(26), "fastball_22_whiff.rds")
saveRDS(predict_whiff_model(29), "fastball_30_whiff.rds")
saveRDS(predict_whiff_model(32), "fastball_31_whiff.rds")
saveRDS(predict_whiff_model(35), "fastball_32_whiff.rds")

saveRDS(predict_whiff_model(3), "offspeed_00_whiff.rds")
saveRDS(predict_whiff_model(6), "offspeed_01_whiff.rds")
saveRDS(predict_whiff_model(9), "offspeed_02_whiff.rds")
saveRDS(predict_whiff_model(12), "offspeed_10_whiff.rds")
saveRDS(predict_whiff_model(15), "offspeed_11_whiff.rds")
saveRDS(predict_whiff_model(18), "offspeed_12_whiff.rds")
saveRDS(predict_whiff_model(21), "offspeed_20_whiff.rds")
saveRDS(predict_whiff_model(24), "offspeed_21_whiff.rds")
saveRDS(predict_whiff_model(27), "offspeed_22_whiff.rds")
saveRDS(predict_whiff_model(30), "offspeed_30_whiff.rds")
saveRDS(predict_whiff_model(33), "offspeed_31_whiff.rds")
saveRDS(predict_whiff_model(36), "offspeed_32_whiff.rds")




#swing Predict ----
predict_swing_model <- function(x)
{
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )  
  
  if(nrow(all_splits[[x]]) > 100)
  {
    fit <- gam(swing ~ s(plate_x,plate_z), family=binomial, data=all_splits[[x]])
    
    plate_x <- seq(-1.5, 1.5, length.out=100)
    plate_z <- seq(1.4, 3.75, length.out=100)
    data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                               plate_z = c(outer(plate_x * 0 + 1, plate_z)))
    lp <- predict(fit, data.predict)
    data.predict$Probability <- exp(lp) / (1 + exp(lp))
    
    # construct the plot V Same
    Plot <- ggplot(kZone, aes(x, y)) +
      theme_void() +
      geom_tile(data=data.predict, 
                aes(x=plate_x, y=plate_z, fill= Probability)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") +
      coord_fixed()+labs(title=paste(ls(all_splits[x]),"Swings"))
    Plot
  }
}


saveRDS(predict_swing_model(1), "breaking_00_swing.rds")
saveRDS(predict_swing_model(4), "breaking_01_swing.rds")
saveRDS(predict_swing_model(7), "breaking_02_swing.rds")
saveRDS(predict_swing_model(10), "breaking_10_swing.rds")
saveRDS(predict_swing_model(13), "breaking_11_swing.rds")
saveRDS(predict_swing_model(16), "breaking_12_swing.rds")
saveRDS(predict_swing_model(19), "breaking_20_swing.rds")
saveRDS(predict_swing_model(22), "breaking_21_swing.rds")
saveRDS(predict_swing_model(25), "breaking_22_swing.rds")
saveRDS(predict_swing_model(28), "breaking_30_swing.rds")
saveRDS(predict_swing_model(31), "breaking_31_swing.rds")
saveRDS(predict_swing_model(34), "breaking_32_swing.rds")

saveRDS(predict_swing_model(2), "fastball_00_swing.rds")
saveRDS(predict_swing_model(5), "fastball_01_swing.rds")
saveRDS(predict_swing_model(8), "fastball_02_swing.rds")
saveRDS(predict_swing_model(11), "fastball_10_swing.rds")
saveRDS(predict_swing_model(14), "fastball_11_swing.rds")
saveRDS(predict_swing_model(17), "fastball_12_swing.rds")
saveRDS(predict_swing_model(20), "fastball_20_swing.rds")
saveRDS(predict_swing_model(23), "fastball_21_swing.rds")
saveRDS(predict_swing_model(26), "fastball_22_swing.rds")
saveRDS(predict_swing_model(29), "fastball_30_swing.rds")
saveRDS(predict_swing_model(32), "fastball_31_swing.rds")
saveRDS(predict_swing_model(35), "fastball_32_swing.rds")

saveRDS(predict_swing_model(3), "offspeed_00_swing.rds")
saveRDS(predict_swing_model(6), "offspeed_01_swing.rds")
saveRDS(predict_swing_model(9), "offspeed_02_swing.rds")
saveRDS(predict_swing_model(12), "offspeed_10_swing.rds")
saveRDS(predict_swing_model(15), "offspeed_11_swing.rds")
saveRDS(predict_swing_model(18), "offspeed_12_swing.rds")
saveRDS(predict_swing_model(21), "offspeed_20_swing.rds")
saveRDS(predict_swing_model(24), "offspeed_21_swing.rds")
saveRDS(predict_swing_model(27), "offspeed_22_swing.rds")
saveRDS(predict_swing_model(30), "offspeed_30_swing.rds")
saveRDS(predict_swing_model(33), "offspeed_31_swing.rds")
saveRDS(predict_swing_model(36), "offspeed_32_swing.rds")

#wOBA Predict
all_predict_wOBA <- all_pitches_trim[FALSE,]
all_predict_wOBA$predict_wOBA <- numeric()

all_pitches_trim_BIP <- all_pitches_trim %>%
  filter(description %in% 
           c("hit_into_play", "hit_into_play_no_out","hit_into_play_score", "pitchout_hit_into_play_score"))

all_splits_BIP <- split(all_pitches_trim_BIP, with(all_pitches_trim_BIP, interaction(pitch_group,count)), drop = TRUE)

predict_wOBA_model <- function(x)
{
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )  
  
  if(nrow(all_splits_BIP[[x]]) > 100)
  {
    fit <- gam(woba_value ~ s(plate_x,plate_z), data=all_splits_BIP[[x]])
    
    plate_x <- seq(-1.5, 1.5, length.out=100)
    plate_z <- seq(1.4, 3.75, length.out=100)
    data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                               plate_z = c(outer(plate_x * 0 + 1, plate_z)))
    lp <- predict(fit, data.predict)

    
    # construct the plot V Same
    Plot <- ggplot(kZone, aes(x, y)) +
      theme_void() +
      geom_tile(data=data.predict, 
                aes(x=plate_x, y=plate_z, fill= lp)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") +
      coord_fixed()+labs(title=paste(ls(all_splits[x]),"wOBA"))
    Plot
  }
}

saveRDS(predict_wOBA_model(1), "breaking_00_woba.rds")
saveRDS(predict_wOBA_model(4), "breaking_01_woba.rds")
saveRDS(predict_wOBA_model(7), "breaking_02_woba.rds")
saveRDS(predict_wOBA_model(10), "breaking_10_woba.rds")
saveRDS(predict_wOBA_model(13), "breaking_11_woba.rds")
saveRDS(predict_wOBA_model(16), "breaking_12_woba.rds")
saveRDS(predict_wOBA_model(19), "breaking_20_woba.rds")
saveRDS(predict_wOBA_model(22), "breaking_21_woba.rds")
saveRDS(predict_wOBA_model(25), "breaking_22_woba.rds")
saveRDS(predict_wOBA_model(28), "breaking_30_woba.rds")
saveRDS(predict_wOBA_model(31), "breaking_31_woba.rds")
saveRDS(predict_wOBA_model(34), "breaking_32_woba.rds")

saveRDS(predict_wOBA_model(2), "fastball_00_woba.rds")
saveRDS(predict_wOBA_model(5), "fastball_01_woba.rds")
saveRDS(predict_wOBA_model(8), "fastball_02_woba.rds")
saveRDS(predict_wOBA_model(11), "fastball_10_woba.rds")
saveRDS(predict_wOBA_model(14), "fastball_11_woba.rds")
saveRDS(predict_wOBA_model(17), "fastball_12_woba.rds")
saveRDS(predict_wOBA_model(20), "fastball_20_woba.rds")
saveRDS(predict_wOBA_model(23), "fastball_21_woba.rds")
saveRDS(predict_wOBA_model(26), "fastball_22_woba.rds")
saveRDS(predict_wOBA_model(29), "fastball_30_woba.rds")
saveRDS(predict_wOBA_model(32), "fastball_31_woba.rds")
saveRDS(predict_wOBA_model(35), "fastball_32_woba.rds")

saveRDS(predict_wOBA_model(3), "offspeed_00_woba.rds")
saveRDS(predict_wOBA_model(6), "offspeed_01_woba.rds")
saveRDS(predict_wOBA_model(9), "offspeed_02_woba.rds")
saveRDS(predict_wOBA_model(12), "offspeed_10_woba.rds")
saveRDS(predict_wOBA_model(15), "offspeed_11_woba.rds")
saveRDS(predict_wOBA_model(18), "offspeed_12_woba.rds")
saveRDS(predict_wOBA_model(21), "offspeed_20_woba.rds")
saveRDS(predict_wOBA_model(24), "offspeed_21_woba.rds")
saveRDS(predict_wOBA_model(27), "offspeed_22_woba.rds")
saveRDS(predict_wOBA_model(30), "offspeed_30_woba.rds")
saveRDS(predict_wOBA_model(33), "offspeed_31_woba.rds")
saveRDS(predict_wOBA_model(36), "offspeed_32_woba.rds")

grid.arrange(readRDS("breaking_02_woba.rds"),
             readRDS("breaking_02_swing.rds"),
             readRDS("breaking_02_whiff.rds"),
             nrow = 1)


grid.arrange(readRDS("fastball_30_woba.rds"),
             readRDS("fastball_30_swing.rds"),
             readRDS("fastball_30_whiff.rds"),
             readRDS("breaking_30_woba.rds"),
             readRDS("breaking_30_swing.rds"),
             readRDS("breaking_30_whiff.rds"),
             readRDS("offspeed_30_woba.rds"),
             readRDS("offspeed_30_swing.rds"),
             readRDS("offspeed_30_whiff.rds"),
             ncol = 3,
             nrow = 3)
