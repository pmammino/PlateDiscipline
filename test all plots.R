install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mgcv")
install.packages("REdaS")
install.packages("gridExtra")
install.packages("lme4")
install.packages('reshape')
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
options(scipen = 999)
memory.limit(100000)


#Swing Predict ----
all_pitches_trim <- readRDS("all_pitches_trim.rds")
all_splits <- split(all_pitches_trim, with(all_pitches_trim, interaction(pitch_group,count)), drop = TRUE)

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
    fit <- gam(whiff ~ s(plate_x,plate_z), family=binomial, data=all_splits[[1]])
    
    plate_x <- seq(-1.5, 1.5, length.out=100)
    plate_z <- seq(1.4, 3.75, length.out=100)
    data.predict <- data.frame(plate_x = c(outer(plate_x, plate_z * 0 + 1)),
                               plate_z = c(outer(plate_x * 0 + 1, plate_z)))
    lp <- predict(fit, data.predict)
    data.predict$Probability <- exp(lp) / (1 + exp(lp))
    
    # construct the plot V Same
    Plot <- ggplot(kZone, aes(x, y)) +
      geom_tile(data=data.predict, 
                aes(x=plate_x, y=plate_z, fill= Probability)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") +
      coord_fixed()+labs(title=paste(ls(all_splits[1]),"Whiffs"))
    Plot
  }
}


all_plots <- lapply(1:length(all_splits), predict_whiff_model)
