library(tidyverse)
library(openxlsx)
library(scales)
library(mgcv)
library(lubridate)

options(scipen = 999) # scientific notation
# plot parameter
lwd_size <- 1.8 # size of geom_line
point_size <- 4
title_size <- 20
text_size <- 15

col5  <- c("#FF0066", "#CB6BCEFF","#1EA7C4", "#9F272EFF","#EC9E58FF")