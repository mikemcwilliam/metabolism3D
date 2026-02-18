rm(list = ls())

library("ggplot2")
library("cowplot")
library("viridis")
library("reshape2")
library("lubridate")
library("vegan")
library("psych")
library("png")
library("grid")

######################################################
#---------------------------------------------#  data

c1 <- read.csv("data/output/metricsCS.csv")
c2 <- read.csv("data/output/metricsLTMP.csv")

ggplot(c2, aes(f.biomass))+geom_histogram()+scale_x_log10()
