# one way ANOVA lab
#clean up working environment
rm(list = ls())
#verify working directory
getwd()
# install packages; ggfortify works with ggplot2 to make nice plots, multcomp does contrasts
# and multiple comparison; nlme is used for random effects ANOVA
install.packages("ggfortify")
library("ggfortify")
install.packages("multcomp")
library("multcomp")
install.packages("nlme")
library("nlme")

# load tidyverse and check for updates
library("tidyverse")
tidyverse_update()

#read in dataset
data01 <- read.csv("datasets/demos/Jaffe.csv")

