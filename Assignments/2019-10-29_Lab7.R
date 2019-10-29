# one way ANOVA lab
#clean up working environment
rm(list = ls())
#verify working directory
getwd()
# install packages

library("tidyverse")
tidyverse_update()

#read in dataset
data01 <- read.csv("datasets/demos/Jaffe.csv")
install.packages("rlang")

