# question - what is diff between read.csv and read_csv 
# one way ANOVA lab
#clean up working environment
rm(list = ls())
#verify working directory
getwd()
# install packages; ggfortify works with ggplot2 to make nice plots, multcomp does contrasts
# and multiple comparisons; nlme is used for random effects ANOVA
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
data01 <- read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))
#look at data
head(data01)
summary(data01)

# Step 1: Plot data ####
# for depth versus aldrin
ggplot(data01, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data01) +
  geom_histogram(aes(Aldrin), binwidth = 10)+
  facet_wrap(~Depth)
ggplot(data01)+
  geom_qq(aes(sample = Aldrin, color = Depth))
# for depth versus HCB
ggplot(data01, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data01) +
  geom_histogram(aes(HCB), binwidth = 10)+
  facet_wrap(~Depth)
ggplot(data01)+
  geom_qq(aes(sample = HCB, color = Depth))
# assumption of normality not met for Aldrin, therefore do log10 transformation
data02 <-mutate(data01, Aldrin = log10(Aldrin))
# for depth versus mutated aldrin
ggplot(data02, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data02) +
  geom_histogram(aes(Aldrin), binwidth = 10)+
  facet_wrap(~Depth)
ggplot(data02)+
  geom_qq(aes(sample = Aldrin, color = Depth))
# test variability
summ_aldrin <- data02 %>%
  group_by(Depth) %>% 
  summarise(mean_aldrin = mean(Aldrin),
            sd_aldrin = sd(Aldrin),
            n_aldrin = n())
ratio_aldrin <-(max(summ_aldrin$sd_aldrin))/(min(summ_aldrin$sd_aldrin))

summ_hcb <- data02 %>%
  group_by(Depth) %>% 
  summarise(mean_hcb = mean(HCB),
            sd_hcb = sd(HCB),
            n_hcb = n())
ratio_hcb <-(max(summ_hcb$sd_hcb))/(min(summ_hcb$sd_hcb))
# variability < 3 for both hcb and aldrin thus homogeneity of variances assumption met

# Step 2: Construct ANOVA ####
model_aldrin01 <- lm(Aldrin~Depth, data = data01)

model_aldrin02 <- lm(Aldrin~Depth, data = data02)

model_hcb <- lm(HCB~Depth, data = data02)

# Step 3: Check Assumptions Again ####
# autoplot function will give a residuals by predicte plot
autoplot(model_aldrin01)
autoplot(model_aldrin02)
autoplot(model_hcb)
# only look at the top two plots (residuals vs fitted and normal q-q); both lines were relatively straight

# Step 4: Interpret results ####
anova(model_aldrin01)
anova(model_aldrin02)
anova(model_hcb)















