rm(list = ls())
getwd()

#load ggfortify, multcomp, nlme, and tidyverse and check if tidyverse has updates
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()


#### Ch 15 Problem 22, Complete parts a, b, c, d #### 
data01 <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", col_types = cols(
  specimen = col_factor()))

#plot data
ggplot(data01, aes(x = "", y = headwidth))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data01) +
  geom_histogram(aes(headwidth), binwidth = 0.02)
ggplot(data01)+
  geom_qq(aes(sample = headwidth, color = ""))

#assumption of normality met, random effects anova model
model01 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = data01)
#variance among and within groups (among is labeled intercept and within is labeled residual in code)
model01_varcomp <- VarCorr(model01)
model01_varcomp
#variance among groups is 0.0002459167; variance within groups is 0.000166
#repeatability
varAmong01  <- as.numeric( model01_varcomp[1,1] )
varWithin01 <- as.numeric( model01_varcomp[2,1] )
repeatability <- varAmong01 / (varAmong01 + varWithin01)
repeatability
#the repeatability is 0.5970059
#The repeatability for example 15.6 is 0.75, therefore in comparison to this data, example 15.6 
#has higher repeatability and is less affected by measurement error.

#### Ch 15 Problem 23, Complete part a only ####
data02 <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor()))

ggplot(data02, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data02) +
  geom_histogram(aes(conemass), binwidth = 1)+
                   facet_wrap(~habitat)
ggplot(data02)+
  geom_qq(aes(sample = conemass, color = habitat))

model02 <- lm(conemass~habitat, data = data02)

summ_conemass <- data02 %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())
ratio02 <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))

autoplot(model02)

anova(model02)

summary(model02)

#this comparison is a planned comparison
planned02 <- glht(model02, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
confint(planned02)
summary(planned02)
# The mean cone size of squirrels absent on islands is significantly less than squirrels present on islands 
#(planned comparison, t=-8.596, p<0.001).


#### Ch 15 Problem 26, Use data to perform the correct test, show code for all steps in your process ####
data03 <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor()))




#### Ch 15 Problem 30, Use data to perform the correct test, show code for all steps in your process ####
data04 <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor()))
data04 <- slice(data04, -85)





