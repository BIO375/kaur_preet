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

#plot data
ggplot(data02, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data02) +
  geom_histogram(aes(conemass), binwidth = 1)+
                   facet_wrap(~habitat)
ggplot(data02)+
  geom_qq(aes(sample = conemass, color = habitat))

#assumption of normality met (medians are about central for all three plots and whiskers are about equal
# lengths as well for island.absent. mainland.present is a bit left skewed because right whisker is a bit
# longer than left and median is a bit more to the right, and island.present is a little right skewed
# because left whisker is longer than right and median is a bit more to the left but both are still pretty
#central with no outliers. also qq plot lines are pretty linear), fixed effect anova model
model02 <- lm(conemass~habitat, data = data02)

#check homogeneity assumption
summ_conemass <- data02 %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())
ratio02 <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))
#ratio is 1.3354 which is less than 3 thus assumption is met
#see residuals vs fitted plot
autoplot(model02)

anova(model02)
# F2,13 = 50.085, p-value < 0.0001 (a little confused as to why the df residual is 13 and not 14)
summary(model02)

#this comparison is a planned comparison
planned02 <- glht(model02, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
confint(planned02)
summary(planned02)
# The mean cone size (mass) on islands where squirrels were absent is significantly greater than islands 
# where squirrels were present (planned comparison, t=-8.596, p<0.0001).


#### Ch 15 Problem 26, Use data to perform the correct test, show code for all steps in your process ####
data03 <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor()))
#plot and check normality
ggplot(data03, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(data03) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 0.9)+
  facet_wrap(~treatmentGroup)
ggplot(data03)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))
# For the control and WT the median was central, with WT having a longer left whisker than right but still
#relatively normal.Scorpine had a median that was slightly to the right but whiskers than were about equal
#lengths, however there were two outliers. All in all relatively normal - assumption met. 

#check homogeneity assumption
summ_logSporozoiteNumbers <- data03 %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio03 <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))
# ratio was 2.8849 which is less than 3 therefore assumption of homogeneity is met
#anova fixed effects model
model03 <- lm(logSporozoiteNumbers~treatmentGroup, data = data03)

#check residuals versus fitted plot
autoplot(model03)
anova(model03)
summary(model03)
# 


#### Ch 15 Problem 30, Use data to perform the correct test, show code for all steps in your process ####
data04 <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor()))
data04 <- slice(data04, -85)





