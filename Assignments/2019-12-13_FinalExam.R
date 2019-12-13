#Final Exam ####
rm(list = ls())
getwd()

#getting all the directories in! 
library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()


#Scenario 1 (linear regression)####
insulation <- read_csv("datasets/final/insulation.csv")
##checking assumptions
#### autoplot: residuals by fitted plot and normal qq plot
modelinsulation <- lm(heat_loss ~ leanness, data = insulation)
autoplot(modelinsulation, smooth.colour = NA)
#### residuals by x plot
insulation <- insulation %>%
  mutate(Resid_heatloss = resid(modelinsulation))
ggplot(data = insulation) +
  geom_point(aes(x = leanness, y = Resid_heatloss))

#statistical results
summary(modelinsulation)

# adding in 95% confidence intervals
ggplot(data = insulation, aes(x = leanness, y = heat_loss)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Leanness", y = "Heat Loss (change in body temp/min spent swimming)")

#Scenario 2 (1-way ANOVA (fixed))####
caffeine  <- read_csv("datasets/final/caffeine.csv", col_types = cols(
  group = col_factor() ))
#checking assumptions
# normality
ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 4)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))
# homogeneity of variability
summ_halflife <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_halflife = mean(half_life),
            sd_halflife = sd(half_life),
            n_halflife = n())
ratiocaffeine <-(max(summ_halflife$sd_halflife))/(min(summ_halflife$sd_halflife))

#construct ANOVA
modelcaffeine <- lm(half_life~group, data = caffeine)
autoplot(modelcaffeine)

#results
anova(modelcaffeine)
summary(modelcaffeine)

#planned comparison
plannedcaffeine <- glht(modelcaffeine, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "high_prog - norm_prog = 0"
                              )))
confint(plannedcaffeine)
summary(plannedcaffeine)

#Scenario 3 ####




