rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

##Assignment
##QUESTION 1
obliquity <- read_csv("datasets/demos/obliquity.csv", col_names = TRUE)

## one-sample, two-sided t-test
t.test(obliquity$obliquity, 
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)




##QUESTION 2
HeartAttack <- read_csv("datasets/demos/HeartAttack_short.csv", col_names = TRUE, 
                        col_types = cols(
  group = col_character() )
  )

## summary stats
summ_cholesterol <- HeartAttack %>%
  group_by(group) %>% 
  summarise(mean_cholesterol = mean(cholest),
            sd_cholesterol = sd(cholest),
            n_cholesterol = n())
view(summ_cholesterol)

##plots
ggplot(HeartAttack) +
  geom_histogram(aes(cholest), binwidth = 30)+
  facet_wrap(~group)

ggplot(HeartAttack) +
  geom_boxplot(aes(x = group, y = cholest))

ggplot(HeartAttack)+
  geom_qq(aes(sample = cholest, color = group))

##ratio between std devs
ratio <-(max(summ_cholesterol$sd_cholesterol))/(min(summ_cholesterol$sd_cholesterol))

## two-sample
t.test(cholest ~ group, data = HeartAttack, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)




##QUESTION 3
Furness <- read_csv("datasets/quinn/chpt3/furness.csv", col_names = TRUE)

## Plots
ggplot(Furness) +
  geom_histogram(aes(METRATE), binwidth = 570)+
  facet_wrap(~SEX)

ggplot(Furness) +
  geom_boxplot(aes(x = SEX, y = METRATE))

ggplot(Furness)+
  geom_qq(aes(sample = METRATE, color = SEX))

## Mann-Whitney U test
wilcox.test(METRATE ~ SEX, data = Furness, alternative = "two.sided", conf.level = 0.95)





##QUESTION 4
##untidy form (tidy form will be elgar2)
Elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")

Elgar <- mutate(Elgar, diff = ##afterImplant - beforeImplant)##
##paired t-test









