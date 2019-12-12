rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

#scenario 1
data01 <- read_csv("datasets/demos/countrybirthrate.csv")

#summary stats
summ_d <- data01 %>%
  summarise(n_d = n(),
            mean_d = mean(d),
            median_d = median(d),
            IQR_d = IQR(d),
            sd_d = sd(d),
            var_d = var(d))

View(summ_d)

#boxplot
ggplot(data01)+
  geom_boxplot(aes(x = "", y = d), notch = FALSE, varwidth = TRUE)

#histogram
ggplot(data01) +
  geom_histogram(aes(d), binwidth = 4)


#scenario 2
data02 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data02 <- data02 %>% slice (-105)


#summary stats
summ_HornLength <- data02 %>%
  group_by(Survival) %>% 
  summarise(n_HornLength = n(),
            mean_HornLength = mean(squamosalHornLength),
            median_HornLength = median(squamosalHornLength),
            IQR_HornLength = IQR(squamosalHornLength),
            sd_HornLength = sd(squamosalHornLength),
            var_HornLength = var(squamosalHornLength))

View(summ_HornLength)

#boxplot
ggplot(data02)+
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = FALSE, varwidth = TRUE)

#histogram
ggplot(data02) +
  geom_histogram(aes(squamosalHornLength), binwidth = 2)+
  facet_wrap(~Survival)

### 10/10 Code runs without breaking ####