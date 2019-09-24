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


#scenario 2
data02 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data02 <- data02 %>% slice (-105)