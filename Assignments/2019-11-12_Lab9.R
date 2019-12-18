#### Lab 9: Correlation, Linear Regression #### 
#clean up working environment
rm(list = ls())
#verify working directory
getwd()

#load them libraries; reminder to only install packages once
library("ggfortify")
install.packages("broom")
library("broom")
library("tidyverse")
tidyverse_update()

#read in data
data01 <- read_csv("datasets/demos/fowler.csv")
#linear regression
model01 <- lm(YIELD ~ FERTILIZER, data = data01)
#residual by predicted plot
autoplot(model01, smooth.colour = NA)
#residual by x plot
data01 <- data01 %>%
  mutate(YIELD_resid = resid(model01))
ggplot(data = data01) +
  geom_point(aes(x = FERTILIZER, y = YIELD_resid))

#the statistical results
summary(model01)

#creating 95% confidence intervals
ggplot(data = data01, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()

#More fertilizer leads to a significantly higher yield of grass (Linear regression: YIELD = 
#51.933 + 0.811(FERTILIZER); df = 1, 8, F=94.04, P<0.0001), and fertilizer explained more than 
#90% of the variability in yield (R2 = 0.922). 


#### 10/10 code runs without breaking ####





