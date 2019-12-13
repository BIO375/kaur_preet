# chi squared and friends ####
rm(list = ls())
getwd()

# installing the new packages and loading tidyverse
install.packages("ggmosaic")
library("ggmosaic")
install.packages("epitools")
library("epitools")
library("tidyverse")
tidyverse_update()

### binomial test ####
# phratora laticollis 2 beetle population in bergen op zoom
49+41 # n = total number of trials
model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01
# We can conclude that females do not occur more frequently than expected in the Phratora laticollis 
# 2 population in Bergen op zoom (binomial test: P>0.05, n=90)

### Chi-squared goodness of fit ####
plant <- read_csv("datasets/demos/plant.csv", col_types = cols(color = col_factor())) 

model02 <-chisq.test(x = plant$observed, p = plant$expected_p)
model02

#We found that the observed yellow flower frequency was not 0.75 
#(Chi-squared goodness of fit: x2 = 4.32, df=1, p-value=0.03767).

### Contingency table analysis
tab01 <- matrix(c(30, 17, 41, 49), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab01) <- list("Locality" = c("Belgium", "Holland"),
                        "Sex" = c("Female", "Male"))
as.matrix(tab01)
model03 <- chisq.test(tab01, correct = FALSE)
model03

#There is a relationship between sex ratio and location, sex ratio is dependent on locality
# (Chi-squared contingency test: x2: 4.1299, df=1, p=0.04213).