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

### Chi-squared goodness of fit ####
tab02 <- matrix(c(84, 0.75, 16, 0.25), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab02) <- list("Flower" = c("Yellow", "Green"),
                        "Number" = c("Observed", "Expectedp"))
as.matrix(tab02)
model02 <-chisq.test(x= tab02$Observed, p = tab02$Expectedp)
model02


### Contingency table analysis
tab01 <- matrix(c(30, 17, 41, 49), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab01) <- list("Locality" = c("Belgium", "Holland"),
                        "Sex" = c("Female", "Male"))
as.matrix(tab01)
model03 <- chisq.test(tab01, correct = FALSE)
model03
