##Problem 33
rm(list = ls())
getwd()

library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

#a) 
# i) H0 = there is no relationship between chest pain and death
# ii) predictor: chest pain (categorical nominal); response: death (categorical nominal)
# iii) x2 contingency
chestpain <- read_csv("datasets/FinalPractice/ChestPain.csv") 
# unneccesary for this shit: ggplot(data = chestpain) +
  # geom_mosaic(aes(x = product(chestPain, survival), fill=chestPain), na.rm=TRUE)

tab01 <- matrix(c(822, 229, 18296, 1534), 2, 2, byrow=TRUE)
dimnames(tab01) <- list("survival" = c("Died", "Survived"),
                        "chestPain" = c("Chest Pain", "No Chest Pain"))
as.matrix(tab01)
model01 <- chisq.test(tab01, correct = FALSE)
model01
# We found that there is a relationship between death and chest pain 
# (x2 = 254.99, df = 1, p-value <0.01)

#b)
# i) h0 = uNT = uT
# ii) predictor: territory (categorical nominal) response: levels mRNA of gnrh (numerical cont)
# iii) two tailed two sample t test
cichlid <- read_csv("datasets/FinalPractice/Cichlid.csv") 

summ_cichlid <- cichlid %>%
  group_by(territorialStatus) %>% 
  summarise(mean_rnalevel = mean(GnRHmRNALevel),
            median_rnalevel = median(GnRHmRNALevel),
            IQR_rnalevel = IQR(GnRHmRNALevel),
            sd_rnalevel = sd(GnRHmRNALevel),
            var_rnalevel = var(GnRHmRNALevel))

View(summ_cichlid)

ratio01 <-(max(summ_cichlid$sd_rnalevel))/(min(summ_cichlid$sd_rnalevel))

ggplot(cichlid) +
  geom_histogram(aes(GnRHmRNALevel), binwidth = 1)+
  facet_wrap(~territorialStatus)

ggplot(cichlid) +
  geom_boxplot(aes(x = territorialStatus, y = GnRHmRNALevel))

ggplot(cichlid)+
  geom_qq(aes(sample = GnRHmRNALevel, color = territorialStatus ))


#normality not met because the qq plot was not linear and boxplots were skewed, 
#variance not met b/c ratio is higher than 3
# therefore need to mutate
cichlid <- cichlid %>%
  mutate(logGnRH = log(GnRHmRNALevel))

ggplot(cichlid) +
  geom_histogram(aes(logGnRH), binwidth = 1)+
  facet_wrap(~territorialStatus)

ggplot(cichlid) +
  geom_boxplot(aes(x = territorialStatus, y = logGnRH))

ggplot(cichlid)+
  geom_qq(aes(sample = logGnRH, color = territorialStatus ))

summ_cichlid2 <- cichlid %>%
  group_by(territorialStatus) %>% 
  summarise(sd_loggnrh = sd(logGnRH))

View(summ_cichlid2)

ratio02 <-(max(summ_cichlid2$sd_loggnrh))/(min(summ_cichlid2$sd_loggnrh))

#assumptions met
t.test(logGnRH ~ territorialStatus, data = cichlid, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#We found that there is a significant difference between the mRNA level of GnRH for territorial fish and for 
#non-territorial fish(two tailed two sample t-test: t=-2.9032, df= 9, p=0.01751).

#c)
# i) 

#d) correlation
# i) h0 = p=0
# ii) both (response) variables are numerical cont: number of offspring and lifespan
# iii) correlation
worm <- read_csv("datasets/FinalPractice/worm.csv")

ggplot(data = worm) +
  geom_point(mapping = aes(x = lifespan, y = relativeOffspringNumber ))
ggplot(data = worm)+
  geom_histogram(aes(lifespan), binwidth = 5)
ggplot(data = worm)+
  geom_histogram(aes(relativeOffspringNumber), binwidth = 4)
ggplot(data = worm)+
  geom_boxplot(aes("", relativeOffspringNumber))
ggplot(data = worm)+
  geom_qq(aes(sample = relativeOffspringNumber))
ggplot(data = worm)+
  geom_boxplot(aes("", lifespan))
ggplot(data = worm)+
  geom_qq(aes(sample = lifespan))
#lifespan one is a little iffy so transform lifespan
worm <- worm %>%
  mutate(log_life = log(lifespan))
ggplot(data = worm) +
  geom_point(mapping = aes(x = log_life, y = relativeOffspringNumber ))
ggplot(data = worm)+
  geom_boxplot(aes("", log_life))
ggplot(data = worm)+
  geom_qq(aes(sample = log_life))
# lifespan still doesn't look good
r <- wormSpear$estimate
r

wormSpear <-cor.test(~ relativeOffspringNumber + log_life, data = worm,
                      method = "spearman")
wormSpear
# There is a negative correlation between lifespan and relative offspring number
#(Spearmans Rank Correlation; S= 726.6, p-value = 0.02422, r=-0.60).

#e) 
# i) h0= u1=u2=u3...=u34
# ii) predictor: mouse (cat nom);
# ii) response: max rate of oxygen consumption(num cont)
# iii)random effects anova
oxygen <- read_csv("datasets/FinalPractice/oxygen.csv", col_types = cols(mouse = col_factor() ) )

summ_oxygen <- oxygen %>%
  group_by(mouse) %>%
  summarise(mean_O2 = mean(VO2max),
            n_O2 = n())

ggplot(summ_oxygen) +
  geom_histogram(aes(mean_O2), binwidth = 0.2)
ggplot(summ_oxygen) +
  geom_boxplot(aes(x = "", y = mean_O2))
ggplot(summ_oxygen)+
  geom_qq(aes(sample = mean_O2))



model02 <- lme(fixed = VO2max ~ 1,
               random = ~1|mouse, data = oxygen)

model02_varcomp <- VarCorr(model02)
model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )

varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability

#e
# i) H0: The proportion of YS:YW:GS:GW is 9/16:3/16:3/16:1/16
# ii) phenotype (categorical; 4 levels: YW YS GS GW) 
# iii) chi square goodness of fit
peas <- read_csv("datasets/FinalPractice/peas.csv")
peas_short <- tribble (
  ~phenotype, ~obs_freq, ~exp_prop,
  #--|--|----
  "Yellow smooth", 315, 9/16,
  "Yellow wrinkled", 101, 3/16,
  "Green smooth", 108, 3/16,
  "Green wrinkled", 32, 1/16
)

model03 <-chisq.test(x = peas_short$obs_freq, p = peas_short$exp_prop)
model03

# The expected and observed frequencies are not significantly different (x2 = 0.47002, df=3, p-value = 0.92454).









