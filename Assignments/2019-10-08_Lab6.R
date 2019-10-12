rm(list = ls())
getwd()
install.packages("tidyverse") 
library("tidyverse")
tidyverse_update()
library("DescTools")
install.packages("Hmisc")
library(Hmisc)

#chapter 13: 20, 25, 26
#Review probs 2: 16
#Due sunday 13th

# chpt 13 #20 ####
data01<-read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv",col_names = TRUE)
##summary stats
summ_skincolor <- data01 %>%
  group_by(species) %>% 
  summarise(n_skincolor = n(),
            mean_skincolor = mean(skinColor),
            median_skincolor = median(skinColor),
            IQR_skincolor = IQR(skinColor),
            sd_skincolor = sd(skinColor),
            var_skincolor = var(skinColor),
            se_skincolor = sd(skinColor)/sqrt(length(skinColor)))

View(summ_skincolor)

#ratio test, if less than 3 then can use t-test b/c meets assumption of similar variances, if not, then have to use Welch's test
ratio01 <-(max(summ_skincolor$sd_skincolor))/(min(summ_skincolor$sd_skincolor))

#Is normality met? using plots to determine:
ggplot(data01) +
  geom_histogram(aes(skinColor), binwidth = .4)+
  facet_wrap(~species)

ggplot(data01) +
  geom_boxplot(aes(x = species, y = skinColor))

ggplot(data01)+
  geom_qq(aes(sample = skinColor, color = species))

#Normality was met as the boxplot showed lines central to the box and the lines in the qq plot were quite straight, but due to high ratio (4.30, which is greater than 3), using Welch's test
t.test (skinColor ~ species, data = data01, alternative = "two.sided", conf.level = 0.95)

# transformation
data01 <- mutate(data01, log_skincolor = log(skinColor))
# summary stats of transformed data
summ_logskincolor <- data01 %>%
  group_by(species) %>% 
  summarise(n_logskincolor = n(),
            mean_logskincolor = mean(log_skincolor),
            median_logskincolor = median(log_skincolor),
            IQR_logskincolor = IQR(log_skincolor),
            sd_logskincolor = sd(log_skincolor),
            var_logskincolor = var(log_skincolor),
            se_logskincolor = sd(log_skincolor)/sqrt(length(log_skincolor)))

View(summ_logskincolor)
# ratio of transformed data to test assumption of similar variances
ratio02 <-(max(summ_logskincolor$sd_logskincolor))/(min(summ_logskincolor$sd_logskincolor))
# testing normality through plots
ggplot(data01) +
  geom_histogram(aes(log_skincolor), binwidth = .2)+
  facet_wrap(~species)

ggplot(data01) +
  geom_boxplot(aes(x = species, y = log_skincolor))

ggplot(data01)+
  geom_qq(aes(sample = log_skincolor, color = species))
# ratio was less than 3 (2.62) and normality met (the median was very close to the center of the IQR box, lines in qq plot were close to being straight, histogram showed a normal (bell curve) looking plot)) therefore can use two sample t-test
t.test(log_skincolor ~ species, data = data01, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#a. list two methods that would be appropriate to test whether there was a difference in mean skin color between the two groups
### Welch's test and 2 sample t-test with transformed data
#b. use a transformation to test if there is a difference in mean skin color between groups. Is there a difference?
### There is a significant difference between the mean skin color of kokanee and sockeye. (t=12.133, df =33, p = 0.0001)

  
#chpt 13 #25 #### 
data02 <-read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv", col_names = TRUE)
#one sample two tailed should be used because comparing change to a value of no change (0)
#tesing normality assumption
ggplot(data02) +
  geom_histogram(aes(biomassChange), binwidth = 3)

ggplot(data02) +
  geom_boxplot(aes(x = "", y = biomassChange))

ggplot(data02)+
  geom_qq(aes(sample = biomassChange))

#Due to normality being met(boxplot - middle median line was in the middle of the IQR box, histogram and boxplot show a bit of a left skew with outliers below the box and a longer whisker below the box, also the bell shape is more the right on the histogram, qq plot is a little rounded but is still pretty straight), can use one sample t-test
t.test(data02$biomassChange, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#Question: test whether there is a change in biomass of rainforest areas following clear-cutting
###There is a change in biomass of rainforest areas following clear-cutting (t = -0.85, df = 35, p = 0.3996)


#chpt 13 #26 ####
data03 <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv",col_names = TRUE)

#checking for assumptions of normality
ggplot(data03) +
  geom_histogram(aes(preference), binwidth = 10)

ggplot(data03) +
  geom_boxplot(aes(x = "", y = preference))

ggplot(data03)+
  geom_qq(aes(sample = preference))

# normality not met: the histogram does not show much because of low sample size, the boxplot shows that the upper whisker is a little longer than the bottom (indicating a bit of a postive skew) and median is lower than normal but still towards the middle, the qq plot looks a bit like a sigmoid graph, will use sign test because want to be conservative as the plots were not exactly normally distributed
# One-sided, HA that preference for male with higher cartenoid diet is higher than preference for male with low cartenoid diet for females
SignTest(data03$preference, alternative = "greater", mu = 0, conf.level = 0.95)

                  
#choose an appropriate method and test whether females preferred one type of male over the other type
### Should use paired t-test because the same experiemental group is used for both treatments (same females used for high dietary cartenoid male and diet low in cartenoids male).
### used sign test because normality was not met, sign test showed that females preferred to sit with the male who had a high cartenoid diet than with the male that had a low cartenoid diet (S = 10, n = 10, p = 0.0009766)


#review 2: #16 ####
data04 <-read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv", col_names = TRUE)
# two sided two sample t-test
# summarize to get sd
summ_SAA <- data04 %>%
  group_by(genotype) %>% 
  summarise(n_SAA = n(),
            mean_SAA = mean(secondsAggressiveActivity),
            sd_SAA = sd(secondsAggressiveActivity)
            )

View(summ_SAA)
# use sd to see what the ratio is, which tells if data meets assumption of homoscedasticity
ratio03 <-(max(summ_SAA$sd_SAA))/(min(summ_SAA$sd_SAA))
# see if assumption of normality is met
ggplot(data04) +
  geom_histogram(aes(secondsAggressiveActivity), binwidth = 40)+
  facet_wrap(~genotype)

ggplot(data04) +
  geom_boxplot(aes(x = genotype, y = secondsAggressiveActivity))

ggplot(data04)+
  geom_qq(aes(sample = secondsAggressiveActivity, color = genotype))

#ratio is less than 3, therefore variability is low; normality assumption is also met because the boxplot shows the middle median line is in the middle for spd and is a little high for wild type but still towards the middle and the qq plots both have similar shapes that look linear, therefore can use two-sample t-test
t.test(secondsAggressiveActivity ~ genotype, data = data04, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#a. estimate the magnitude of the effect of the mutation (diff between means) on the amount of time spent in aggressive activity. put appropriate bounds on your estimate of the effect 
### The confidence interval is 25.93 to 110.26 which is a very broad range. 
#b. what is the weight of evidence that this effect is not zero? (pvalue) perform appropriate stat test of diff
### The weight of evidence that the effect is not zero is 0.003142 (the p-value). There is a difference between individuals who had the gene and those who did not in terms of how long they will aggressively act when shown a mirror (t = 3.3802, df = 19, p-value = 0.003142).

















