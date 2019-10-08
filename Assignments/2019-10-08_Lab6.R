# tip: dont use summary tools, use summarize; explain why normality is/isn't met (if it isn't do sign test or mann-whitney u test)

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

#Normality was met but due to high ratio, using Welch's test
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
# less than 3 and normality met therefore can use two sample t-test
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

#Due to normality being met, can use one sample t-test
t.test(data02$biomassChange, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#Question: test whether there is a change in biomass of rainforest areas following clear-cutting
###There is a change in biomass of rainforest areas following clear-cutting (t = -0.85, df = 35, p = 0.3996)


#chpt 13 #26 ####
data03 <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv",col_names = TRUE)
                  
#choose an appropriate method and test whether females preferred one type of male over the other type




#review 2: #16 ####
data04 <-read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv", col_names = TRUE)



















