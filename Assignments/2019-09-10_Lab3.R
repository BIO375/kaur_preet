### Lab 3. Data manipulation and graphing

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# Read in data file
ward_data<-read_csv("datasets/quinn/chpt3/ward.csv", col_names = TRUE)

# Pasted from Import Dataset Tool
library(readr)
ward <- read_csv("datasets/quinn/chpt3/ward.csv")
View(ward)
# Note that for us, library(readr) is redundant because we loaded it with
# all the other tidyverse packages earlier
library(readr)
ward <- read_csv("datasets/quinn/chpt3/ward.csv")

# Read in compensation data file
compensation<-read_csv("datasets/demos/compensation.csv")

# names() tells you the names assigned to each column, generally variable
# names
names(ward)

# head() gives you the first six rows of a dataset
head(ward)

# dim() gives you the dimensions of your dataset
dim(ward)

# str() returns the structure of the dataset
str(ward)

# Calculate summary statistics about groups.  I give the general form below
# in comments

# <new_object_name> <- <data> %>%
# group_by(<grouping_variable>) %>%
# summarise(
# mean_resp = mean(<response_variable_name>),
# median_resp = median(<response_variable_name>),
# IQR_resp = IQR(<response_variable_name>),
# sd_resp = sd(<response_variable_name>),
# var_resp = var(<response_variable_name>)
# )

summ_eggs <- ward %>%
group_by(ZONE) %>% 
  summarise(n_eggs = n(),
            mean_eggs = mean(EGGS),
            median_eggs = median(EGGS),
            IQR_eggs = IQR(EGGS),
            sd_eggs = sd(EGGS),
            var_eggs = var(EGGS))

View(summ_eggs)

# mutate() adds new variables while preserving existing ones.  General form:
# <dataset_name> <- mutate(<dataset_name>, <transform_variable_name> =
# <mathematical_function>(<variable_name>))
ward<-mutate(ward, squareroot_eggs = sqrt(EGGS))

compensation<-mutate(compensation, log(Root))

# R for Data Science, Chapter 3
# https://r4ds.had.co.nz/data-visualisation.html
# Enter your code here

library(tidyverse)
ggplot2::ggplot()
ggplot2::mpg
?mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "green")


# Compare the histograms and boxplots of EGGS and squareroot_eggs
ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)
ggplot(ward) +
  geom_histogram(aes(squareroot_eggs), binwidth = 0.5)+
  facet_wrap(~ZONE)

ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = EGGS), notch = TRUE, varwidth = TRUE)
ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = squareroot_eggs), notch = TRUE, varwidth = TRUE)

### Assignment

# Load the sanchez.csv file
# Enter your code here

sanchez<-read_csv("datasets/demos/sanchez.csv")


# Calculate summary statistics
# Enter your code here
names(sanchez)

head(sanchez)
dim(sanchez)
str(sanchez)

summ_BEETLE96 <- sanchez %>%
  group_by(birdcolony) %>% 
  summarise(n_BEETLE96 = n(),
            mean_BEETLE96 = mean(BEETLE96),
            median_BEETLE96 = median(BEETLE96),
            IQR_BEETLE96 = IQR(BEETLE96),
            sd_BEETLE96 = sd(BEETLE96),
            var_BEETLE96 = var(BEETLE96),
            se_BEETLE96 = sd(BEETLE96)/sqrt(length(BEETLE96)))

View(summ_BEETLE96)





# Add a new column of log(y+1) transformed beetle densities to the sanchez dataset
# Enter your code here
# <dataset_name> <- mutate(<dataset_name>, <transform_variable_name> =
# <mathematical_function>(<variable_name>))
sanchez <- mutate(sanchez, log_BEETLE96 = log(BEETLE96+1))



# Generate histograms of beetle density by colony type before and after data 
# transformation
# Enter your code here

ggplot(sanchez) +
  geom_histogram(aes(BEETLE96), binwidth = 11)+
  facet_wrap(~birdcolony)

ggplot(sanchez) +
  geom_histogram(aes(log_BEETLE96), binwidth = 2)+
  facet_wrap(~birdcolony)


# Plot boxplots of beetle density by colony type before and after data 
# transformation
# Enter your code here
ggplot(sanchez)+
  geom_boxplot(aes(x = birdcolony, y = BEETLE96), notch = FALSE, varwidth = TRUE)
ggplot(sanchez)+
  geom_boxplot(aes(x = birdcolony, y = log_BEETLE96), notch = FALSE, varwidth = TRUE)

### CODE RUNS CORRECTLY 10/10 ####
