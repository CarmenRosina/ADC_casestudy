##################################
## ADC Case Study - Credit Scoring
## Variable Selection
## 16-12-2021
## Carmen Wolvius
## carmen.wolvius@gmail.com
##################################

#### Load Libraries
library(here)
library(tidyverse)

library(naniar)
library(Information)

#### Load data
training.data.raw <- read.csv(here("data/raw/titanic", "train.csv"), 
                              header=T,
                              na.strings=c(""))

### Data Quality Check

## Covariance - Check parch and SibSp as they have low variability
cov(training.data.raw %>% select_if(is.numeric))

## Missing values

# 2 variables have noticeable missing values: Age (177) & Cabin (687)
sapply(training.data.raw, function(x) sum(is.na(x)))

sapply(training.data.raw, function(x) length(unique(x)))

# Visual representation:

# Show missing with percentage
vis_miss(training.data.raw)

# See if missings are at random
gg_miss_upset(training.data.raw)

# Imputing Age variable
training.data.raw <- training.data.raw %>% 
  mutate(Pclass = factor(Pclass)) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(Embarked = factor(Embarked))

# Compute the analysis of variance
res.aov <- aov(Age ~ Pclass, data = training.data.raw)

# Summary of the analysis - Significant difference in age between classes
summary(res.aov)

# Imputation of age dependent on ticket class
training.data.raw <- training.data.raw %>% 
  mutate(Age = replace(Age, is.na(Age) & Pclass == 1, mean(training.data.raw[training.data.raw$Pclass==1, "Age"], na.rm=TRUE))) %>% 
  mutate(Age = replace(Age, is.na(Age) & Pclass == 2, mean(training.data.raw[training.data.raw$Pclass==2, "Age"], na.rm=TRUE))) %>% 
  mutate(Age = replace(Age, is.na(Age) & Pclass == 3, mean(training.data.raw[training.data.raw$Pclass==3, "Age"], na.rm=TRUE)))

# Check how R is handling the categorical variables
contrasts(training.data.raw$Sex)
contrasts(training.data.raw$Embarked)

# CONCLUSION: Discard variables based on missing values, and drop the ID - remove rows with missings in Embarked (only 2)
data <- training.data.raw %>%
  select(-c(Cabin, PassengerId, Ticket, Name)) %>%
  filter(!is.na(Embarked))

## Outliers

# Numerical variables: Age, Fare

hist(data$Age,
     xlab = "Age",
     main = "Histogram of Age",
     breaks = sqrt(nrow(data))
) # set number of bins

hist(data$Fare,
     xlab = "Age",
     main = "Histogram of Fare",
     breaks = sqrt(nrow(data))
) # set number of bins


# outliers based on quantiles (no weird values for age)
lower_bound <- quantile(data$Age, 0.025)
upper_bound <- quantile(data$Age, 0.975)

# for Fare quantiles seem to strict
lower_bound <- quantile(data$Fare, 0.025)
upper_bound <- quantile(data$Fare, 0.975)

# Also based on 3 sds larger values do not seem strange 
mean(data$Fare) + (sd(data$Fare)*3)

# CONCLUSION -> no outliers removed

### Information value check - both Parch and SibSp seem to have low predictive power
create_infotables(data=data, y="Survived", bins=10, parallel=FALSE)

# CONCLUSTION -> As the number of variables is low, and the predictive power is now too weak the variables will not be excluded

# ? binning

### Trend Check

# Use distributions to discover trends different to the dependent variable  
data %>%
  ggplot(aes(Age)) +
  geom_histogram(binwidth = 1.25, color = "black",fill = "grey") +
  labs(title = "Distribution of age relative to survival",
       x = "Age",
       y = "Survived") +
  theme_minimal() +
  facet_grid(Survived~.)

data %>%
  ggplot(aes(Fare)) +
  geom_histogram(binwidth = 1.25, color = "black",fill = "grey") +
  labs(title = "Distribution of the fare relative to survival",
       x = "Fare",
       y = "Survived") +
  theme_minimal() +
  facet_grid(Survived~.)

### Correlation Check
cor(data %>% select_if(is.numeric))

### Penalised Regression
# https://cran.r-project.org/web/packages/penalized/vignettes/penalized.pdf

# write final data set to processed folder
write.csv2(data, 
           here("data/processed/", "training_set_cleaned.csv"), 
           row.names = FALSE)
