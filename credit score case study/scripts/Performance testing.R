##################################
## ADC Case Study - Credit Scoring
## Performance Testing
## 16-12-2021
## Carmen Wolvius
## carmen.wolvius@gmail.com
##################################

#### Load Libraries
library(here)
library(tidyverse)

## Load cleaned train data
train_data_cleaned <- read.csv2(here("data/processed/","training_set_cleaned.csv"))

train <- train_data_cleaned[1:800,]
test <- train_data_cleaned[801:889,]

## Fit model
model <- glm(Survived ~.,
             family=binomial(link='logit'),
             data=train)

summary(model)

# ANOVA: The difference between the null deviance and the residual deviance shows how our model is doing against the null model 
anova(model, test="Chisq")

## Test model
fitted.results <- predict(model, 
                          newdata=test %>% 
                            select(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked), 
                          type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)

results <- data.frame(predicted = fitted.results, 
                      actual = test$Survived)
results <- results %>%
  mutate(score = ifelse(predicted == actual, 1, 0))

