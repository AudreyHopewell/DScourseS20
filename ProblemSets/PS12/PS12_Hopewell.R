library(stargazer)
library(tidyverse)
library(sampleSelection)
library(mlogit)

setwd("~/DScourseS20/Structural")
wages12 <- read.csv("wages12.csv")


wages12$college <- as.factor(wages12$college)
wages12$married <- as.factor(wages12$married)
wages12$union <- as.factor(wages12$union)

stargazer(wages12)
sum(is.na(wages12$logwage))/length(wages12$logwage)

# listwise deletion of missing wages (MCAR assumption)
wages12.complete <- na.omit(wages12)
model1 <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = wages12.complete)

# mean imputation
wages12.mean <- wages12
wages12.mean$logwage[is.na(wages12.mean$logwage)] <- mean(wages12.mean$logwage, na.rm = TRUE)
model2 <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = wages12.mean)

# accounting for non-random missing values
wages12 <- wages12 %>% mutate(
  valid = if_else(
    is.na(logwage)==TRUE, 0, 1
  )
)

wages12$logwage[is.na(wages12$logwage)] <- 0

model3 <- selection(selection = valid ~ hgc + union + college + 
             exper + married + kids, outcome = logwage ~ 
             hgc + union + college + exper + I(exper^2), 
           data = wages12, method = "2step")


stargazer(model1, model2, model3, title = "Results")

# estimating a probit model of union preferences

unionprobit <- glm(union ~ hgc + college + exper + married + kids, 
                   family = binomial(link = 'probit'), data = wages12)
print(summary(unionprobit))
wages12$predprobit <- predict(unionprobit, newdata = wages12, type = "response")
print(summary(wages12$predprobit))

# counterfactual model

unionprobit$coefficients["married"] <- 0
unionprobit$coefficients["kids"] <- 0

wages12$predcounterfactual <- predict(unionprobit, newdata = wages12, type = "response")
summary(wages12$predcounterfactual)

