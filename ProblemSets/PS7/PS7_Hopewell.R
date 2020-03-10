install.packages("mice")
install.packages("stargazer")

library(readr)
wages <- read_csv("ModelingOptimization/wages.csv")
class(wages)

library(data.table)
wages <- data.table(wages)
wages.filtered <- na.omit(wages, cols = c("hgc", "tenure"))

library(stargazer)
stargazer(wages)

sum(is.na(wages$logwage))/length(wages$logwage)

wages.na.omit <- na.omit(wages.filtered)

model1 <- lm(logwage ~ hgc + college 
             + tenure +(tenure)^2 + age
             + married, data = wages.na.omit)
model1
library(stargazer)
stargazer(model1)

wages.filtered$logwage[is.na(wages.filtered$logwage)] <- 
  mean(wages.filtered$logwage, na.rm = TRUE)

model2 <- lm(logwage ~ hgc + college 
             + tenure +(tenure)^2 + age
             + married, data = wages.filtered)
model2
stargazer(model2)

prediction <- predict(model1, wages.filtered)
impute <- function(a, a.impute){
  ifelse (is.na(a), a.impute, a)
}
wages.filtered$logwage <- impute(wages.filtered$logwage, prediction)

model3 <- lm(logwage ~ hgc + college 
             + tenure +(tenure)^2 + age
             + married, data = wages.filtered)

library(mice)
wages.imputed <- mice(wages.filtered)
summary(wages.imputed)
fit = with(wages.imputed, lm(logwage ~ hgc + college 
                             + tenure +(tenure)^2 + age
                             + married))

stargazer(model1, model2, model3, title = "Results", align = TRUE)
