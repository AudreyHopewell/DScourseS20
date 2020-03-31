library(mlr)
library(glmnet)
library(caret)
library(tidyverse)

housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula <- as.formula(lmedv ~ .^3 + poly(crim, 6) + 
                        poly(zn, 6) + poly(indus , 6) + 
                        poly(nox, 6) + poly(rm, 6) + 
                        poly(age, 6) + poly(dis, 6) + 
                        poly(rad, 6) + poly(tax, 6) + 
                        poly(ptratio , 6) + poly(b, 6) + 
                        poly(lstat , 6))
mod_matrix <- data.frame(model.matrix(formula, housing))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix[, 1] = housing$lmedv
colnames(mod_matrix)[1] = "lmedv" #make sure to rename it otherwise MLR
# won ’t find it
head(mod_matrix) #just make sure everything is hunky-dory
# Break up the data:
n <- nrow(mod_matrix)
train <- sample(n, size = .8*n) 
test <- setdiff(1:n, train) 
housing.train <- mod_matrix[train ,] 
housing.test <- mod_matrix[test, ]


# using LASSO to predict log median house value

task <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(task)

predAlg <- makeLearner("regr.glmnet")

resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=1,upper=1))

tuneMethod <- makeTuneControlRandom(maxit = 50L)

tunedModel <- tuneParams(learner = predAlg,
                         task = task,
                         resampling = resampleStrat,
                         measures = rmse,
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# optimal lambda is 0.0291, in-sample RMSE is 0.1970

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)
resample(predAlg,task,resampleStrat,measures=list(rmse))
finalModel <- train(learner = predAlg, task = task)
prediction <- predict(finalModel, newdata = housing.test)
print(head(prediction$data))
print(measureRMSE(prediction$data$truth,prediction$data$response))

# out-of-sample RMSE is 0.1679

# ridge regression model

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=0,upper=0))
tunedModel <- tuneParams(learner = predAlg,
                         task = task,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)
resample(predAlg,task,resampleStrat,measures=list(rmse))
finalModel <- train(learner = predAlg, task = task)
prediction <- predict(finalModel, newdata = housing.test)
print(measureRMSE(prediction$data$truth,prediction$data$response))
getLearnerModel(finalModel)$beta

# elastic net

x <- model.matrix(lmedv~., housing.train)[,-1]
y <- housing.train$lmedv

cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min

model <- train(lmedv~., data = housing.train, method = "glmnet",
  trControl = trainControl("cv", number = 6),
  tuneLength = 6)

model$bestTune
coef(model$finalModel, model$bestTune$lambda)
x.test <- model.matrix(lmedv ~., housing.test)[,-1]
predictions <- model %>% predict(x.test)

RMSE(predictions, housing.test$lmedv)








