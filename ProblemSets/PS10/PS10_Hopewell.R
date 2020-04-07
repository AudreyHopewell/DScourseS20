library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)

# creating the task, respampling, and tuning strategies
classtask <- makeClassifTask(id = "taskname", data = income.train, target = "high.earner")
resample <- makeResampleDesc(method = "CV", iters = 3)
tune <- makeTuneControlRandom(maxit = 10)

# setting up algorithms
trees <- makeLearner("classif.rpart", predict.type = "response")
logregression <- makeLearner("classif.glmnet",predict.type = "response")
neuralnet <- makeLearner("classif.nnet",predict.type = "response")
naivebayes <- makeLearner("classif.naiveBayes",predict.type = "response")
kNN <- makeLearner("classif.kknn",predict.type = "response")
SVM <- makeLearner("classif.svm",predict.type = "response")

# creating hyperparameters 
tree_param <- makeParamSet(
  makeIntegerParam("minsplit", lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = .001, upper =.2)
)

logit_param <- makeParamSet(
  makeNumericParam("lambda", lower = 0, upper = 3),
  makeNumericParam("alpha", lower = 0, upper = 1)
)

neural_param <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 10),
  makeNumericParam("decay", lower = .1, upper = .5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000)
)

kNN_param <- makeParamSet(
  makeIntegerParam("k", lower = 1,upper = 30)
)

SVM_param <- makeParamSet(
  makeDiscreteParam("kernel", values = "radial"),
  makeDiscreteParam("cost", values = c(2^-2, 2^-1, 2^0, 2^1, 2^2, 2^10)),
  makeDiscreteParam("gamma", values = c(2^-2, 2^-1, 2^0, 2^1, 2^2, 2^10))
)

# tuning the models

tuned_trees <- tuneParams(learner = trees,
                          task = classtask,
                          resampling = resample,
                          measures = list(f1, gmean),
                          par.set = tree_param,
                          control = tune,
                          show.info = TRUE
                          )
#[Tune] Result: minsplit=49; minbucket=17; cp=0.0111 : f1.test.mean=0.8950632,gmean.test.mean=0.6599017

tuned_logit <- tuneParams(learner = logregression,
                          task = classtask,
                          resampling = resample,
                          measures = list(f1, gmean),
                          par.set = logit_param,
                          control = tune,
                          show.info = TRUE
                          )
#[Tune] Result: Result: lambda=0.237; alpha=0.0901 : f1.test.mean=0.8848959,gmean.test.mean=0.4789392

tuned_neural <- tuneParams(learner = neuralnet,
                           task = classtask,
                           resampling = resample,
                           measures = list(f1, gmean),
                           par.set = neural_param,
                           control = tune,
                           show.info = TRUE
                           )
# [Tune] Result: size=9; decay=0.164; maxit=1000 : f1.test.mean=0.9053202,gmean.test.mean=0.7487213

tuned_kNN <- tuneParams(learner = kNN,
                        task = classtask,
                        resampling = resample,
                        measures = list(f1, gmean),
                        par.set = kNN_param,
                        control = tune,
                        show.info = TRUE
                        )      
# [Tune] Result: k=29 : f1.test.mean=0.8983492,gmean.test.mean=0.7501544


tuned_SVM <- tuneParams(learner = SVM,
                        task = classtask,
                        resampling = resample,
                        measures = list(f1, gmean),
                        par.set = SVM_param,
                        control = tune,
                        show.info = TRUE
                        )
# Result: kernel=radial; cost=0.5; gamma=0.5 : f1.test.mean=0.9044762,gmean.test.mean=0.7228864

# applying optimal parameters to the models

trees <- setHyperPars(learner = trees, par.vals = tuned_trees$x)
logregression <- setHyperPars(learner = logregression, par.vals = tuned_logit$x)
neuralnet <- setHyperPars(learner = neuralnet, par.vals = tuned_neural$x)
kNN <- setHyperPars(learner = kNN, par.vals = tuned_kNN$x)
SVM <- setHyperPars(learner = SVM, par.vals = tuned_SVM$x)

# training the models

final_trees <- train(learner = trees, task = classtask)
final_logregression <- train(learner = logregression, task = classtask)
final_neuralnet <- train(learner = neuralnet, task = classtask)
final_naivebayes <- train(learner = naivebayes, task = classtask)
final_kNN <- train(learner = kNN, task = classtask)
final_SVM <- train(learner = SVM, task = classtask)
  
# generating predictions

predict_trees <- predict(final_trees, newdata = income.test)
predict_logregression <- predict(final_logregression, newdata = income.test)
predict_neuralnet <- predict(final_neuralnet, newdata = income.test)
predict_naivebayes <- predict(final_naivebayes, newdata = income.test)
predict_kNN <- predict(final_kNN, newdata = income.test)
predict_SVM <- predict(final_SVM, newdata = income.test)
  
# evaluating performance

print(performance(predict_trees, measures = list(f1, gmean)))
print(performance(predict_logregression, measures = list(f1, gmean)))
print(performance(predict_neuralnet, measures = list(f1, gmean)))
print(performance(predict_naivebayes, measures = list(f1, gmean)))
print(performance(predict_kNN, measures = list(f1, gmean)))
print(performance(predict_SVM, measures = list(f1, gmean)))

  
  
