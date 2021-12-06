logistic.model.metrics <- function(formula, data.train, data.test, treshold = 0.5){
  # Starting time count
  start <- Sys.time()
  
  # Fitting Logistic Model
  logistic.fit <- glm(formula = formula, data = data.train, family = binomial)
  
  # Predicting expected classes
  if (is.numeric(treshold) != FALSE || treshold >= 0 || treshold <= 1 ){
    logistic.pred.class <- as.factor(ifelse(predict(logistic.fit, data.test, type = "response") > treshold, 1, 0))
  } else{
    stop("Insert a valid treshold value. It must be numeric and between 0 and 1.")
  } 
  
  # Predicting expected probabilities
  logistic.pred.probability <- predict(logistic.fit, data.test, type = "response")
  
  # Creating the confusion matrices
  logistic.confusion.matrix <- caret::confusionMatrix(logistic.pred.class, data.test$Y)
  
  # Calculanting performance metrics
  logistic.performance.metrics <- summary.measures(logistic.confusion.matrix$table, print = FALSE)
  
  # Calculating Chi-Square Statistics
  logistic.chi.square.statistic <- sum(((logistic.pred.probability - data.test$p)^2)/(logistic.pred.probability))
  
  # Calculating KS Statistic
  logistic.ks.statistic <- ks.test(logistic.pred.probability, data.test$p)$statistic
  
  # Calculating run time
  logistic.run.time <- Sys.time() - start
  
  logistic.output <- data.frame(logistic.performance.metrics, logistic.chi.square.statistic,
                                logistic.ks.statistic, logistic.run.time, mean(logistic.pred.probability))
  
  names(logistic.output) <- c("logistic_ACC","logistic_TPR","logistic_TNR","logistic_CSI", "logistic_SSI",
                              "logistic_FAITH","logistic_PDIF","logistic_GS","logistic_P", "logistic_R",
                              "logistic_F1","logistic_MCC","logistic_ChiSquare","logistic_KS","logistic_RunTime",
                              "logistic_ProbMean")
  return(logistic.output)
}











randomforest.model.metrics <- function(formula, data.train, data.test, treshold = 0.5){
  # Starting time count
  start <- Sys.time()
  
  # Fitting Random Forest Model
  randomforest.fit <- randomForest(formula = formula, data = data.train)
  
  # Predicting expected classes
  if (is.numeric(treshold) != FALSE || treshold >= 0 || treshold <= 1 ){
    randomforest.pred.class <- as.factor(ifelse(predict(randomforest.fit, data.test, "prob")[,2] > treshold, 1, 0))
  } else{
    stop("Insert a valid treshold value. It must be numeric and between 0 and 1.")
  } 
  
  # Predicting expected probabilities
  randomforest.pred.probability <- predict(randomforest.fit, data.test, type = "prob")[,2]
  
  # Creating the confusion matrices
  randomforest.confusion.matrix <- caret::confusionMatrix(randomforest.pred.class, data.test$Y)
  
  # Calculanting performance metrics
  randomforest.performance.metrics <- summary.measures(randomforest.confusion.matrix$table, print = FALSE)
  
  # Calculating Chi-Square Statistics
  randomforest.chi.square.statistic <- sum(((randomforest.pred.probability - data.test$p)^2)/(randomforest.pred.probability+0.00001))
  
  # Calculating KS Statistic
  randomforest.ks.statistic <- ks.test(randomforest.pred.probability, data.test$p)$statistic
  
  # Calculating run time
  randomforest.run.time <- Sys.time() - start
  
  randomforest.output <- data.frame(randomforest.performance.metrics, randomforest.chi.square.statistic,
                                randomforest.ks.statistic, randomforest.run.time, mean(randomforest.pred.probability))
  
  names(randomforest.output) <- c("randomforest_ACC","randomforest_TPR","randomforest_TNR","randomforest_CSI", "randomforest_SSI",
                              "randomforest_FAITH","randomforest_PDIF","randomforest_GS","randomforest_P", "randomforest_R",
                              "randomforest_F1","randomforest_MCC","randomforest_ChiSquare","randomforest_KS","randomforest_RunTime",
                              "randomforest_ProbMean")
  return(randomforest.output)
}


