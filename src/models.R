library(caret)
library(RWeka)
# TODO: change save/load path by model/prediction_path
# get_family_prototypes <- function() {
#   # TODO: (5) move this to config file  
#   return(c(
#     "mlp_7",
#     "multinom",
#     "fda_prune17",
#     "simpls_ncomp3",
#     "rda",
#     "knn_k3",
#     "lvq_3",
#     "svmPoly_d_2_s_0.1",
#     "gbm_3_50",
#     "RPART",
#     "JRip",
#     "ctree_c0.05",
#     "C5.0",
#     "PART",
#     "rf_mtry64",
#     "rfRules_mtry64",
#     "NB",
#     "rbf"
#   ))
# }



learn_model <- function(model_name, 
                        train, 
                        load_path = NULL, 
                        save_path = load_path,
                        mylogger = NULL) {
  
  model <- load_object(load_path, 
                       sprintf("%s model", model_name), 
                       mylogger)
  if (!is.null(model)) {
    return(model)
  }
  
  log_info_message(sprintf("Learning model %s...", model_name), mylogger)
  
  if (model_name=="C5.0") {
    load_install("C50")
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    model <- C5.0(Class ~., data = train)
    
    tryCatch(detach("package:C50", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RF") {
    load_install("randomForest")
    model <- caret::train(Class ~ ., 
                          data = train, 
                          preProc = c("center", "scale"), 
                          method = "rf",
                          # tuneGrid=data.frame(mtry = 64),
                          trControl = trainControl(method="none"))    
    
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "1-NN") { 
    model <- caret::train(Class ~ ., 
                          data = train, 
                          preProc = c("center", "scale"), 
                          method = "knn",
                          tuneGrid=data.frame(k = 1),  
                          trControl = trainControl(method="none"))
  }
  else if (model_name == "knn_k3") { 
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "knn",tuneGrid=data.frame(k = 3),  
                          trControl = trainControl(method="none"))
  }
  else if (model_name=="JRip"){
    load_install("RWeka")
    # preProc <- preProcess(train, method=c("center", "scale"))
    # train <- predict(preProc, train)
    # model <-JRip(Class ~., data = train)
    model <- caret::train(Class ~ ., data = train, 
                          preProc = c("center", "scale"), 
                          method = "JRip", 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "SVM") {
    load_install("kernlab")
    model <- caret::train(Class ~ ., 
                          data = train, 
                          preProc = c("center", "scale"), 
                          method = "svmRadialCost", 
                          # tuneGrid=data.frame(C=2^5), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "NB") {
    load_install("naivebayes")
    
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    model <- naive_bayes(train[,-ncol(train)], train[,ncol(train)])
    
    tryCatch(detach("package:naivebayes", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "FDA_prune9") {
    load_install("mda")
    model <- caret::train(Class ~ ., data = train, 
                          preProc = c("center", "scale"), 
                          method = "fda", 
                          tuneGrid=data.frame(nprune = 9, degree = 1), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "NNET") {
    load_install("nnet")
    
    model <- caret::train(Class~ .,
                          data=train,
                          method="nnet")
                          # metric="Rsquared",
                          # trControl=ctrl,
                          # tuneGrid=t.grid)
    
    tryCatch(detach("package:nnet", unload=TRUE), error = function(e){print(e)})
    
  }
  else if (model_name == "ADA") {
    load_install("fastAdaboost")
    model <- caret::train(Class~ .,
                          data=train,
                          method="adaboost")
    
    tryCatch(detach("package:fastAdaboost", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RPART") {
    load_install("rpart")
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    
    model <- rpart(Class ~., data = train, method = "class")
    
    tryCatch(detach("package:rpart", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "simpls_ncomp3") { 
    load_install("pls")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "simpls",
                          tuneGrid=data.frame(ncomp = 3),  
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:pls", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "mlpWeightDecay_1e05") {
    load_install("RSNNS")
    model <- caret::train(
      Class ~ ., data = train, 
      preProc = c("center", "scale"), 
      method = "mlpWeightDecay", 
      tuneGrid=data.frame(size=5, decay = 1e-05))
    
    tryCatch(detach("package:RSNNS", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "mlp_7") {
    load_install("RSNNS")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), method = "mlp", tuneGrid=data.frame(size=7), trControl = trainControl(method="none"))
    
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "multinom") {
    load_install("nnet")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), method = "multinom", 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:nnet", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "fda_prune17") {
    load_install("mda")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "fda", 
                          tuneGrid=data.frame(nprune = 17, degree = 1), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rda") {
    load_install("klaR")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), method = "rda")
    
    tryCatch(detach("package:klaR", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "lvq_3") {
    load_install("class")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "lvq", tuneGrid=data.frame(size=50, k = 3), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:class", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "svmPoly_d_2_s_0.1") {
    load_install("kernlab")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "svmPoly", tuneGrid=data.frame(degree=2, scale= 0.1, C = 1), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "gbm_3_50") {
    load_install("gbm")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "gbm", 
                          tuneGrid=data.frame(n.trees = 50, interaction.depth = 3, 
                                              shrinkage = 0.1 , n.minobsinnode = 2), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:gbm", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "ctree_c0.05"){
    load_install("party")
    
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "ctree", 
                          tuneGrid=data.frame(mincriterion=0.5), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:party", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "PART"){
    load_install("RWeka")
    preProc <- preProcess(train, method=c("center", "scale"))
    # train <- predict(preProc, train)
    # model <- PART(Class ~., data = train)
    model <- caret::train(Class ~ ., data = train, 
                          preProc = c("center", "scale"), 
                          method = "PART", 
                          trControl = trainControl(method="none"))
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rf_mtry64") {
    load_install("randomForest")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "rf",tuneGrid=data.frame(mtry = 64),  
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rfRules_mtry64") {
    load_install("inTrees")
    load_install("randomForest")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "rfRules", tuneGrid=data.frame(mtry = 64, maxdepth=2),  
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:randomForest", unload = TRUE), error = function(e){print(e)})
    tryCatch(detach("package:inTrees", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rbf") {
    load_install("RSNNS")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "rbfDDA", tuneGrid=data.frame(negativeThreshold=0.001), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  
  
  if (is.null(model)) {
    log_error_message(sprintf("The model %s could not be trained. Possible reasons:\n 
                              \t(1) You mistyped the model_name parameter (%s)", 
                              model_name, model_name), 
                      mylogger)
  } 
  else {
    save_object(model, save_path, sprintf("%s model", model_name), mylogger)
  }
  
  return(model)
}

predict_test <- function(model,
                         model_name,
                         test, 
                         train,
                         pred_levels = levels(train$Class),
                         load_path = NULL, 
                         save_path = load_path,
                         mylogger = NULL) {
  
  predictions <- load_object(load_path, 
                       sprintf("%s predictions", model_name), 
                       mylogger)
  if (!is.null(predictions)) {
    return(predictions)
  }
  
  log_info_message(sprintf("Predicting with model %s...", model_name), mylogger)
  
  if (model_name=="C5.0") {
    load_install("C50")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:C50", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RF") {
    load_install("randomForest")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  } 
  else if (model_name == "1-NN") {
    predictions <- predict(model, newdata = test)
  }
  else if (model_name == "knn_k3") { 
    predictions <- predict(model, newdata = test)
  }
  else if (model_name == "JRip"){
    load_install("RWeka")
    # preProc <- preProcess(train, method=c("center", "scale"))
    # test <- predict(preProc, test)
    # 
    # # NOTE: This is if fails to load the trained model 
    model <- learn_model(model_name = model_name,
                         train = train,
                         mylogger = mylogger)
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "SVM") {
    load_install("kernlab")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "NB") {
    load_install("naivebayes")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:naivebayes", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "FDA_prune9") {
    load_install("mda")

    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  } 
  else if (model_name == "NNET") {
    load_install("nnet")
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:nnet", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "ADA") {
    load_install("fastAdaboost")
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:fastAdaboost", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RPART") {
    load_install("rpart")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    
    raw_predictions <- predict(model, newdata = test)
    predictions <-  factor(as.vector(apply(raw_predictions, 1, 
                                           function (x) {
                                             colnames(raw_predictions)[which.max(x)]
                                           })), 
                           levels=levels(as.factor(train$Class)))
    
    tryCatch(detach("package:rpart", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "simpls_ncomp3") { 
    load_install("pls")
    predictions <- predict(model, newdata = test); "OK"
    tryCatch(detach("package:pls", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "mlp_7") {
    load_install("RSNNS")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "multinom") {
    load_install("nnet")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:nnet", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "fda_prune17") {
    load_install("mda")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rda") {
    load_install("klaR")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:klaR", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "lvq_3") {
    load_install("class")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:class", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "svmPoly_d_2_s_0.1") {
    load_install("kernlab")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "gbm_3_50") {
    load_install("gbm")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:gbm", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "ctree_c0.05"){
    load_install("party")
    # NOTE: This is if fails to load the trained model 
    model <- learn_model(model_name = model_name,
                         train = train,
                         mylogger = mylogger)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:party", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "PART"){
    load_install("RWeka")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    model <- learn_model(model_name = model_name,
                         train = train,
                         mylogger = mylogger)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rf_mtry64") {
    load_install("randomForest")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rfRules_mtry64") {
    load_install("inTrees")
    load_install("randomForest")
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:randomForest", unload = TRUE), error = function(e){print(e)})
    tryCatch(detach("package:inTrees", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rbf") {
    load_install("RSNNS")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  
  
  
  
  
  
  
  if (is.null(predictions)) {
    log_error_message(sprintf("The model %s could not be used to predict. Possible reasons:\n 
                              \t(1) You mistyped the model_name parameter (%s)", 
                              model_name, model_name), 
                      mylogger)
  }
  else {
    predictions <- factor(predictions, levels = pred_levels)
    save_object(predictions, save_path, sprintf("%s predictions", model_name), mylogger)
  }
  
  
  
  return(predictions)
  
}
