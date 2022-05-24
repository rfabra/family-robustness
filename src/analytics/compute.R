library(ggplot2)
library(gridExtra)

# compute_class_distribution_per_bin <- function(predictions_per_bin) {
#   
#   num_models <- length(unique(predictions_per_bin$Model))
#   num_classes <- length(unique(predictions_per_bin$Real))
#   num_noise <- length(unique(predictions_per_bin$Noisy.Instances))
#   num_bins <- length(unique(predictions_per_bin$Num.Bin))
#   
#   num_rows <- num_models * num_classes * num_noise * num_bins
#   
#   result_df <- data.frame(
#     Noisy.Instances = rep(NA, num_rows),
#     Model = rep(NA, num_rows),
#     Bin = rep(NA, num_rows),
#     Num.Bin = rep(NA, num_rows),
#     Class = rep(NA, num_rows),
#     Real.Count = rep(NA, num_rows),
#     Predicted.Count = rep(NA, num_rows)
#   )
#   
#   result <- list(
#     # TODO: funcio que compose el nom donat model, soroll, etc
#   )
#   result[[noise]][[model]][[bin]][[class]] <- result[[noise]][[model]][[bin]][[class]] + 1
#   for (ni in predictions_per_bin)
#   
#   
# }

kappa_chance_prob <- function(data, reference) {
  classes <- levels(reference)
  num_instances <- length(reference)
  p_e <- 0  
  for (k in classes) {
    mul_1 <- 0
    # mul
    for (j in classes) {
      for (i in seq(num_instances)) {
        mul_1 <- mul_1 +
          as.integer(reference[i] == k) *
          as.integer(data[i] == j)
      }
    }
    mul_2 <- 0
    for (j in classes) {
      for (i in seq(num_instances)) {
        mul_2 <- mul_2 + 
          as.integer(reference[i] == j) *
          as.integer(data[i] == k)
      }
    }
    p_e <- p_e + mul_1 * mul_2
  }
  
  p_e <- p_e / (num_instances^2)
  
  return(p_e)
}

my_kappa <- function(data, reference, p_e = NULL) {
  # data <- factor(c(1,1,1), levels=c(1,2))
  # reference <- factor(c(1,1,1), levels=c(1,2))
  # p_e <- NULL
  if (is.null(p_e)) {
    p_e <- kappa_chance_prob(data, reference)    
  }
  
  p_a <- mean(as.integer(data == reference))
  # p_a
  # caret::confusionMatrix(data, reference)$overall[["Accuracy"]]
  kappa <- (p_a - p_e)/(1 - p_e)
  # kappa
  
  # caret::confusionMatrix(data, reference)$overall[["Kappa"]]
  
  return(kappa)
  
}

analytics_column_names <- function() {
  return(c("Dataset",
    "Item",
    "Split",
    "Real.Class",
    "Predicted.Class",
    "Predicted.Class.Test",
    "Noise.Instances.Prop",
    "Noise.Level",
    "Model",
    "Difficulty",
    "Difficulty.Bin"))
}

init_analytics <- function() {
  column_names <- analytics_column_names()
  analytics <- data.frame(matrix(nrow=0, ncol=length(column_names)))
  colnames(analytics) <- c(analytics_column_names())
  
  return(analytics)
}

structure_analytics <- function(dataset,
                                items,
                                split,
                                real_class,
                                predicted_class,
                                predicted_class_test,
                                noise_instances_prop,
                                noise_level,
                                model,
                                difficulty,
                                difficulty_bin) {
  info <- list(
    dataset,
    items,
    split,
    real_class,
    predicted_class,
    predicted_class_test,
    noise_instances_prop,
    noise_level,
    model,
    difficulty,
    difficulty_bin
  )
  names(info) <- analytics_column_names()
  
  return(data.frame(info))
}

compute_analytics <- function(analytics, ground_truth, metric, mylogger=NULL) {
  
  log_info_start_process("compute_analytics", 
                         list(analytics=sprintf("Data frame of size %d, %d", 
                                                nrow(analytics), ncol(analytics)),
                              ground_truth = ground_truth,
                              metric = metric)
                         , mylogger)
  
  result <- NULL
  
  if (ground_truth == "test") {
    log_info_message("Ground truth: test", mylogger)
    if (metric == "error") {
      log_info_message("Metric: error", mylogger)
      result <- analytics %>% dplyr::mutate(Hit=as.numeric(Predicted.Class != Predicted.Class.Test)) %>%
        dplyr::group_by(Split, Model, Difficulty.Bin) %>%
        dplyr::summarise(Difficulty=mean(Difficulty),
                  Error = mean(Hit))
    }
    else if (metric == "accuracy") {
      log_info_message("Metric: accuracy", mylogger)
      result <- analytics %>% dplyr::mutate(Hit=as.numeric(Predicted.Class == Predicted.Class.Test)) %>%
        dplyr::group_by(Split, Model, Difficulty.Bin) %>%
        dplyr::summarise(Difficulty=mean(Difficulty),
                  Accuracy = mean(Hit))
    }
    else if (metric == "kappa") {
      log_info_message("Metric: kappa", mylogger)
      result <- analytics %>% dplyr::mutate(Hit=as.numeric(Predicted.Class == Predicted.Class.Test)) %>%
        dplyr::group_by(Split, Model, Difficulty.Bin) %>%
        dplyr::summarise(Difficulty=mean(Difficulty),
                  Kappa = confusionMatrix(data=Predicted.Class, 
                                             reference = Predicted.Class.Test)$overall[["Kappa"]])
    }
    else {
      msg_error <- sprintf("Invalid metric <%s>. Options are: <accuracy>, <error> or <kappa>", 
                           metric)
      log_error_message(msg_error, mylogger)
      stop(msg_error)
    }
  }
  else if (ground_truth == "real") {
    log_info_message("Ground truth: real", mylogger)
    if (metric == "error") {
      log_info_message("Metric: error", mylogger)
      result <- analytics %>% mutate(Hit=as.numeric(Predicted.Class != Real.Class)) %>%
        group_by(Split, Model, Difficulty.Bin) %>%
        summarise(Difficulty=mean(Difficulty),
                  Error = mean(Hit))
    }
    else if (metric == "accuracy") {
      log_info_message("Metric: accuracy", mylogger)
      result <- analytics %>% mutate(Hit=as.numeric(Predicted.Class == Real.Class)) %>%
        group_by(Split, Model, Difficulty.Bin) %>%
        summarise(Difficulty=mean(Difficulty),
                  Accuracy = mean(Hit))
    }
    else if (metric == "kappa") {
      log_info_message("Metric: kappa", mylogger)
      result <- analytics %>% mutate(Hit=as.numeric(Predicted.Class == Real.Class)) %>%
        group_by(Split, Model, Difficulty.Bin) %>%
        summarise(Difficulty=mean(Difficulty),
                  Kappa = confusionMatrix(data=Predicted.Class, 
                                          reference = Real.Class)$overall[["Kappa"]])
    }
    else {
      msg_error <- sprintf("Invalid metric <%s>. Options are: <accuracy>, <error> or <kappa>", 
                           metric)
      log_error_message(msg_error, mylogger)
      stop(msg_error)
    }
  } 
  else {
    msg_error <- sprintf("Invalid ground truth <%s>. Options are: <real> or <test>", 
                         ground_truth)
    log_error_message(error_msg, mylogger)
    stop(msg_error)
  }
  log_info_message("END compute_analytics", mylogger)
  return(result)
}
