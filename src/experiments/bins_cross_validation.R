cross_val_dataset_bins <- function(
  dataset_data,
  paths,
  irt,
  noise_data,
  models,
  mylogger = NULL) {

  log_info_message("START cross_val_dataset_bins ")
  # Load and prepare dataset ------------------------------------------------
  dataset_name <- names(dataset_data)[[1]]
  task_id <- dataset_data[[dataset_name]]
  dataset_path <- build_path(paths$datasets, c(dataset_name), "rds")
  dataset <- load_dataset(dataset_name = dataset_name,
                          task_id = task_id,
                          dataset_path = dataset_path)
  # library(xtable)
  # browser()
  # 
  # dataset_data <- data.frame(
  #   Dataset = dataset_name,
  #   Instances = nrow(dataset),
  #   Features = ncol(dataset) - 1,
  #   Classes = length(unique(dataset$Class))
  # )
  # 
  # xtable(dataset_data)
  # 
  # return(T)
  dataset <- cbind(data.frame(row_id = rownames(dataset)), dataset)
  
  
  
  # Compute difficulty data -------------------------------------------------
  path_difficulty <- file_path_difficulty(paths$irt, dataset_name, 
                                          irt$num_models)
  if (file.exists(path_difficulty)) {
    difficulty_data <- load_object(path_difficulty)
    difficulty <- difficulty_data$difficulty
    difficulty_bins <- difficulty_data$bins
  }
  else {
    set.seed(123)
    path_runs <- build_path(paths$datasets, c("run", dataset_name), "rds")
    
    task_runs <- get_runs_from_task(task_id, path_runs, mylogger)
    path_response_matrix <- file_path_response_matrix(
      paths$irt, 
      dataset_name,
      irt$num_models)
    
    response_matrix <- get_response_matrix(dataset,
                                           task_runs,
                                           irt$num_models,
                                           path_response_matrix = path_response_matrix,
                                           mylogger = mylogger)
    
    # response_matrix <- remove_constant_columns(response_matrix)
    
    # Estimate IRT ------------------------------------------------------------
    irt_model_path <- build_path(paths$irt,
                                 c(dataset_name,
                                   irt$n_cycles,
                                   irt$chunk_size),
                                 "rds")
    
    irt_result <- incremental_IRT(response_matrix = response_matrix,
                                  chunk_size = irt$chunk_size,
                                  remove_abtruse_instances = TRUE,
                                  n_cycles = irt$n_cycles,
                                  path_irt = irt_model_path,
                                  mylogger = mylogger)
    # Extract difficulty and bins ------------------------------------------------------
    
    path_difficulty <- file_path_difficulty(paths$irt, 
                                            dataset_name, 
                                            irt$num_models)
    
    difficulty_data <- extract_difficulty_data(irt_data = irt_result,
                                               irt_num_bins = irt$num_bins,
                                               path_difficulty = path_difficulty,
                                               mylogger = mylogger)
    difficulty <- difficulty_data$difficulty
    difficulty_bins <- difficulty_data$bins
  }
  dataset <- merge(data.frame(row_id = difficulty$row_id), dataset, by="row_id")
  
  
  set.seed(123)
  folds <- create_folds(dataset$Class, k=5)
  i_fold <- 0
  
  path_predictions_per_bin <- file_path_predictions_per_bin(
    base_path = paths$predictions_per_bin,
    dataset_name = dataset_name,
    noise_magnitude = noise_data$magnitude)
  if (file.exists(path_predictions_per_bin)) {
    predictions_per_bin <- load_object(path_predictions_per_bin, "predictions per bin", mylogger)
  }
  else {
    num_rows <- nrow(dataset) * length(models) * length(noise_data$instance_proportions)
    predictions_per_bin <- data.frame(
      Fold = NA,
      row_id = NA,
      Model = NA,
      Real = NA,
      Predicted = NA,
      Predicted.Noisy = NA,
      Noisy.Instances = NA,
      Difficulty = NA,
      Bin = NA,
      Num.Bin = NA
    )
    predictions_per_bin <- predictions_per_bin[0,]
    
    # predictions_per_bin <- data.frame(
    #   Fold = rep(NA, num_rows),
    #   row_id = rep(NA, num_rows),
    #   Model = rep(NA, num_rows),
    #   Real = rep(NA, num_rows),
    #   Predicted = rep(NA, num_rows),
    #   Predicted.Noisy = rep(NA, num_rows),
    #   Noisy.Instances = rep(NA, num_rows),
    #   Difficulty = rep(NA, num_rows),
    #   Bin = rep(NA, num_rows),
    #   Num.Bin = rep(NA, num_rows)
    # )
    
    i_iter <- 1
    for(raw_train_fold in folds) {
      i_fold <- i_fold + 1
      log_info_message(sprintf("Fold %d", i_fold), mylogger)
      train_fold <- rownames(dataset[raw_train_fold,])
      test_fold <- rownames(dataset[-raw_train_fold,])
      data_train <- dataset[train_fold,]
      data_test <- dataset[test_fold,]
      
      train_row_id <- data_train$row_id
      test_row_id <- data_test$row_id
      data_train <- dplyr::select(data_train, -row_id)
      data_test <- dplyr::select(data_test, -row_id)
      
      # Generate noisy dataset --------------------------------------------------
      noisy_dataset_path <- file_path_noisy_dataset(
        base_path = paths$noisy_datasets,
        dataset_name = dataset_name,
        noise_instance_proportion = 1.0,
        noise_magnitude = noise_data$magnitude,
        fold = i_fold
      )
      
      feature_stats <- compute_feature_stats(data_train, mylogger)
      
      data_test_noisy <- generate_noisy_dataset(
        data = data_test,
        feature_stats = feature_stats,
        noise_level = noise_data$magnitude,
        instance_proportion = 1.0,
        load_path = noisy_dataset_path,
        save_path = noisy_dataset_path,
        myseed = 123,
        mylogger = mylogger)
      
      for (model_name in models) {
        log_info_message(sprintf("Model %s", model_name), mylogger)
        model_path <- file_path_model(paths$models,
                                           model_name = model_name,
                                           dataset_name = dataset_name,
                                           fold = i_fold)
        
        model <- learn_model(model_name = model_name,
                             train = data_train,
                             load_path = model_path,
                             mylogger = mylogger)
        
        preds_path <- file_path_predictions(
          paths$predictions,
          dataset_name = dataset_name,
          model_name = model_name,
          split = "Original",
          fold = i_fold)
        
        predictions_original <- predict_test(
          model = model,
          model_name = model_name,
          test = data_test,
          train = data_train,
          load_path = preds_path,
          mylogger = mylogger)
        
        
        preds_path <- file_path_predictions(
          paths$predictions,
          dataset_name = dataset_name,
          model_name = model_name,
          split = sprintf("Test-%0.2f-%0.2f", 1.0, noise_data$magnitude),
          fold = i_fold)
        
        predictions_noisy <- predict_test(
          model = model,
          model_name = model_name,
          test = data_test_noisy,
          train = data_train,
          load_path = preds_path,
          mylogger = mylogger)
        
        predictions <- data.frame(
          Fold = i_fold,
          row_id = test_row_id,
          Model = model_name,
          Real = data_test$Class,
          Predicted = predictions_original,
          Predicted.Noisy = predictions_noisy
        )
        predictions_metadata <- merge(predictions, difficulty, by="row_id")
        
        # Iterate noisy instances proportion --------------------------------------
        for (noise_instance in noise_data$instance_proportions) {
          log_info_message(sprintf("Noisy instances: %0.2f", noise_instance), mylogger)
          predictions_metadata <- dplyr::mutate(predictions_metadata, Noisy.Instances = noise_instance)
          for (d_bin in difficulty_bins) {
            i_bin <- which(d_bin == difficulty_bins)
            log_info_message(paste("Processing bin", i_bin), mylogger)
            # Get the data from the bin
            data_test_bin <- dplyr::filter(predictions_metadata, Bin == d_bin) %>%
              mutate(Num.Bin = i_bin)
            
            # Perturb the noise_instance% of instances
            num_instances_bin <- nrow(data_test_bin)
            set.seed(123)
            perturbed_idx <- sample(1:num_instances_bin, noise_instance*num_instances_bin)
            if (length(perturbed_idx) == 0) {
              data_test_bin$Predicted.Noisy <- data_test_bin$Predicted
            }
            else {
              data_test_bin[-perturbed_idx,]$Predicted.Noisy <- 
                data_test_bin[-perturbed_idx,]$Predicted
            }
            data_test_bin$Noisy.Instances <- factor(data_test_bin$Noisy.Instances,
                                                    levels = noise_data$instance_proportions)
            # TODO: initialize size at the beginning, avoid rbind(), its too slow
            # browser()
            # predictions_per_bin[i_iter:(i_iter + nrow(data_test_bin) - 1), ] <- data_test_bin
            # i_iter <- i_iter + nrow(data_test_bin)
            
            predictions_per_bin <- rbind(predictions_per_bin, data_test_bin)
          }
        }
      }
    }
    predictions_per_bin <- predictions_per_bin[complete.cases(predictions_per_bin),]
    saveRDS(predictions_per_bin, path_predictions_per_bin)
  }
  
  predictions_per_bin <- load_object(path_predictions_per_bin, 
                                     "Predictions per bin", 
                                     mylogger = mylogger)
  
  eval_metric <- list(
    "Kappa" = function(data, reference) {
      return(confusionMatrix(data=data, 
                             reference = reference)$overall[["Kappa"]])
    },
    "Accuracy" = function(data, reference) {
      return(confusionMatrix(data=data, 
                             reference = reference)$overall[["Accuracy"]])
    },
    "F1" = function(data, reference) {
      conf_mat <- confusionMatrix(data=data, 
                                  reference=reference, mode="everything")$byClass
      if (length(levels(reference)) > 2) {
        return(mean(conf_mat[,"F1"]))
      }
      else {
        return(conf_mat[["F1"]])
      }
    },
    "Global.Kappa" = function(data, reference, chance_prob) {
      return(my_kappa(data, reference, chance_prob))
    }
  )
  
  get_class_distribution <- list(
    "Real" = function(predictions_per_bin) {
      real_class_count <- predictions_per_bin %>% 
        dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin, Real) %>%
        dplyr::summarise(
          Real.Count = length(Real)
        )
    },
    "Predicted" = function(predictions_per_bin) {
      real_class_count <- predictions_per_bin %>% 
        dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin, Predicted) %>%
        dplyr::summarise(
          Predicted.Count = length(Predicted)
        )
    },
    "Predicted.Noisy" = function(predictions_per_bin) {
      real_class_count <- predictions_per_bin %>% 
        dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin, Predicted.Noisy) %>%
        dplyr::summarise(
          Predicted.Noisy.Count = length(Predicted.Noisy)
        )
    }
  )
  # Plot class distribution
  class_distribution_real <- get_class_distribution[["Real"]](predictions_per_bin)
  class_distribution_predicted_noisy <- get_class_distribution[["Predicted.Noisy"]](predictions_per_bin)

  plot_path <- build_path(paths$analytics$predictions_per_bin_predicted,
                          c(dataset_name, "Predicted", noise_data$magnitude),
                          "pdf")
  class_plots <- plot_predicted_class_distribution(
    class_distribution_predicted_noisy,
    plot_path,
    mylogger = mylogger)
  
  plot_path <- build_path(paths$analytics$predictions_per_bin_real,
                          c(dataset_name, "Real", noise_data$magnitude),
                          "pdf")
  class_plots <- plot_real_class_distribution(
    class_distribution_real,
    plot_path,
    mylogger = mylogger)
  
  # Plot global dataset evaluation
  path_evaluations <- build_path(
    paths$analytics$evaluation, 
    dataset_name,
    "rds")
  if (file.exists(path_evaluations)) {
    dataset_evaluation <- load_object(path_evaluations)
  }
  else {
    dataset_evaluation <- predictions_per_bin %>% # dplyr::mutate(Hit=as.numeric(Predicted.Class != Predicted.Class.Test)) %>%
      dplyr::group_by(Noisy.Instances, Model) %>%
      dplyr::summarise(
        Dataset.Accuracy.Predicted = eval_metric$Accuracy(Predicted, Predicted.Noisy),
        Dataset.Kappa.Predicted = eval_metric$Kappa(Predicted, Predicted.Noisy),
        Dataset.F1.Predicted = eval_metric$F1(Predicted, Predicted.Noisy),
        Dataset.Accuracy.Real = eval_metric$Accuracy(Real, Predicted.Noisy),
        Dataset.Kappa.Real = eval_metric$Kappa(Real, Predicted.Noisy),
        Dataset.F1.Real = eval_metric$F1(Real, Predicted.Noisy)
        #Global.Chance = kappa_chance_prob(Predicted, Predicted.Noisy)
      )
    
    save_object(dataset_evaluation, path_evaluations)
  }
  # Plot robustness curves
  path_robustness_curves <- build_path(
    paths$robustness_curves, c(dataset_name), "rds"
  )
  
  if (file.exists(path_robustness_curves)) {
    robustness_curves <- load_object(path_load = path_robustness_curves,
                                        object_name = "robustness curves",
                                        mylogger = mylogger)
  }
  else {
    robustness_curves <- predictions_per_bin %>% 
      dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin) %>%
      dplyr::summarise(
        Difficulty=mean(Difficulty),
        Accuracy.Predicted = eval_metric$Accuracy(Predicted, Predicted.Noisy),
        Kappa.Predicted = eval_metric$Kappa(Predicted, Predicted.Noisy),
        F1.Predicted = eval_metric$F1(Predicted, Predicted.Noisy),
        Accuracy.Real = eval_metric$Accuracy(Real, Predicted.Noisy),
        Kappa.Real = eval_metric$Kappa(Real, Predicted.Noisy),
        F1.Real = eval_metric$F1(Real, Predicted.Noisy)
        # Global.Kappa = eval_metric$Global.Kappa(Predicted, Predicted.Noisy, Global.Chance)
      )
    save_object(robustness_curves, path_robustness_curves)
  }
  # browser()
  plot_path <- build_path(paths$analytics$robustness_curves_predicted,
                          c(dataset_name, "Predicted", "Kappa", noise_data$magnitude),
                          "pdf")
  robustness_plots <- plot_robustness_curves(
    robustness_curves = robustness_curves,
    y_axis_name = "Kappa.Predicted",
    path_plot = plot_path,
    mylogger = mylogger)
  
  plot_path <- build_path(paths$analytics$robustness_curves_predicted,
                          c(dataset_name, "Predicted", "Accuracy", noise_data$magnitude),
                          "pdf")
  robustness_plots <- plot_robustness_curves(
    robustness_curves = robustness_curves,
    y_axis_name = "Accuracy.Predicted",
    path_plot = plot_path,
    mylogger = mylogger)
  
  plot_path <- build_path(paths$analytics$robustness_curves_real,
                          c(dataset_name, "Real", "Kappa", noise_data$magnitude),
                          "pdf")
  robustness_plots <- plot_robustness_curves(
    robustness_curves = robustness_curves,
    y_axis_name = "Kappa.Real",
    path_plot = plot_path,
    mylogger = mylogger)
  
  plot_path <- build_path(paths$analytics$robustness_curves_real,
                          c(dataset_name, "Real", "Accuracy", noise_data$magnitude),
                          "pdf")
  robustness_plots <- plot_robustness_curves(
    robustness_curves = robustness_curves,
    y_axis_name = "Accuracy.Real",
    path_plot = plot_path,
    mylogger = mylogger)
  
  return(TRUE)
}

multidataset_cross_val_bins <- function(datasets_data,
                                        paths,
                                        irt,
                                        noise_data,
                                        models,
                                        mylogger = NULL) {
  for (dataset_name in names(datasets_data)) {
    log_info_message(sprintf("Processing dataset %s", dataset_name), mylogger)
    dataset_data <- list(datasets_data[[dataset_name]])
    names(dataset_data) <- dataset_name
    tryCatch(
      cross_val_dataset_bins(dataset_data,
                             paths,
                             irt,
                             noise_data,
                             models,
                             mylogger),
      error = function(x) {
        print(sprintf("Error when processing dataset\n%s",
                      x))
      })
    log_info_message(sprintf("END dataset %s", dataset_name), mylogger)
  }
  return(TRUE)
}
