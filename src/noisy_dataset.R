compute_feature_stats <- function(data, mylogger=NULL) {
  log_info_start_process("compute_feature_stats", 
                         list(data = sprintf("Data frame of size %d %d", 
                                             nrow(data), ncol(data))),
                         mylogger)
  
  numcols<-ncol(data)-1
  numrows <- nrow(data)
  
  feature_data <- list()
  feature_data$values <- list() 
  feature_data$sd <- list()
  feature_data$freqs <- list()
  feature_data$props <- list()
  for (f in (1:numcols)) {
    feature_name <- colnames(data)[f]
    if (is.factor(data[,f]) || is.integer(data[,f]))
    { # Nominal or ordinal
      feature_data$values[[feature_name]] <- as.factor(unique(data[,f]))
      data_table <- table(data[,f])
      feature_data$freqs[[feature_name]] <- as.list(data_table)
      feature_data$props[[feature_name]] <- as.list(prop.table(data_table))
    } 
    else 
    { # Numeric
      feature_data$sd[[feature_name]] <- sd(data[,f])
    }
  }
  log_info_message("End compute_feature_stats", mylogger)
  return(feature_data)
}

generate_noisy_dataset <- function(data,
                                   feature_stats = NULL,
                                   data_for_stats = NULL,
                                   noise_level=0.1, 
                                   instance_proportion=1, 
                                   load_path = NULL,
                                   save_path = load_path,
                                   myseed=123, 
                                   mylogger = NULL)
{
  # TODO: canviar load/save_path per noisy_dataset_path
  log_info_start_process("generate_noisy_dataset", 
                         list(data = sprintf("Data frame of size %d %d", 
                                             nrow(data), ncol(data)),
                              data_for_stats = sprintf("Data frame of size %d %d", 
                                                       nrow(data_for_stats), 
                                                       ncol(data_for_stats)),
                              noise_level = noise_level,
                              instance_proportion = instance_proportion,
                              load_path = load_path,
                              save_path = save_path,
                              myseed = myseed
                         ), mylogger)
  
  perturbed_data <- load_object(load_path, "noisy dataset", mylogger)
  
  if (!is.null(perturbed_data)) {
    return(perturbed_data)
  }
  
  perturbed_data <- data
  
  numcols<-ncol(data)-1
  numrows <- nrow(data)
  
  set.seed(myseed)
  instances_to_perturb <- sample(1:numrows, instance_proportion*numrows)
  
  # Precalculate statistics
  if (is.null(feature_stats)) {
    if (is.null(data_for_stats)) {
      error_msg <- paste("If <feature_stats> parameter is NULL",
                         "<data_for_stats> must be provided to compute feature stats.",
                         sep=" ")
      log_error_message(error_msg, mylogger)
      stop(error_msg)
    }
    else {
      feature_stats <- compute_feature_stats(data_for_stats, mylogger)
    }
  }
  # NOTE: preguntar si les ordinals han de ser tractades igual que les nominals
  set.seed(myseed)
  for (f in (1:numcols)) {
    feature_name <- colnames(data)[f]
    for (i in instances_to_perturb) {
      if (is.factor(data[,f]) || is.integer(data[,f]))
      { # Nominal or ordinal
        f_value <- data[i,f]
        t_array <- rep(0, length(feature_stats$values[[feature_name]]))
        names(t_array) <- feature_stats$values[[feature_name]]
        t_array[feature_stats$values[[feature_name]] == f_value] <- 1
        
        alpha <- 1 - exp(-noise_level)
        probs_new <- rep(0, length(feature_stats$values[[feature_name]]))
        names(probs_new) <- feature_stats$values[[feature_name]]
        for (v_name in names(probs_new)) {
          #v_name <- "2"
          probs_new[v_name] <- (alpha * feature_stats$props[[feature_name]][[v_name]]) + (1 - alpha) * t_array[[v_name]]
        }
        #probs_new <- alpha * feature_data$props[[feature_name]][[f]] + (1 - alpha) * t_array
        new_value <- sample(sample(names(probs_new), 1000, prob=probs_new, replace=TRUE), 1)
        if (is.integer(data[,f])) {
          new_value <- as.integer(new_value)
        }
        else {
          new_value <- factor(new_value, levels=names(probs_new))
        }
        perturbed_data[i,f] <- new_value
      } 
      else 
      { # Numeric
        perturbed_data[i,f] <- as.numeric(
          rnorm(1, mean = data[i,f], 
                sd = feature_stats$sd[[feature_name]] * noise_level)
        )
      }  
    } 
    
  }
  
  save_object(perturbed_data, save_path, "noisy dataset", mylogger)
  
  log_info_message("END generate_noisy_dataset", mylogger)
  return(perturbed_data)
  
}
