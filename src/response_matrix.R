get_response_matrix <- function(dataset, task_runs, num_runs, 
                                path_response_matrix = NULL, 
                                mylogger = NULL
) {
  log_info_start_process("get_response_matrix", list(num_runs = num_runs,
                                                     path_load_response_matrix = path_response_matrix,
                                                     path_save_response_matrix = path_response_matrix
                                                     ), mylogger)
  
  response_matrix <- load_object(path_response_matrix, 
                                 "response matrix",
                                 mylogger)
  if (is.null(response_matrix)) {
    log_info_message("The response matrix cannot be loaded.", mylogger)
  }
  else {
    return(response_matrix)
  }
  irt_input <- data.frame(matrix(nrow = 0, ncol = nrow(dataset)))
  colnames(irt_input) <- rownames(dataset)
  
  set.seed(123)
  random_runs_id <- sample(task_runs$run.id)
  
  true_labels <- dplyr::select(dataset, Class) %>%
    dplyr::mutate(row_id = rownames(dataset))
  
  available_runs <- length(random_runs_id)
  i <- 1
  for (run_id in random_runs_id) {
    log_info_message(sprintf("Collected runs: (%d/%d) from %d available", 
                             i - 1, num_runs, available_runs), mylogger)
    if (i - 1  == num_runs) {
      break
    }
    log_info_message(paste("Run index:", i, "run ID:", run_id), mylogger)
    # Get the run
    run <- tryCatch(
      getOMLRun(run.id = run_id),
      error = function(x) {
        log_info_message(x, mylogger)
        return(NULL)
      })
    
    # If we got the run, add information
    if (!is.null(run)) {
      pred_data <- run$predictions %>% 
        dplyr::select(row_id, prediction=prediction) %>%
        #mutate(row_id = normalize_instance_names(row_id)) %>%
        group_by(row_id) %>% 
        summarise(prediction = Mode(prediction))
      result <- merge(pred_data, true_labels, by="row_id") %>%
        mutate(Hit=as.numeric(prediction == Class))
      response <- data.frame(matrix(nrow = 1, ncol = nrow(dataset)))
      colnames(response) <- result$row_id
      # colnames(response) <- nrow(dataset)
      response[1,] <- result$Hit
      irt_input <- rbind(irt_input, response)
      i <- i + 1
    }
  }
  
  
  if (i < num_runs) {
    log_warn_message(sprintf("Could not get all the runs.\nRequired runs: %d\nValid runs: %d",
                             num_runs, i),
                     mylogger)
  }
  irt_input <- remove_constant_columns(irt_input)
  save_object(irt_input, path_response_matrix, "response matrix", mylogger)
  
  log_info_message("END get_response_matrix", mylogger)
  return(irt_input)
}