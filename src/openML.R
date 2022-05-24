library(OpenML)

get_dataset_from_task <- function(dataset_name, task, mylogger = NULL) {
  log_info_start_process("get_dataset_from_task", 
                         list(dataset_name = dataset_name), mylogger)
  # NOTE: maybe we need to customize for some datasets
  dataset <- task$input$data.set$data
  
  if (dataset_name == "Click_prediction_small.arff" || 
      dataset_name == "JapaneseVowels.arff" || 
      dataset_name == "lung-cancer.arff" ||
      dataset_name == "monks-problems-1.arff" ||
      dataset_name == "monks-problems-2.arff" ||
      dataset_name == "monks-problems-3.arff") {
    dataset <- cbind(dataset[,2:ncol(dataset)], dataset[,1])
    # colnames(dataset)[ncol(dataset)]<-"Class"
  }
  if (dataset_name == "splice.arff" || dataset_name == "synthetic_control.arff") {
    # Eliminar identificadors
    dataset <- dataset[,2:ncol(dataset)]
  }
  if (dataset_name == "letters") {
    n_feats <- ncol(dataset) - 1
    for (i in 1:n_feats) {
      dataset[, i] <- as.integer(dataset[,i])
    }
  }
  
  colnames(dataset)[ncol(dataset)]<-"Class"  
  # rownames(dataset) <- normalize_instance_names(rownames(dataset))
  
  log_info_end_process("get_dataset_from_task", mylogger)
  
  return(dataset)
}

get_runs_from_task <- function(task_id, 
                               path_runs = NULL, 
                               mylogger = NULL) {
  # TODO: listOMLEvalutations
  # TODO: save and load to avoid repetition of downloads
  log_info_start_process("get_runs_from_task",
                         list(task_id = task_id,
                              runs_path = path_runs), mylogger)
  task_runs_total <- load_object(path_runs, 
                       sprintf("runs for task %d", task_id), 
                       mylogger)
  if (!is.null(task_runs_total)) {
    return(task_runs_total)
  }
  
  task_runs_total <- listOMLRuns(task.id=task_id, limit = 1)
  continue <- TRUE
  offset <- 1
  while (continue) {
    # print("Download runs")
    task_runs <- listOMLRuns(task.id=task_id, offset = offset)
    
    if (nrow(task_runs) == 0) {
      continue <- FALSE
    }
    else {
      task_runs_total <- rbind(task_runs_total, task_runs)
      offset <- offset + nrow(task_runs)  
    }
  }
  # TODO: unique() per a eliminar runs repetits (incluint el task.id)
  
  if (is.null(task_runs_total)) {
    log_error_message(sprintf("Could not get the runs for task %d", 
                              task_id), mylogger)
    
  } 
  else {
    save_object(task_runs_total, path_runs, 
                sprintf("runs for task %d", task_id), mylogger)
  }
  log_info_end_process("get_runs_from_task", mylogger)
  return(task_runs_total)
}

load_dataset <- function(dataset_name, task_id, dataset_path=NULL, mylogger = NULL) {
  log_info_start_process("load_dataset",
                         list(dataset_name = dataset_name, task_id = task_id),
                         mylogger)
  dataset <- load_object(dataset_path, sprintf("Dataset %s", datase_name),
                         mylogger = mylogger)
  
  if (is.null(dataset)) {
    task <- getOMLTask(task.id = task_id)
    # task_runs <- get_runs_from_task(task_id)
    dataset <- get_dataset_from_task(dataset_name, task)
    # instance_names <- rownames(dataset)
    # dataset <- correct_load(dataset_name, dataset)
    log_info_end_process("load_dataset", mylogger)
    save_object(dataset, dataset_path, sprintf("Dataset %s", datase_name),
                mylogger = mylogger)
  }
  return(dataset)
}

