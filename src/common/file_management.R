library(fs)
load_object <- function(path_load, object_name = "object", mylogger=NULL) {
  loaded_object <- NULL
  log_info_message(sprintf("Loading %s from <%s>", object_name, path_load), mylogger)
  if (!is.null(path_load)) {
    loaded_object <- tryCatch(
      {
        readRDS(path_load)
      },
      error = function(e) {
        print(e)
        return(NULL)
      }
    )
    if (is.null(loaded_object)) {
      log_info_message(paste("Could not load", object_name, "from:\n", path_load, 
                             "\nSee original error above"), mylogger)
    }
  }
  return(loaded_object)
}

save_object <- function(object_to_save, save_path, object_name="object", mylogger=NULL) {
  if (!is.null(save_path)) {
    log_info_message(sprintf("Saving %s in:\n%s", object_name, save_path), 
                     mylogger)
    saveRDS(object_to_save, file=save_path)
  }
}

build_path <- function(base_path, data, extension) {
  return(path(base_path,
              sprintf("%s.%s",
                      paste(data, collapse="_"), 
                      extension)))
}

file_path_response_matrix <- function(base_path, 
                                      dataset_name, 
                                      num_models) {
  return(path(base_path,
              sprintf("response-matrix_dataset-%s_models-%d.rds", 
                      dataset_name, num_models)))
}

file_name_response_matrix <- function(dataset_name, num_models) {
  return(sprintf("response-matrix_dataset-%s_models-%d.rds", dataset_name, num_models))
}

file_path_IRT <- function(base_path, 
                          dataset_name,
                          n_cycles,
                          chunk_size) {
  return(path(base_path,
              sprintf("IRT-model_dataset-%s_n-cycles-%d_chunk-size-%d.rds",
                      dataset_name, n_cycles, chunk_size)))
}

file_name_IRT <- function(dataset_name, n_cycles, chunk_size) {
  
  return(sprintf("IRT-model_dataset-%s_n-cycles-%d_chunk-size-%d.rds",
                 dataset_name, n_cycles, chunk_size))
  # out_str <- ""
  # if (!is.null(dataset_name)) {
  #   out_str <- sprintf("%s-", dataset_name)
  # }
  # 
  # return(sprintf("%sIRT_model.rds", out_str))
}

file_path_difficulty <- function(base_path, dataset_name, num_models) {
  return(path(base_path, 
              sprintf("difficulty-%s_models-%d.rds", 
                      dataset_name, num_models)))
}

file_name_difficulty <- function(dataset_name, num_models) {
  return(sprintf("difficulty-%s_models-%d.rds", dataset_name, num_models))
}

file_path_dataset_evaluation <- function(base_path, dataset_name) {
  return(path(base_path,
              sprintf("%s-dataset_evaluation.rds", dataset_name)))
}

file_name_results_predicted_global <- function(dataset_name) {
  return(sprintf("%s-results_predicted_global.rds", dataset_name))
}

file_path_robustness_curves <- function(base_path, dataset_name) {
  return(path(base_path,
              sprintf("%s-robustness_curves.rds", 
                      dataset_name)))
}


file_path_noisy_dataset <- function(base_path,
                                    dataset_name,
                                    noise_instance_proportion, 
                                    noise_magnitude,
                                    fold,
                                    bin=NULL) {
  if (is.null(bin)) {
    # TODO: assume bin is always null. Consider fold in the else statement
    return(path(base_path, sprintf("noisy-dataset-%s_fold-%d_noise-%0.2f_instances-%0.2f.rds",
                   dataset_name, fold, noise_magnitude, noise_instance_proportion)))
  }
  else {
    return(path(base_path, sprintf("noisy-dataset-%s_noise-%0.2f_instances-%0.2f_bin-%d.rds",
                   dataset_name, noise_magnitude, noise_instance_proportion, bin)))
  }
}

file_name_noisy_dataset <- function(dataset_name,
                                    noise_instance_proportion, 
                                    noise_magnitude,
                                    fold,
                                    bin=NULL) {
  if (is.null(bin)) {
    # TODO: assume bin is always null. Consider fold in the else statement
    return(sprintf("noisy-dataset-%s_fold-%d_noise-%0.2f_instances-%0.2f.rds",
                   dataset_name, fold, noise_magnitude, noise_instance_proportion))
  }
  else {
    return(sprintf("noisy-dataset-%s_noise-%0.2f_instances-%0.2f_bin-%d.rds",
                   dataset_name, noise_magnitude, noise_instance_proportion, bin))
  }
}

file_path_plot_noisy_instances  <- function(base_path,
                                            dataset_name,
                                            ground_truth,
                                            metric,
                                            noise_magnitude) {
  
  return(path(base_path, sprintf("%s_%s_%s_%0.2f.pdf",
                                 dataset_name, 
                                 ground_truth,
                                 metric,
                                 noise_magnitude)))
}

file_name_plot_noisy_instances  <- function(dataset_name,
                                            ground_truth,
                                            metric,
                                            noise_instance_min,
                                            noise_instance_max,
                                            noise_instance_step,
                                            noise_magnitude,
                                            model_name = "") {
  
  if (model_name == "") {
    return(sprintf("CCC_dataset-%s_gt-%s_metric-%s_noise-%0.2f-instances-from-%0.2f_to-%0.2f_step-%0.2f.pdf",
                   dataset_name, 
                   ground_truth,
                   metric,
                   noise_magnitude,
                   noise_instance_min,
                   noise_instance_max,
                   noise_instance_step
    ))
  }
  else {
    return(sprintf("CCC_dataset-%s_gt-%s_metric-%s_model-%s_noise-%0.2f-instances-from-%0.2f_to-%0.2f_step-%0.2f.pdf",
                   dataset_name, 
                   ground_truth,
                   metric,
                   model_name,
                   noise_magnitude,
                   noise_instance_min,
                   noise_instance_max,
                   noise_instance_step
    ))
  }
  
}

file_name_analytics_noisy_instances  <- function(dataset_name,
                                            ground_truth,
                                            metric,
                                            noise_instance_min,
                                            noise_instance_max,
                                            noise_instance_step,
                                            noise_magnitude,
                                            model_name = "") {
  
  if (model_name == "") {
    return(sprintf("CCC_dataset-%s_gt-%s_metric-%s_noise-%0.2f-instances-from-%0.2f_to-%0.2f_step-%0.2f.rds",
                   dataset_name, 
                   ground_truth,
                   metric,
                   noise_magnitude,
                   noise_instance_min,
                   noise_instance_max,
                   noise_instance_step
    ))
  }
  else {
    return(sprintf("CCC_dataset-%s_gt-%s_metric-%s_model-%s_noise-%0.2f-instances-from-%0.2f_to-%0.2f_step-%0.2f.rds",
                   dataset_name, 
                   ground_truth,
                   metric,
                   model_name,
                   noise_magnitude,
                   noise_instance_min,
                   noise_instance_max,
                   noise_instance_step
    ))
  }
  
}

file_path_predictions_per_bin <- function(base_path,
                                          dataset_name,
                                          noise_magnitude) {
  return(path(base_path,
              sprintf("%s_%0.2f.rds",
                      dataset_name,
                      noise_magnitude)))
}

file_name_analytics_noisy_instances_bins  <- function(dataset_name,
                                                 ground_truth,
                                                 metric,
                                                 noise_instance_min,
                                                 noise_instance_max,
                                                 noise_instance_step,
                                                 noise_magnitude,
                                                 model_name = "") {
  
  if (model_name == "") {
    return(sprintf("CCC_dataset-%s_gt-%s_metric-%s_noise-%0.2f-instances-from-%0.2f_to-%0.2f_step-%0.2f_bins.rds",
                   dataset_name, 
                   ground_truth,
                   metric,
                   noise_magnitude,
                   noise_instance_min,
                   noise_instance_max,
                   noise_instance_step
    ))
  }
  else {
    return(sprintf("CCC_dataset-%s_gt-%s_metric-%s_model-%s_noise-%0.2f-instances-from-%0.2f_to-%0.2f_step-%0.2f_bins.rds",
                   dataset_name, 
                   ground_truth,
                   metric,
                   model_name,
                   noise_magnitude,
                   noise_instance_min,
                   noise_instance_max,
                   noise_instance_step
    ))
  }
  
}

file_path_model <- function(base_dir, dataset_name, model_name, fold) {
  return(path(base_dir,
              sprintf("model-%s_dataset-%s_fold-%d.rds", 
                      model_name, dataset_name, fold)))
}

file_name_model <- function(dataset_name, model_name, fold) {
  return(sprintf("model-%s_dataset-%s_fold-%d.rds", model_name, dataset_name, fold))
}

file_path_predictions <- function(base_path, dataset_name, 
                                  model_name, split, fold, bin=NULL) {
  if(is.null(bin)) {
    return(path(base_path, sprintf("predictions_dataset-%s_model-%s_fold-%d_split-%s.rds", 
                   dataset_name, model_name, fold, split)))
  }
  else {
    return(path(base_path, sprintf("predictions_dataset-%s_model-%s_split-%s_bin-%d.rds", 
                   dataset_name, model_name, split, bin)))
  }
}

file_name_predictions <- function(dataset_name, model_name, split, fold, bin=NULL) {
  if(is.null(bin)) {
    return(sprintf("predictions_dataset-%s_model-%s_fold-%d_split-%s.rds", 
                   dataset_name, model_name, fold, split))
  }
  else {
    return(sprintf("predictions_dataset-%s_model-%s_split-%s_bin-%d.rds", 
                   dataset_name, model_name, split, bin))
  }
}


# get path functions ------------------------------------------------------



get_path_oml_runs <- function(base_dir, dataset_name) {
  out_path <- path(base_dir, dataset_name)
  dir.create(out_path,
             showWarnings = FALSE,
             recursive = TRUE)
  return(path(out_path, "oml_runs.rds"))
}

get_path_response_matrix <- function(base_dir, dataset_name, num_runs) {
  out_path <- path(base_dir, dataset_name)
  dir.create(out_path,
             showWarnings = FALSE,
             recursive = TRUE)
  return(path(out_path, sprintf("response_matrix-%d.rds", num_runs)))
}

get_path_irt <- function(base_dir, dataset_name) {
  out_path <- path(base_dir, dataset_name)
  dir.create(out_path,
             showWarnings = FALSE,
             recursive = TRUE)
  return(path(out_path, "irt_model.rds"))
}

get_path_difficulty <- function(base_dir, dataset_name, num_bins) {
  out_path <- path(base_dir, dataset_name)
  dir.create(out_path,
             showWarnings = FALSE,
             recursive = TRUE)
  return(path(out_path, sprintf("difficulty-%d.rds", num_bins)))
}
