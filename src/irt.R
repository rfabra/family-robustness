library(OpenML)
library(mirt)

modelIRT <- function(dat.irt, 
                     method = "2PL", 
                     n_cycles=300, 
                     remove_abtruse_instances = FALSE, 
                     mylogger=NULL) {
  log_info_start_process("modelIRT", list(
    method = method, 
    n_cycles = n_cycles, 
    remove_abtruse_instances = remove_abtruse_instances
  ), mylogger)
  
  if(method == "2PL"){
    log_info_message("START. Fitting the 2PL model", mylogger)
    fit <- mirt(dat.irt, 1, itemtype = '2PL', technical = list(NCYCLES = n_cycles))
    
  }else{
    if(method == "1PL"){
      
    }else{
      log_info_message("START. Fitting the 1PL model", mylogger)
      fit <- mirt(dat.irt, 1, itemtype = '1PL', technical = list(NCYCLES = n_cycles))
    }
  }
  log_info_message("...END fit", mylogger)
  
  log_info_message("Extracting parameters...", mylogger)
  temp = coef(fit, simplify = T, IRTpars =T)$items
  params <- data.frame(temp[,c("g","b","a")])
  # colnames(params)<-c("Gussng","Dffclt","Dscrmn")
  colnames(params)<-c("Gussng","Difficulty","Dscrmn")
  params <- cbind(params, data.frame(row_id = colnames(dat.irt)))
  rownames(params) <- colnames(dat.irt)
  
  if (remove_abtruse_instances) {
    log_info_message("Filtering abtruse items (with negative discrimination)")
    params <- params[params$Dscrmn >= 0,]
  }
  
  # computing the abilities 'ab_vector' of the respondents   
  print("Extracting ability...")
  abil<- as.data.frame(t(fscores(fit)))
  colnames(abil) = rownames(dat.irt)
  rownames(abil) = NULL
  log_info_message("...END IRT function", mylogger)
  return(list("model"=fit, "parameters"=params, "abilities"=abil))
}

incremental_IRT <- function(response_matrix, 
                            method = "2PL", 
                            n_cycles=300, 
                            chunk_size=300, 
                            remove_abtruse_instances = FALSE,
                            path_irt = NULL,
                            mylogger = NULL) {
  # TODO: (1) chunk_size = sqrt(num_examples)
  num_examples <- ncol(response_matrix)
  chunk_size <- ceil(sqrt(num_examples))
  # Fer prints del model, per a vore les logistiques i si s'ajusten
  irt_model <- load_object(path_irt, "IRT model", mylogger)
  if (is.null(irt_model)) {
    log_info_message("The IRT model cannot be loaded.", mylogger)
  }
  else {
    return(irt_model)
  }
  for (i in seq(1, num_examples, chunk_size)) {
    log_info_message(sprintf("Current index %d", i), mylogger)
    current_chunk <- response_matrix[,i:min(i + chunk_size - 1, ncol(response_matrix))]
    
    irt_result <- modelIRT(current_chunk, 
                           method, 
                           n_cycles, 
                           remove_abtruse_instances,
                           mylogger)
    
    if (!exists("irt_result_total")) {
      irt_result_total <- irt_result
    }
    else {
      irt_result_total$parameters <- rbind(irt_result_total$parameters, irt_result$parameters)
    }
  }
  
  save_object(irt_result_total, path_irt, "IRT model", mylogger)
  
  return(irt_result_total)
}



extract_difficulty_data <- function(irt_data, irt_num_bins, 
                                    path_difficulty, mylogger = NULL) {
  
  difficulty_data <- load_object(path_difficulty, "Difficulty data", mylogger)
  if (is.null(difficulty_data)) {
    log_info_message("The Difficulty data cannot be loaded.", mylogger)
  }
  else {
    return(difficulty_data)
  }
  
  difficulty <- dplyr::select(irt_data$parameters, Difficulty, row_id)
  
  # Filter outliers and add difficulty bins
  # TODO: (4) This is done manually. How to automate it?
  difficulty <- difficulty %>%
    dplyr::filter(Difficulty > -6) %>%
    dplyr::filter(Difficulty <  6) %>%
    dplyr::mutate(Bin=cut2(Difficulty,
                           g=irt_num_bins))
  
  difficulty_bins <- sort(unique(difficulty$Bin))
  
  difficulty_data <- list(difficulty = difficulty, 
                          bins = difficulty_bins)
  save_object(object_to_save = list(difficulty = difficulty, bins = difficulty_bins),
              save_path = path_difficulty,
              object_name = "Difficulty data")
  
  return(difficulty_data)
}
