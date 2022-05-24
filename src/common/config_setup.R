options( java.parameters = "-Xmx6g" )
options(warn=1)

# library(fs)
# library(log4r)
# library(argparser)
# library(rjson)

options(warn=0)
options(java.parameters = "-Xmx6g")

initialize_project_packages <- function() {
  library(OpenML)
  library(mirt)
  library(caret)
  library(RWeka)
  library(fs)
  library(log4r)
  library(argparser)
  library(rjson)
  library(ggplot2)
  library(gridExtra)
  library(ggpubr)
  library(Hmisc)
  library(dplyr)
  library(ggrepel)
  library(wesanderson)
  library(stringr)
  library(earth)
  library(splitTools)
  library(factoextra)
  
  source("src/common/logger.R")
  source("src/common/command_line.R")
  source("src/common/config_setup.R")
  source("src/common/file_management.R")
  source("src/common/utils.R")
  source("src/openML.R")
  source("src/response_matrix.R")
  source("src/irt.R")
  source("src/noisy_dataset.R")
  source("src/analytics/compute.R")
  source("src/analytics/plot.R")
  source("src/models.R")
  source("src/experiments/bins_cross_validation.R")
  #source("src/experiments/hierarchical_clustering.R")
}

initialize_paths <- function(paths, mylogger=NULL) {
  log_info_message("Creating paths...", mylogger)
  base_dir <- paths[1]
  paths_with_base_dir <- paths
  for (p in names(paths[2:length(paths)])) {
    if (p != "analytics")
      paths_with_base_dir[p] <- path(base_dir, paths[p])
    else {
      for (pa in names(paths[[p]])) {
        paths_with_base_dir[[p]][[pa]] <- path(base_dir, paths[[p]][[pa]])
      }
    }
      
  }
  return(paths_with_base_dir)
}

create_directories <- function(paths, mylogger=NULL) {
  log_info_message("Creating directories...", mylogger)
  for (p in names(paths)) {
    if (p != "analytics") {
      log_info_message(paste(p, ":", paths[[p]]), mylogger)
      dir.create(paths[[p]], recursive=TRUE, showWarnings = FALSE)
    }
    else {
      for (pa in names(paths[[p]])) {
        log_info_message(paste(p, ":", paths[[p]][[pa]]), mylogger)
        dir.create(paths[[p]][[pa]], recursive=TRUE, showWarnings = FALSE)
      }
    }
  }
  return(paths)
}

# Parse command line arguments
# and set global flags
# TODO: separate config_file from config_setup.
config_setup <- function(description, config_file = NULL) {
  initialize_project_packages()
  # Parse command line arguments
  # You must define command_line_parser() and parse_arguments()
  # in command_line.R
  command_line <- command_line_parser(description)
  command_args <- parse_arguments(command_line, config_file)
  
  # Initialize config from  config.json file
  if (is.null(config_file)) {
    config <- fromJSON(file=command_args$config)
  }
  else {
    config <- fromJSON(file=config_file)
  }
  config$argv <- command_args
  
  # Initialize paths and create directories
  config$paths <- initialize_paths(config$paths)
  create_directories(config$paths)
  
  # Start logging
  config$logger <- init_logger(get_log_file(config$paths$logs), config$argv$no_print_std)
  log_info_message("Initialized config setup.", config$logger)
  
  # Parse command line arguments
  # You must define command_line_parser() and parse_arguments()
  # in command_line.R
  # command_line <- command_line_parser(config$description, config$logger)
  # config$argv <- parse_arguments(command_line, config$logger)
  
  return(config)
}
