command_line_parser <- function(description) {
  command_line <- arg_parser(description)
  
  command_line <- add_argument(command_line, 
                               "--single_dataset", 
                               type="logical", 
                               #default=FALSE,
                               help="Execute the experiments for specified datasets.",
                               flag=TRUE)
  
  command_line <- add_argument(command_line, 
                               "--multidataset", 
                               type="logical", 
                               #default=FALSE,
                               help="Execute the experiments for specified datasets.",
                               flag=TRUE)
  
  command_line <- add_argument(command_line, 
                               "--cross_val", 
                               type="logical", 
                               #default=FALSE,
                               help="Execute the experiments for specified datasets.",
                               flag=TRUE)
  
  command_line <- add_argument(command_line, 
                               "--analytics", 
                               type="logical", 
                               #default=FALSE,
                               help="Execute the experiments for specified datasets.",
                               flag=TRUE)
  
  command_line <- add_argument(command_line, 
                               "--no_print_std", 
                               type="logical", 
                               help="Log only on file, not on console output.",
                               flag=TRUE)
  
  command_line <- add_argument(command_line, 
                               "--config", 
                               nargs = 1,
                               #type="logical", 
                               #default=FALSE,
                               help="Path to JSON file with experiment configuration.")
                               #flag=TRUE)
  
  return(command_line)
}

parse_arguments <- function(command_line, config_file = NULL) {
  argv <- parse_args(command_line)
  if (argv$multidataset) {
    print("Running experiment with letter dataset")
  }
  if (argv$cross_val) {
    print("Running experiment with cross validation")
  }
  if (is.na(argv$no_print_std)) {
    argv$no_print_std <- FALSE
  }
  if (is.na(argv$config)) {
    if (is.null(config_file)) {
      stop("No config file provided. Try this:\n\t--config <path_to_JSON_file>")
    }
    else {
      argv$config <- config_file
    }
    
  }
  
  return(argv)
}