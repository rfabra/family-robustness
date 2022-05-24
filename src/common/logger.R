library(log4r)

get_log_file <- function(logs_path) {
  return(path(logs_path, format(Sys.time(), "%m-%d_%X.log")))
}

init_logger <- function(logs_path, no_print_std=FALSE) {
  # TODO: (5) perform here get_log_file
  if (no_print_std)
    return(log4r::logger("DEBUG", appenders= list(file_appender(logs_path))))
  return(log4r::logger("DEBUG", appenders= list(console_appender(), 
                                                file_appender(logs_path))))
}

log_debug_message <- function(message, logger=NULL) {
  if (!is.null(logger)) {
    log4r::debug(logger, message)
  }
}

log_info_message <- function(message, logger=NULL) {
  if (!is.null(logger)) {
    log4r::info(logger, message)
  }
}

log_warn_message <- function(message, logger=NULL) {
  if (!is.null(logger)) {
    log4r::warn(logger, message)
  }
}

log_error_message <- function(message, logger=NULL) {
  if (!is.null(logger)) {
    log4r::error(logger, message)
  }
}

log_info_start_process <- function(func_name, parameters, logger=NULL) {
  if (!is.null(logger)) {
    out_log <- sprintf("Executing %s(\n", func_name)
    for (param in names(parameters))
      out_log <- paste(out_log, "    ", param, " = ", parameters[param], ",\n", sep="")
    out_log <- paste(out_log, ")", sep="")
    log4r::info(logger, out_log)
  }
}

log_info_end_process <- function(func_name, logger=NULL) {
  if (!is.null(logger)) {
    log4r::info(logger, sprintf("Done (%s)", func_name))
  }
}
