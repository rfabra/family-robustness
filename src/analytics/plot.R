library(ggplot2)
library(gridExtra)

plot_real_class_distribution <- function(class_distribution, path_plot, mylogger = NULL) {
  
  class_distribution$Noisy.Instances <- paste("Noisy\nProp.\n", class_distribution$Noisy.Instances)
  class_distribution$Num.Bin <- paste("Bin", class_distribution$Num.Bin)
  
  class_distribution <- dplyr::select(class_distribution, Num.Bin, 
                                      Real, Real.Count) %>%
    dplyr::distinct()
  p <- ggplot(class_distribution, aes(x=Real, y=Real.Count, fill=Num.Bin)) + 
    geom_bar(stat="identity") +
    xlab("Class") + ylab("Real Class Freq.") +
    theme_minimal() +
    geom_text(aes(label=Real.Count), vjust=0) +
    facet_grid(. ~ Num.Bin)

  pdf_width <- ceil(1.5 * (length(unique(class_distribution$Real)) +
                        length(unique(class_distribution$Num.Bin)) +
                        length(unique(class_distribution$Noisy.Instances))))
  
  pdf_height <- ceil(0.6 * pdf_width)
  
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  pdf(path_plot, width = pdf_width, height = pdf_height)
  print(p)
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
  
  return(p)
}

plot_predicted_class_distribution <- function(class_distribution,  path_plot, mylogger = NULL) {
  
  class_distribution$Noisy.Instances <- paste("Noisy\nProp.\n", class_distribution$Noisy.Instances)
  class_distribution$Num.Bin <- paste("Bin", class_distribution$Num.Bin)
  p <- ggplot(class_distribution, aes(x=Predicted.Noisy, y=Predicted.Noisy.Count, fill=Num.Bin)) + 
    geom_bar(stat="identity") +
    xlab("Class") + ylab("Predictions Class Freq.") +
    theme_minimal() +
    geom_text(aes(label=Predicted.Noisy.Count), vjust=0) +
    facet_grid(Model ~ Noisy.Instances + Num.Bin)
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  pdf_width <- ceil(1.5 * (length(unique(class_distribution$Predicted.Noisy)) +
    length(unique(class_distribution$Num.Bin)) +
    length(unique(class_distribution$Noisy.Instances))))
  
  pdf_height <- ceil(7 * length(unique(class_distribution$Model)))
  
pdf(path_plot, width = pdf_width, height = pdf_height)
  print(p)
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
  
  return(p)
}


plot_robustness_curves <- function(robustness_curves, y_axis_name, path_plot, mylogger = NULL) {
  # TODO: aes_string(x="Difficulty", y=y_axis_name) and remove so many ifs
  log_info_start_process("plot_analytics",
                         list(analytics = sprintf("data frame of size %d, %d", 
                                                  nrow(robustness_curves), ncol(robustness_curves)),
                              y_axis_name = y_axis_name,
                              path_plot = path_plot),
                         mylogger)
  model_names <- unique(robustness_curves$Model)
  plot_limit <- ylim(0,1)
  if (grepl("Kappa", y_axis_name)) {
    plot_limit <- ylim(0,1)
  }
  
  plot_list <- list()
  for (model_name in model_names) {
    log_info_message(sprintf("Processing model %s", model_name), mylogger)
    model_plot_data <- dplyr::filter(robustness_curves, Model == model_name)
    
    plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + plot_limit +
      geom_line(data=model_plot_data, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
      geom_point(data=model_plot_data, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances"))
  }
  
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  pdf(path_plot, width = 14, height = 20)
  acc_vs_dfclt <- grid.arrange(grobs = plot_list, ncol=2)
  print(acc_vs_dfclt)
  
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
  
  return(plot_list)
  
}

plot_analytics_cross_val <- function(analytics, y_axis_name, path_plot, mylogger = NULL) {
  log_info_start_process("plot_analytics",
                         list(analytics = sprintf("data frame of size %d, %d", 
                                                  nrow(analytics), ncol(analytics)),
                              y_axis_name = y_axis_name,
                              path_plot = path_plot),
                         mylogger)
  model_names <- unique(analytics$Model)
  plot_list <- list()
  for (model_name in model_names) {
    log_info_message(sprintf("Processing model %s", model_name), mylogger)
    model_plot_data <- filter(analytics, Model == model_name)
    
    if (y_axis_name == "Accuracy") {
      log_info_message("Y axis: Accuracy", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(0,1) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Accuracy, colour=Noisy.Instances)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Accuracy, colour=Noisy.Instances))
    }
    else if (y_axis_name == "Error") {
      log_info_message("Y axis: Error", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(0,1) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Error, colour=Noisy.Instances)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Error, colour=Noisy.Instances))
    }
    else if (y_axis_name == "Kappa.Predicted") {
      log_info_message("Y axis: Kappa", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(0,1) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Kappa.Predicted, colour=Noisy.Instances)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Kappa.Predicted, colour=Noisy.Instances))
    }
    else if (y_axis_name == "Global.Kappa") {
      log_info_message("Y axis: Global.Kappa", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(-2,2) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Global.Kappa, colour=Noisy.Instances)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Global.Kappa, colour=Noisy.Instances))
    }
    else {
      log_error_message(sprintf("Metric <%s> not recoginized. Try one of: <Accuracy>, <Error>, <Kappa>, <Global.Kappa>"), mylogger)
      stop(sprintf("Metric <%s> not recoginized. Try one of: <Accuracy>, <Error>, <Kappa>"))
    }
  }
  
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  pdf(path_plot, width = 14, height = 20)
  acc_vs_dfclt <- grid.arrange(grobs = plot_list, ncol=2)
  print(acc_vs_dfclt)
  
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
}

plot_analytics <- function(analytics, y_axis_name, path_plot, mylogger = NULL) {
  log_info_start_process("plot_analytics",
                         list(analytics = sprintf("data frame of size %d, %d", 
                                                  nrow(analytics), ncol(analytics)),
                              y_axis_name = y_axis_name,
                              path_plot = path_plot),
                         mylogger)
  model_names <- unique(analytics$Model)
  plot_list <- list()
  for (model_name in model_names) {
    log_info_message(sprintf("Processing model %s", model_name), mylogger)
    model_plot_data <- filter(analytics, Model == model_name)
    
    if (y_axis_name == "Accuracy") {
      log_info_message("Y axis: Accuracy", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(0,1) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Accuracy, colour=Split)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Accuracy, colour=Split))
    }
    else if (y_axis_name == "Error") {
      log_info_message("Y axis: Error", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(0,1) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Error, colour=Split)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Error, colour=Split))
    }
    else if (y_axis_name == "Kappa") {
      log_info_message("Y axis: Kappa", mylogger)
      plot_list[[model_name]] <- ggplot() + ggtitle(model_name) + ylim(0,1) +
        geom_line(data=model_plot_data, aes(x=Difficulty, y=Kappa, colour=Split)) +
        geom_point(data=model_plot_data, aes(x=Difficulty, y=Kappa, colour=Split))
    }
    else {
      log_error_message(sprintf("Metric <%s> not recoginized. Try one of: <Accuracy>, <Error>, <Kappa>"), mylogger)
      stop(sprintf("Metric <%s> not recoginized. Try one of: <Accuracy>, <Error>, <Kappa>"))
    }
  }
  
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  
  pdf(path_plot, width = 14, height = 20)
  acc_vs_dfclt <- grid.arrange(grobs = plot_list, ncol=2)
  print(acc_vs_dfclt)
  
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
}