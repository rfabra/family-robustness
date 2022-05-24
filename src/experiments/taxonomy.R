source("src/common/config_setup.R")

initialize_project_packages()

aggregate_and_plot_dendrogram <- function(datasets_names, 
                                          models_data,
                                          noise_instance_proportions,
                                          paths,
                                          mylogger = NULL) {
  
  path_dend <- build_path(paths$taxonomy, c("Kappa"), "rds")
  if (file.exists(path_dend)) {
    dend_data <- load_object(path_dend, "taxonomy")
  } else {
    for (dataset_name in datasets_names) {
      path_robustness_curves <- build_path(
        paths$robustness_curves, c(dataset_name), "rds"
      )
      robustness_curves <- readRDS(path_robustness_curves)
      
      difficulty_bins <- sort(unique(robustness_curves$Bin))
      
      column_names <- unique(paste(sprintf("Bin.%i", robustness_curves$Num.Bin),
                                   sprintf(".Noisy.%s", robustness_curves$Noisy.Instances), sep=""))
      if (!exists("dend_data")) {
        dend_data <- data.frame(matrix(ncol=length(column_names), nrow=length(models_data)))
        colnames(dend_data) <- column_names
        rownames(dend_data) <- models_data
        dend_data[is.na(dend_data)] <- 0
      }
      
      for (model_name in models_data) {
        for (noise_instance in noise_instance_proportions) {
          for (d_bin in difficulty_bins) {
            i_bin <- which(d_bin == difficulty_bins)
            df_result <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == i_bin, Noisy.Instances == noise_instance)
            col_idx <- sprintf("Bin.%i.Noisy.%s", i_bin, noise_instance)
            
            dend_data[model_name, col_idx] <-
              dend_data[model_name, col_idx] + df_result$Kappa.Predicted
          }
        }
      }
    }
    dend_data <- dend_data/length(datasets_names)
    save_object(dend_data, path_dend, "dendrogram data", mylogger)  
  }
  
  dend_clust <- hclust(dist(dend_data))
browser()
  path_dend_plot <- build_path(paths$analytics$taxonomy, c("Kappa"), "pdf")
  pdf(path_dend_plot, width=3, height=4.5)
  nd <- fviz_dend(dend_clust, 3, #k = 20, # Cut in K groups
                  cex = 0.4, # label size
                  #k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                  show_labels = TRUE,
                  color_labels_by_k = T, # color labels by groups
                  # rect = T, # Add rectangle around groups
                  #rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
                  rect_fill = F,
                  horiz = FALSE,
                  main = "",
                  labels_track_height = 0.4,
                  rect_lty = 2,
                  lwd = 0.3
  )
  
  nd
  dev.off()
  
  return(TRUE)
}
# 
# stop("Review parameters!")
# 
# models_data <- c(
#   "mlp_7",
#   # "multinom",
#   # "fda_prune17",
#   # "simpls_ncomp3",
#   # "rda",
#   "knn_k3",
#   # "lvq_3",
#   # "svmPoly_d_2_s_0.1",
#   # "gbm_3_50",
#   # "PART",
#   # "JRip",
#   # "ctree_c0.05",
#   "C5.0",
#   # "RPART",
#   # "rf_mtry64",
#   # # "rfRules_mtry64",
#   "NB"
#   # "rbf"
# )
# 
# dataset_data <- list(letter2=1886,
#                      JapaneseVowels=3839,
#                      optdigits = 1792,
#                      pendigits = 3882,
#                      segment = 146822,
#                      vehicle = 3857,
#                      kc1 = 4622)
# 
# aggregate_and_plot_dendrogram(datasets_names = names(dataset_data),
#                               models_data = models_data,
#                               path_plot = "tests/files/ragnar_cross_val_2/analytics/")























# 
# 
# source("src/common/config_setup.R")
# 
# initialize_project_packages()
# 
# aggregate_and_plot_dendrogram <- function(datasets_names, 
#                                           models_data, 
#                                           path_plot) {
#   
#   for (dataset_name in datasets_names) {
#     path_analytics <- path(path_plot,
#                            file_name_analytics_noisy_instances(
#                              dataset_name = dataset_name,
#                              ground_truth = "predicted",
#                              metric = "Accuracy-Kappa",
#                              noise_instance_min = 0.0,
#                              noise_instance_max = 0.0,
#                              noise_instance_step = 0.0,
#                              noise_magnitude = 0.2))
#     result_predicted <- readRDS(path_analytics)
#     
#     difficulty_bins <- sort(unique(result_predicted$Bin))
#     
#     column_names <- unique(paste(sprintf("Bin.%i", result_predicted$Num.Bin),
#                                  sprintf(".Noisy.%s", result_predicted$Noisy.Instances), sep=""))
#     if (!exists("dend_data")) {
#       dend_data <- data.frame(matrix(ncol=length(column_names), nrow=length(models_data)))
#       colnames(dend_data) <- column_names
#       rownames(dend_data) <- models_data
#       dend_data[is.na(dend_data)] <- 0
#     }
#     
#     noise_instance_proportions <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
#     for (model_name in models_data) {
#       for (noise_instance in noise_instance_proportions) {
#         for (d_bin in difficulty_bins) {
#           i_bin <- which(d_bin == difficulty_bins)
#           df_result <-
#             result_predicted %>%
#             dplyr::filter(Model == model_name, Num.Bin == i_bin, Noisy.Instances == noise_instance)
#           col_idx <- sprintf("Bin.%i.Noisy.%s", i_bin, noise_instance)
#           dend_data[model_name, col_idx] <-
#             dend_data[model_name, col_idx] + df_result$Accuracy
#         }
#       }
#     }
#   }
#   
#   dend_data <- dend_data/length(datasets_names)
#   
#   path_dend <- path(path_plot,
#                     "dendrogram.pdf")
#   
#   pdf(path_dend)
#   
#   plot(hclust(dist(dend_data)), 
#        main = "Model Families by Robustness", 
#        xlab="ML models", 
#        ylab = "Height (Accuracy)", 
#        sub="")
#   dev.off()
#   
#   return(TRUE)
# }
# 
# stop("Review parameters!")
# 
# models_data <- c(
#   "mlp_7",
#   # "multinom",
#   # "fda_prune17",
#   # "simpls_ncomp3",
#   # "rda",
#   "knn_k3",
#   # "lvq_3",
#   # "svmPoly_d_2_s_0.1",
#   # "gbm_3_50",
#   # "PART",
#   # "JRip",
#   # "ctree_c0.05",
#   "C5.0",
#   # "RPART",
#   # "rf_mtry64",
#   # # "rfRules_mtry64",
#   "NB"
#   # "rbf"
# )
# 
# dataset_data <- list(letter2=1886,
#                      JapaneseVowels=3839,
#                      optdigits = 1792,
#                      pendigits = 3882,
#                      segment = 146822,
#                      vehicle = 3857,
#                      kc1 = 4622)
# 
# aggregate_and_plot_dendrogram(datasets_names = names(dataset_data),
#                               models_data = models_data,
#                               path_plot = "tests/files/ragnar_cross_val_2/analytics/")
# 
# 
# 
# 
# 
# 
# 
# 
# 














