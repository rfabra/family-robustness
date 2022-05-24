setwd("~/Projects/family-robustness/")

source("src/common/config_setup.R")

# config <- fromJSON(file="files/config/multidataset.json")
config <- config_setup("Experiments with multiple dataset for model robustness assessment.")

if (config$argv$cross_val) {
  source("src/experiments/bins_cross_validation.R")
  multidataset_cross_val_bins(config$datasets,
                              config$paths,
                              config$irt,
                              config$noise,
                              config$models,
                              mylogger = config$logger)
}

if (config$argv$taxonomy) {
  source("src/experiments/taxonomy.R")
  aggregate_and_plot_dendrogram(config$datasets,
                                config$models,
                                config$noise$instance_proportion,
                                config$paths,
                                mylogger = config$mylogger)
} 


if (config$argv$analytics) {
  stop("Not implemented yet")
}
