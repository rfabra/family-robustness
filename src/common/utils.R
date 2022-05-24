load_install <- function(lib_name)
{
  if ((lib_name %in% installed.packages()) == FALSE) 
  {
    install.packages(lib_name)
  }
  require(lib_name, character.only = TRUE)
}

remove_constant_columns <- function(in_data) {
  # Remove 0 variance features
  idx_constant_columns = apply(in_data, 2, FUN = function(x) {all(duplicated(x)[-1L])})
  return(in_data[,!idx_constant_columns])
}

normalize_instance_names <- function(current_names) {
  return(paste("Inst", current_names, sep="."))
}

# TODO: Remove when dead
correct_load <- function (ds_name, ds_data)
{
  if (ds_name == "Click_prediction_small.arff" || 
      ds_name == "JapaneseVowels.arff" || 
      ds_name == "lung-cancer.arff" ||
      ds_name == "monks-problems-1.arff" ||
      ds_name == "monks-problems-2.arff" ||
      ds_name == "monks-problems-3.arff") {
    ds_data <- cbind(ds_data[,2:ncol(ds_data)], ds_data[,1])
    colnames(ds_data)[ncol(ds_data)]<-"Class"
  }
  if (ds_name == "splice.arff" || ds_name == "synthetic_control.arff") {
    # El primer atribut es un identificador de la instància.
    # Això probablement mareje, ja que pot prendre molts valors possibles
    ds_data <- ds_data[,2:ncol(ds_data)]
  }
  colnames(ds_data)[ncol(ds_data)]<-"Class"  
  
  return(ds_data)
}

train_test_split <- function(dataset, dataset_split_percent, myseed=123) {
  set.seed(myseed)
  trainIndex <- createDataPartition(dataset$Class, p=dataset_split_percent, list=FALSE)
  data_train <- dataset[trainIndex,]
  data_test <- dataset[-trainIndex,]
  
  return(list("train"=data_train, "test"=data_test))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}