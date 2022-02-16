library(fairml)
library(moccf)
adult = fairml::adult

get_rf_classif_adult = function() {
  file_path = "test_files/rf_classif_adult.RDS"
  if (!file.exists(file_path)) {
    if (!dir.exists("test_files")) {
      dir.create("test_files")
    }
    rf = randomForest::randomForest(income ~ ., data = adult, ntree = 20L)
    saveRDS(rf, file = file_path) 
  }
  readRDS(file_path)
}

save_test_png = function(code, width = 400, height = 400) {
  path = tempfile(fileext = ".png")
  cowplot::save_plot(path, code)
  path
}

quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
