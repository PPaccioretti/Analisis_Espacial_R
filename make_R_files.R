my_files <- list.files(pattern = "MLM*|Test*|Trigo*")

if(!file.exists('R_code')) {
  dir.create('R_code')
}
sapply(my_files, function(f) knitr::purl(f,
output = file.path('R_code', paste0(tools::file_path_sans_ext(basename(f)),".R")), documentation = 0))

