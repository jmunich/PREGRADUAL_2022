# Create folders for outputs

code_files <- list.files("code") 

code_files_loc  <- paste0("outputs/code/", code_files[grepl("^0_1_3|^0_2|^0_3|^1_1", code_files)],"/")

code_files_loc <- gsub("\\.R", "", code_files_loc)

lapply(code_files_loc, function(x){
  lapply(paste0(x, c("data", "png", "rds", "tab")), function(y){
    dir.create(y, showWarnings = FALSE, recursive = TRUE)
  })
})

dir.create("outputs/code/0_3_save/report", showWarnings = FALSE)
