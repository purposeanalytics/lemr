read_latest_file <- function(directory, suffix, fileext = "csv") {
  files <- fs::dir_ls(directory)
  file_names <- basename(files)
  file_dates <- stringr::str_remove(file_names, suffix) %>%
    as.Date()
  latest_date <- file_dates == max(file_dates)
  latest_file <- files[latest_date]

  switch(fileext,
    csv = readr::read_csv(latest_file),
    rds = readRDS(latest_file)
  )
}
