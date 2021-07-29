cache_csv <- function(path, fetch_function, read_csv_function = readr::read_csv) {
  if (file.exists(path)) {
    read_csv_function(path)
  } else {
    data <- fetch_function()
    readr::write_csv(data, path)
    data
  }
}