# function to cache csv of prepared data. If csv already in cache will read data
# from cache, if not will run defined script to produce and then cache data

cache_csv <- function(path, fetch_function, read_csv_function = readr::read_csv) {
  if (file.exists(path)) {
    read_csv_function(path)
  } else {
    data <- fetch_function()
    readr::write_csv(data, path)
    data
  }
}