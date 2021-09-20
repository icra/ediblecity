## code to prepare `city_functions` dataset goes here

city_functions <- read.csv2("C:/Users/jpueyo/Univerza v Ljubljani/EdiCitNet - WP2/citymodels/ediblecity/data-raw/city_functions.csv")

usethis::use_data(city_functions, overwrite = TRUE)
