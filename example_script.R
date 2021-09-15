library(tidyverse)
setwd("C:/Users/jpueyo/Univerza v Ljubljani/EdiCitNet - WP2/citymodels/data_preparation")

x <- sf::st_read("SFL_all.gpkg", layer = "SFL_slope")


houses <- c(
  "Contemporary midlevel house",
  "Mid-century midlevel house",
  "Mid-century midlevel appartments",
  "Offices with houses",
  "Shops with houses",
  "Warehouses with houses"
)

city_example <- x %>%
  mutate(Function_verbose = ifelse(Function_verbose %in% houses, "Residence", Function_verbose))

usethis::use_data(city_example)

setwd("C:/Users/jpueyo/Univerza v Ljubljani/EdiCitNet - WP2/citymodels/data_preparation")
neighbourhoods <- sf::st_read("neighbourhoods.geojson") %>%
  select(NOMBARRI, POBLACI)

names(neighbourhoods)[1:2] <- c('name', 'inhabitants')

setwd("C:/Users/jpueyo/Univerza v Ljubljani/EdiCitNet - WP2/citymodels/ediblecity")
usethis::use_data(neighbourhoods, overwrite = T)


scen <- set_scenario(x, min_area_garden = 10, pCommercial = 0, min_area_rooftop = 10)

inner_join(base_table, full_table, by="NOMBARRI")

green_distance(city_example)

base_table <- green_capita(city_example,
                           neighbourhoods = neighbourhoods,
                           name_col = 'name',
                           inh_col = 'inhabitants',
                           verbose = T,
                           private = F)
