setwd("C:/Users/jpueyo/Univerza v Ljubljani/EdiCitNet - WP2/citymodels/data_preparation")

x <- sf::st_read("SFL_all.gpkg", layer = "SFL_slope")
neighbourhoods <- sf::st_read("neighbourhoods.geojson")

pGardens = 1
pVacant = 1
pRooftop = 1
perc_garden = c(0.02, 0.3)
perc_vacant = c(0.52, 0.75)
perc_rooftop = c(0.6,0.62)
min_area_garden = 100
min_area_vacant = 100
min_area_rooftop = 100
private_gardens_from = "Normal garden"
vacant_from = "Vacant"
rooftop_from = "Rooftop"
pCommercial = 0
area_field = 'flat_area'

scen <- set_scenario(x, min_area_garden = 10, pCommercial = 0, min_area_rooftop = 10)

green_capita(x)
