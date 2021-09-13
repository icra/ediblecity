setwd("C:/Users/jpueyo/Univerza v Ljubljani/EdiCitNet - WP2/citymodels/data_preparation")

x <- st_read("SFL_all.gpkg", layer = "SFL_slope")

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

scen <- set_scenario(x, min_area_garden = 10, pCommercial = 1)

vacant_cat <- c("Community garden", "Commercial garden")

sum(scen$Function == "Edible private garden")
sum(scen$Function == "Commercial garden")
sum(scen$Function == "Community garden")

sum(x$Function == "Normal garden" & x$flat_area >= 10)

min(scen$edible_area[scen$Function == "Edible private garden"]/scen$flat_area[scen$Function == "Edible private garden"])
max(scen$edible_area[scen$Function == "Edible private garden"]/scen$flat_area[scen$Function == "Edible private garden"])
min(scen$edible_area[scen$Function %in% vacant_cat]/scen$flat_area[scen$Function %in% vacant_cat])
max(scen$edible_area[scen$Function %in% vacant_cat]/scen$flat_area[scen$Function %in% vacant_cat])
