#' @title Set the scenario for your edible city
#' @description You can adjust different parameters to define different city scenarios.
#' The object must contain a field 'Function' which describes the function or type of each feature.
#' @param x An 'sf' object with the urban model of your city.
#' @param pGardens The proportion of private gardens (Function == 'Gardens') that will become
#' edible gardens.
#' @param pVacant The proportion of vacant plot (Function == 'Vacant') that will become
#' edible gardens.
#' @param pRooftop The proportion of rooftops (Function == 'Flat rooftop') out of rooftops larger than
#' 'min_area_rooftop' that will become edible rooftops.
#' @param perc_garden The proportion in a range of surface in a garden that is occupied by edible plants.
#' @param perc_vacant The proportion in a range of surface in a vacant plot that is occupied by edible plants.
#' @param perc_rooftop The proportion in a range of surface in a rooftop that is occupied by edible plants.
#' @param min_area_garden The minimum area that a garden must have to become an edible garden.
#' @param min_area_vacant The minimum area that a vacant must have to become an community or commercial garden.
#' @param min_area_rooftop The minimum area that a flat rooftop must have to become an edible rooftop.
#' @param private_gardens_from The categories in 'Functions' potentially converted to edible private gardens
#' @param vacant_from The categories in 'Functions' potentially converted to community or commercial gardens
#' @param rooftop_from The categories in 'Functions' potentially converted to edible rooftop
#' (community raised beds or commercial hydroponic)
#' @param pCommercial The proportion of plots and rooftop that will be commercial. The rest will be community gardens
#' In rooftops it is equivalent to raised beds and hydroponic system respectively.
#' @return An 'sf' object as 'x' with the respective proportion of gardens ('Edible private garden'),
#' vacant plots ('Community plot garden', 'Commercial plot garden') and rooftop gardens ('Community rooftop garden',
#' 'Commercial hydroponic rooftop')
#' labelled as edible gardens.
#' @area_field The field to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()

set_scenario <- function(x,
                         pGardens = 1,
                         pVacant = 1,
                         pRooftop = 1,
                         perc_garden = c(2, 30),
                         perc_vacant = c(52, 75),
                         perc_rooftop = c(60,62),
                         min_area_garden = 100,
                         min_area_vacant = 100,
                         min_area_rooftop = 100,
                         private_gardens_from = "Normal garden",
                         vacant_from = "Vacant",
                         rooftop_from = "Rooftop",
                         pCommercial = 0,
                         area_field = 'flat_area'
                         ){

  #if area_field is null, calculates de area of each feature
  if (is.null(area_field)) {
    x$area <- sf::st_area(x)
  } else {
    x$area <- as.data.frame(x) %>% .[area_field]
  }

  #CONVERT PRIVATE GARDENS TO EDIBLE GARDENS

  #calculate the total area of private gardens
  area_gardens <- x %>%
    filter(Function %in% private_gardens_from) %>%
    as.data.frame() %>% select(area) %>% sum(.$area)

  #define area of edible gardens as zero
  edible_gardens <- 0

  while (area_gardens > edible_gardens * pGardens){
    #find the largest garden
    gardens <- dplyr::filter(x, Function %in% private_gardens_from)

    x[(x$area == max(gardens$area) & x$Function %in% private_gardens_from),]

  }

  return(x)
}
