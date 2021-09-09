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
#' @param min_area_rooftop The minimum area that a flat rooftop must have to become an edible rooftop.
#' @return An 'sf' object as 'x' with the respective proportion of gardens, vacant plots and rooftop
#' labelled as edible gardens.
#' @export normalHist

set_scenario <- function(x,
                         pGardens = 1,
                         pVacant = 1,
                         pRooftop = 1,
                         min_area_rooftop = 100){

  return(x)
}
