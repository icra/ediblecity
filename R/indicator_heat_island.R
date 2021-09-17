#' @title Heat island effect
#' @description The indicator calculates de urban heat island (UHI) using the DPRA guidelines of the Dutch
#' government.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of urban features.
#' @param green_categories A vector of categories that are considered as urban green. If NULL, categories of 'get_categories()'
#' are considered.
#' @param pGreen A vector of same length than green_categories with the percentage of green in each category.
#' If pGreen is NULL, green_categories are considered totally green.
#' @param SVF A 'stars' object representing sky view factor. It can be computed, e.g. with SAGA's
#' Sky View Factor algorithm and then loaded with stars::read_stars().
#' @param Qql A numerical value representing the average solar radiation in W/m2/hour.
#' @param Cair A numerical value representing the air heat capacity in J.
#' @param Pair A numerical value representing the air density in kg/m3.
#' @param Tmax Averaged maximum temperature in ºC.
#' @param Tmin Averaged minimum temperature in ºC.
#' @param return_raster If TRUE, the raster of UHI values is returned. Otherwise, a summary of raster values is returned.
#' @param verbose If TRUE, returns a vector with UHI value in each cell.
#' @details DEFAULT values are the values for 'city_example' dataset in August (averaged values from 2011-2020)
#' @return A 'stars' object with values of UHI. Or a numerical vector or summary statistic for UHI values.
#' See params for more information on how to select each one.
#' @export



## FORMULA (data from August in SFL)
# UHImax = (2 - SVF - Fveg) * ((S * (Tmax - Tmin)^3)/U)^(1/4)
# SVF <- shadow::SVF() Sky view factor (average windows?)
# Fveg <- The amount of green in each pixel (average windows depending on wind
#                             https://support.tygron.com/wiki/Average_calculation_model_(Heat_Overlay))
# S <- Daily average global radiation (W/m2/hr = 6.11) / air heat capacity (=1007) * Pair (=1.14)
# Tmax = 30.8
# Tmin = 20.0

UHI <- function(
                x,
                green_categories = NULL,
                pGreen = NULL,
                SVF,
                Qql = 6.11,
                Cair = 1007,
                Pair = 1.14,
                Tmax = 30.8,
                Tmin = 20.0,
                windspeed = 2.77,
                return_raster = F,
                verbose = F
                ){

  x$green <- ifelse(x$Function %in% unlist(get_categories()),1,0)

  x_rast <- stars::st_rasterize(x['green'], dx=5, dy=5)

  # Reproject SVF if necessary
  if(sf::st_crs(x_rast) != sf::st_crs(SVF)){
    warning("Reprojecting SVF to ", sf::st_crs(x_rast)[[1]])
    SVF <- sf::st_transform(SVF, sf::st_crs(x_rast))
    }

  if(attr(SVF, "dimensions")[[1]]$delta < attr(x_rast, "dimensions")[[1]]$delta){
    SVF <- stars::st_warp(SVF, x_rast)
  }

  S <- Qql /(Cair * Pair)
  result <- (2 - SVF - x_rast) * ((S * (Tmax - Tmin)^3/windspeed)^(1/4))

  if(return_raster) return(result)

  if (verbose) return(dplyr::as_tibble(result) %>%
                        dplyr::filter(!is.na(.[[3]])) %>%
                                 .[[3]]
                      )

  return(summary(dplyr::as_tibble(result)[[3]]))




  return(summary(result))

}



