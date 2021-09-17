#' @title Heat island effect
#' @description The indicator calculates de urban heat island (UHI) using the DPRA guidelines of the Dutch
#' government.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of urban features.
#' @param green_categories A vector of categories that are considered as urban green. If NULL, categories of 'get_categories()'
#' are considered.
#' @param pGreen A vector of same length than green_categories with the percentage of green in each category.
#' If pGreen is NULL, green_categories are considered totally green.
#' @param SVF A 'stars' or 'raster' object representing sky view factor. It can be computed, e.g. with 'shadow::SVF()' or
#' with SAGA's Sky View Factor algorithm.
#' @param Qql A numerical value representing the average solar radiation in W/m2/hour.
#' @param Cair A numerical value representing the air heat capacity in J.
#' @param Pair A numerical value representing the air density in kg/m3.
#' @param Tmax Averaged maximum temperature in ºC.
#' @param Tmin Averaged minimum temperature in ºC.
#' @param verbose If TRUE, the raster of UHI values is returned. Otherwise, a summary of raster values is returned.
#' @details DEFAULT values are the values for 'city_example' dataset in August (averaged values from 2011-2020)
#' @returns If verbose is TRUE, the output is a 'raster' object with values of UHI. Otherwise, the output is the
#' summary of UHI in the city.
#' @import stars
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
                verbose = F
                ){

  if(class(SVF) != 'stars'){
    if(class(SVF)[1] == "RasterLayer") {
      SVF <- stars::st_as_stars(SVF)
    } else {
      stop("SVF is not a 'raster' neither a 'stars' object")
    }
  }

    # x$area <- as.numeric(sf::st_area(x))

  x$green <- ifelse(x$Function %in% unlist(get_categories()),1,0)

  x_sp <- as(x, Class = "Spatial")

  x_rast <- stars::st_rasterize(x['green'], dx=5, dy=5)



  x_rast <- raster::raster(crs = raster::crs(x_sp),
                           vals = 0,
                           resolution = c(5, 5),
                           ext = raster::extent(x_sp)) %>%
    raster::rasterize(x_sp, ., field = "green")

  if(st_crs(x_rast) != st_crs(SVF)) SVF <- st_transform(SVF, st_crs(x_rast))

  if(attr(SVF, "dimensions")[[1]]$delta < attr(x_rast, "dimensions")[[1]]$delta){
    agg <- aggregate(SVF, x_rast, mean)
  }





}



