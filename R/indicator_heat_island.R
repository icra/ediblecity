#' @title Heat island effect
#' @description The indicator calculates de urban heat island (UHI) using the DPRA guidelines of the Dutch
#' government.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of urban features.
#' @param SVF A 'stars' object representing sky view factor. It can be computed, e.g. with SAGA's
#' Sky View Factor algorithm and then loaded with stars::read_stars().
#' @param green_df A dataframe of categories that are considered as urban green with two columns. 'functions'
#' with the names of 'Function' in 'x' to be considered as green; a 'pGreen' column with the percentage of green
#' of that function. If NULL, categories and values of 'city_functions' dataset are considered.
#' @param Qql A numerical value representing the average solar radiation in W/m2/hour.
#' @param Cair A numerical value representing the air heat capacity in J.
#' @param Pair A numerical value representing the air density in kg/m3.
#' @param Tmax Averaged maximum temperature in ºC.
#' @param Tmin Averaged minimum temperature in ºC.
#' @param windspeed Averaged wind speed in m/s.
#' @param return_raster If TRUE, the raster of UHI values is returned. Otherwise, a summary of raster values is returned.
#' @param verbose If TRUE, returns a vector with UHI value in each cell.
#' @details DEFAULT values are the values for 'city_example' dataset in August (averaged values from 2011-2020)
#' @return A 'stars' object with values of UHI. Or a numerical vector or summary statistic for UHI values.
#' See params for more information on how to select each one.
#' @import stars
#' @export

UHI <- function(
                x,
                SVF,
                green_df = NULL,
                Qql = 6.11,
                Cair = 1007,
                Pair = 1.14,
                Tmax = 30.8,
                Tmin = 20.0,
                windspeed = 2.77,
                return_raster = F,
                verbose = F
                ){

  # To avoid notes in R CMD Check
  pGreen <- NULL
  functions <- NULL
  . <- NULL

  check_sf(x)
  if (!("stars" %in% class(SVF))) rlang::abort("SVF must be an object of class 'stars'")

  city_functions <- city_functions %>%
    mutate(pGreen = ifelse(!is.na(pGreen),
                           pGreen,
                           ifelse(location == "rooftop",
                                  0.61,
                                  1))) %>%
    select(functions, pGreen)

  if (is.null(green_df)) green_df <- city_functions

  x <- x %>%
    left_join(green_df, by = c("Function" = "functions")) %>%
    mutate(pGreen = ifelse(is.na(pGreen),
                           0,
                           pGreen))

  x_rast <- stars::st_rasterize(x['pGreen'], dx=5, dy=5)

  # Reproject SVF if necessary
  if(sf::st_crs(x_rast) != sf::st_crs(SVF)){
    warning("Reprojecting SVF to ", sf::st_crs(x_rast)[[1]])
    SVF <- sf::st_transform(SVF, sf::st_crs(x_rast))
    }

  x_rast <- stars::st_warp(x_rast, SVF)

  S <- Qql /(Cair * Pair)
  result <- (2 - SVF - x_rast) * ((S * (Tmax - Tmin)^3/windspeed)^(1/4))
  names(result) <- "UHI"

  if(return_raster) return(result)

  if (verbose) return(dplyr::as_tibble(result) %>%
                        dplyr::filter(!is.na(.[[3]])) %>%
                                 .[[3]]
                      )

  return(summary(dplyr::as_tibble(result)[[3]]))




  return(summary(result))

}

