#' @title Sequestration of nitrogen dioxide
#' @description This indicator returns the amount of NO2 that is sequestered by urban green.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of
#' urban features.
#' @param green_df A dataframe of categories that are considered as urban green with two columns. 'functions'
#' with the names of 'Function' in 'x' to be considered as green and a 'no2_seq' columns with NO2
#' sequestration capacity of each function. If NULL, categories and values of 'city_functions' dataset are
#' considered.
#' @param traffic_rast A 'stars' object with 'traffic flow' in units per hour.
#' @param traffic_col The column in 'x' representing the traffic flow in each function (in units per hour). If
#' 'traffic_rast' is provided, this param is ignored
#' @param traffic_lanes If previous both are NULL, then 'traffic lanes' is used to determine which functions in
#' 'x' have traffic flow.
#' @param traffic_flow If ' traffic_raster' and 'traffic_col' are NULL. This value is assumed as constant in all
#' 'traffic_lanes'.
#' @return lore ipsum
#' @export
#'

no2_seq <- function(x,
                    green_df = NULL,
                    traffic_rast = NULL,
                    traffic_col = NULL,
                    traffic_lanes = "Streets",
                    traffic_flow = 120,
                    wind_speed = 2.77,
                    return_raster = F,
                    verbose = F
                    ){



  if(!is.null(traffic_rast)){
    cellsize <- stars::st_dimensions(traffic_rast)[[1]]$delta
    window <- as.integer(100/cellsize)
    if (window %% 2 == 0) window <- window + 1
    traffic <- starsExtra::focal2(traffic_rast, matrix(1,window,window), "mean")
  } else if(!is.null(traffic_col)){
    traffic <- stars::st_rasterize(x[traffic_col], dx=5, dy=5) %>%
      starsExtra::focal2(., matrix(1,21,21), "mean")
  } else {
    x <- x %>%
      mutate(traffic = ifelse(Function %in% traffic_lanes, traffic_flow, 0))
    traffic <- stars::st_rasterize(x['traffic'], dx=5, dy=5) %>%
      starsExtra::focal2(., matrix(1,21,21), "mean")
  }

  if(is.null(green_df)){
    green_df <- city_functions %>%
      select(functions, no2_seq)
  }

  x <- left_join(x,green_df, by=c("Function" = "functions"))

  sequest <- stars::st_rasterize(x['no2_seq'], dx=5, dy=5)

  if(sf::st_crs(traffic) != sf::st_crs(sequest)) traffic <- sf::st_transform(traffic, sf::st_crs(sequest))

  balance <- (traffic * 0.02904) - sequest

  names(balance) <- "balance"

  balance$balance[balance$balance < 0 & !is.na(balance$balance)] <- 0


  if (return_raster) return(balance)
  if (verbose) return (balance %>% as_tibble() %>% filter(!is.na(balance)) %>% .$balance)

  return(summary(as_tibble(balance) %>% filter(!is.na(balance)) %>%  .$balance))


}

