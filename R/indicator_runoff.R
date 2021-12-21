#' @title Runoff prevention
#' @description The indicator calculates the runoff prevention considering a rain event, the infiltration
#' capacity and the rain harvesting and storage capacity.
#' government.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories
#' of urban features.
#' @param runoff_df A dataframe of categories that are considered impervious area with five columns. 'functions'
#' with the names of 'Function' in 'x' to be considered as impervious; two infiltration columns 'infiltration1'
#' and 'infiltration2 with the range of infiltration capacity (mm in 24 hours) of that function;
#' and two water storage capacity columns 'water_storage1' and 'water_storage2' with the range of water
#' storage capacity of that function. If NULL, categories and values of 'city_functions' dataset
#' are considered.
#' @param rain The amount of 24h-rain to be simulated, default is 100 mm.
#' @param floors_field The column in 'x' containing the number of floors of each building. Zero is considered
#' unbuilt areas like gardens or streets. It is used to calculate rainwater haversting area, since only
#' upper surface are considered. Missing values are considered as zero.
#' @param harvest_dist Maximum distance (in meters) of buildings where to harvest rainwater
#' @param tank_size A two-length vector with the range of tank size possibilities (in l/m2).
#' @details
#' @return It returns a named vector with values of percentage of rainfall infiltred or captured,
#' total rainfall, total infiltration and total rainharvest (all of them in m3)
#' @export


runoff_prev <- function(
                        x,
                        runoff_df = NULL,
                        rain = 100,
                        floors_field = 'floors',
                        harvest_dist = 10,
                        tank_size = c(0,45)
                        ){

  #if no runoff_df, use the city_functions values
  if(is.null(runoff_df)){
    runoff_df <- city_functions %>%
      dplyr::select(
        functions,
        water_storage,
        contains("infiltration")
      )
  }

  #create new field and equal NA floors to zero
  x <- x %>%
    dplyr::mutate(floors_ = !!as.symbol(floors_field),
                 floors_ = ifelse(is.na(floors_), 0, floors_))


  #calculate area
  x$area <- as.numeric(sf::st_area(x))

  #rainfall volume in the city
  rainfall <- rain * sum(x$area)


  #calculate infiltration
  vacant_inf <- c(runoff_df$infiltration1[runoff_df$functions == "Vacant"],
                  runoff_df$infiltration2[runoff_df$functions == "Vacant"])

  infilt_func <- function(u){
    func <- x %>%
      dplyr::filter(Function == u[["functions"]])

    infilt1 <- as.numeric(u[["infiltration1"]])
    infilt2 <- as.numeric(u[["infiltration2"]])

    if (sum(func$edible_area) > 0){
      func <- func %>%
        dplyr::mutate(edible_inf = edible_area * runif(1,infilt1, infilt2),
                      normal_inf = ifelse(floors_ > 0, 0, (area - edible_area) * runif(1, vacant_inf[1], vacant_inf[2])
                      ))
    } else {
      func <- func %>%
        dplyr::mutate(edible_inf = 0,
                      normal_inf = area * runif(1, infilt1, infilt2)
        )
    }
    return(min(sum(func$area) * rain, sum(func$edible_inf) + sum(func$normal_inf)))

  }

  infiltration <- sum(apply(runoff_df, 1, "infilt_func", simplify = T))

  ##calculate rainwater harvesting area and tank size


  #create unique id
  x$id <- rownames(x)

  harvest_functions <- runoff_df$functions[runoff_df$water_storage]

  #buffer of functions with storage using harvest_dist
  buffer_harv <- x %>%
    dplyr::filter(Function %in% harvest_functions) %>%
    dplyr::select(id, floors_, area) %>%
    sf::st_buffer(harvest_dist)

  #join rooftops as harvesting surfaces with buffer_harv
  rooftops <- x %>%
    dplyr::filter(floors_ > 0,
                  !(Function %in% harvest_functions))

  rooftops <- suppressWarnings(sf::st_join(rooftops, buffer_harv, largest = T)) %>%
    dplyr::select(id.x, id.y, floors_.x, area.x) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!is.na(id.y))

  buffer_harv <- sf::st_drop_geometry(buffer_harv)

  storage_func <- function(u){
    collect <- rainfall * sum(rooftops$area.x[rooftops$id.y == u[[1]] & rooftops$floors_.x > u[[2]]], na.rm = T)
    tank <-  as.numeric(u[[3]]) * runif(1, tank_size[1], tank_size[2])
    return(min(collect,tank))
  }

  rainharvest <- sum(apply(buffer_harv, 1, "storage_func"))


  runoff <- (rainfall - (rainharvest + infiltration)) / rainfall


  return(c(runoff = 1-runoff,
           rainfall = rainfall/1000,
           rainharvest = rainharvest/1000,
           infiltration = infiltration/1000))

}

