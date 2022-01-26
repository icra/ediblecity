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

#CN https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/cn-tables

runoff_prev <- function(
                        x,
                        runoff_df = NULL,
                        rain = 85,
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
        contains("CN")
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

  #join runoff_df with x

  x <- x %>%
    dplyr::left_join(runoff_df, by = c("Function" = "functions")) %>%
    dplyr::mutate(
      CN1 = ifelse(is.na(CN1), 98, CN1),
      CN2 = ifelse(is.na(CN2), 98, CN2)
    )

  areaCN <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      CN = runif(1, CN1, CN2),
      areaCN = case_when(
        Function == "Rooftop garden" ~ CN * area * 0.60,
        TRUE ~ CN * area)
        ) %>%
    dplyr::pull(areaCN)

  weightedCN <- sum(areaCN)/sum(x$area)

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

  #total rain harvested in litres
  total_RH <- sum(apply(buffer_harv, 1, "storage_func"))

  #rain harvested in mm
  RH <- total_RH/sum(x$area)

  #SCS model for runoff
  S <- 25.4*((1000/weightedCN)-10) #mm
  Ia <- (0.2 * S) + RH #mm
  Q <- ((rain-Ia)^2)/(rain-Ia+S) #mm


  return(
    c(
      runoff = Q, #mm
      rainfall = rainfall/1000, #cubic metres
      rainharvest = total_RH/1000 #cubic metres
      )
    )

}

