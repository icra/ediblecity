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
#' @return


runoff_prev <- function(
                        x,
                        runoff_df = NULL,
                        rain = 100,
                        floors_field = 'floors',
                        harvest_dist = 10,
                        tank_size = c(0,100),
                        calc_storage = T
                        ){

  #if no runoff_df, use the city_functions values
  if(is.null(runoff_df)){
    runoff_df <- city_functions %>%
      dplyr::select(
        functions,
        contains("water_storage"),
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

  infiltration <- 0

  for (i in 1:nrow(runoff_df)){
    func <- x %>%
      dplyr::filter(Function == runoff_df$functions[i])

    if (sum(func$edible_area) > 0){
      func <- func %>%
        dplyr::mutate(edible_inf = edible_area * runif(1, runoff_df$infiltration1, runoff_df$infiltration2),
             normal_inf = ifelse(floors_ > 0, 0, (area - edible_area) * runif(1, vacant_inf[1], vacant_inf[2])
             ))
    } else {
      func <- func %>%
        dplyr::mutate(edible_inf = 0,
               normal_inf = area * runif(1, runoff_df$infiltration1[i], runoff_df$infiltration2[i])
               )
    }

    func_infilt <- min(sum(func$area) * rain, sum(func$edible_inf) + sum(func$normal_inf))


    infiltration <- infiltration + func_infilt

  }

  ##calculate rainwater harvesting area and tank size

  if (calc_storage){
    harvest_functions <- runoff_df$functions[runoff_df$water_storage]

    harvest_x <- x %>%
      dplyr::filter(Function %in% harvest_functions)

    #buffer of BOI using harvest_dist
    buffer_harv <- sf::st_buffer(harvest_x, harvest_dist)

    rooftops <- x %>%
      dplyr::filter(floors_ > 0,
                    !(Function %in% harvest_functions))

    storage <- 0

    tanks <- buffer_harv$area * runif(nrow(buffer_harv), tank_size[1], tank_size[2])

    for (i in 1:nrow(buffer_harv)){

      #calculate rainwater harvesting area
      harvest_collect <- rooftops %>%
        dplyr::filter(sf::st_intersects(.,buffer_harv[i,], sparse = F)) %>%
        dplyr::filter(floors_ > buffer_harv$floors_[i]) %>%
        with(., sum(area))*rain

      storage <- storage + min(harvest_collect, tanks[i])

      #ÉS MOLT LENT, PENSAR COM FER-HO PER TOTS A LA VEGADA
    }

  } else {
    storage <- 0
  }




}

