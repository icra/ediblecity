#' @title Set the scenario for your edible city
#' @description You can adjust different parameters to define different city scenarios.
#' The object must contain a field 'Function' which describes the function or type of each feature.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'Function' field with categories of urban features.
#' @param pGardens The proportion of private gardens (Function == 'Gardens')
#' that will become edible gardens [0-1].
#' @param pVacant The proportion of vacant plot (Function == 'Vacant') with 'area >= min_area_vacant'
#' that will become edible gardens [0-1].
#' @param pRooftop The proportion of rooftops (Function == 'Flat rooftop') with 'area >= min_area_rooftop'
#' that will become edible rooftops [0-1].
#' @param perc_garden The proportion in a range of surface in a garden that is occupied by edible plants [0-1].
#' @param perc_vacant The proportion in a range of surface in a vacant plot that is occupied by edible plants [0-1].
#' @param perc_rooftop The proportion in a range of surface in a rooftop that is occupied by edible plants [0-1].
#' @param min_area_garden The minimum area that a garden must have to become an edible garden.
#' @param min_area_vacant The minimum area that a vacant must have to become an community or commercial garden.
#' @param min_area_rooftop The minimum area that a flat rooftop must have to become an edible rooftop.
#' @param private_gardens_from The categories in 'Functions' potentially converted to edible private gardens
#' @param vacant_from The categories in 'Functions' potentially converted to community or commercial gardens
#' @param rooftop_from The categories in 'Functions' potentially converted to edible rooftop
#' (community raised beds or commercial hydroponic)
#' @param pCommercial The proportion of plots and rooftop that will be commercial. The rest will be community gardens
#' In rooftops it is equivalent to raised beds and hydroponic system respectively.
#' @param area_field The field to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()
#' @return An 'sf' object as 'x' with the respective proportion of gardens ('Edible private garden'),
#' vacant plots ('Community plot garden', 'Commercial plot garden') and rooftop gardens ('Community rooftop garden',
#' 'Commercial hydroponic rooftop')
#' labelled as edible gardens.
#' @details When pGardens is lower than 1, the gardens are selected randomly among gardens with an area larger than
#' 'min_area_garden'. However, vacant plots and rooftops are selected from larger to smaller, assuming that the best
#' spots (i.e. larger) are occupied first. Likewise, when pCommercial > 0, commercial gardens and hydroponic rooftops are
#' settled in the larger features, assuming that commercial initiatives have the power to acquire the best spots.
#' @export

set_scenario <- function(x,
                         pGardens = 1,
                         pVacant = 1,
                         pRooftop = 1,
                         perc_garden = c(0.02, 0.3),
                         perc_vacant = c(0.52, 0.75),
                         perc_rooftop = c(0.6,0.62),
                         min_area_garden = 10,
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
    x$area <- as.numeric(sf::st_area(x))
  } else {
    x$area <- x[[area_field]]
  }

  #create edible area field if it does not exist
  if (!("edible_area" %in% colnames(x))) x$edible_area <- 0

  #CONVERT PRIVATE GARDENS TO EDIBLE GARDENS
  if (pGardens > 0){
    gardens_index <- which(x$Function %in% private_gardens_from & x$area >= min_area_garden)
    nGardens <- sum(x$Function %in% private_gardens_from)

    if (pGardens == 1 || nGardens*pGardens >= length(gardens_index)){

      x$Function[gardens_index] <- city_functions$functions[city_functions$location == "garden"]

      if (nGardens*pGardens >= length(gardens_index)){
        warning(paste("Only", length(gardens_index), "private gardens out of", nGardens*pGardens, "assumed satisfy the 'min_area_garden'\n"))
      }

    } else if (pGardens < 1){
      nGardens <- nGardens*pGardens
      gardens_index <- sample(gardens_index, nGardens)
      x$Function[gardens_index] <- city_functions$functions[city_functions$location == "garden"]

    }

    x$edible_area[gardens_index] <- x$area[gardens_index]*runif(length(gardens_index), perc_garden[1], perc_garden[2])
  }


  #CONVERT VACANT PLOTS TO COMMUNITY OR COMMERCIAL GARDENS
  if (pVacant > 0){

    #locate and count vacant plots
    vacant_index <- which(x$Function %in% vacant_from & x$area >= min_area_vacant)
    nVacant <- sum(x$Function %in% vacant_from)

    #set transformed vacants, commercial and community proportions to zero
    total_vacant <- 0
    commercial <- 0

    #define categories in a vector
    commercial_garden <- city_functions$functions[city_functions$jobs & city_functions$location == "vacant"]
    community_garden <- city_functions$functions[city_functions$volunteers & city_functions$location == "vacant"]


    if (length(vacant_index) < nVacant*pVacant){
      warning(paste("Only", length(vacant_index), "vacant plots out of", nVacant*pVacant, "assumed satisfy the 'min_area_vacant'\n"))
      nVacant <- length(vacant_index)

    } else {
      nVacant <- round(nVacant*pVacant,0)
    }

    while (total_vacant < nVacant && commercial < pCommercial){

      larger <- which(x$area == max(x$area[vacant_index]) & x$Function %in% vacant_from)
      if (length(larger) > 1) larger <- larger[1]
      vacant_index <- vacant_index[vacant_index != larger]

      # if (commercial < pCommercial){

      x$Function[larger] <- commercial_garden
      commercial <- sum(x$Function == commercial_garden)/nVacant

      # } else {
      #
      #   x$Function[larger] <- community_garden
      # }

      total_vacant <- total_vacant + 1
    }

    comm_index <- sample(vacant_index, nVacant - total_vacant)
    x$Function[comm_index] <- community_garden

    new_index <- x$Function %in% c(commercial_garden, community_garden)
    x$edible_area[new_index] <-
      x$area[new_index]*runif(sum(new_index), perc_vacant[1], perc_vacant[2])

  }

  #CONVERT ROOFTOPS TO ROOFTOP GARDENS OR HYDROPONIC ROOFTOPS
  if (pRooftop > 0){

    #locate and count vacant plots
    rooftop_index <- which(x$Function %in% rooftop_from & x$area >= min_area_rooftop)
    nRooftop <- sum(x$Function %in% rooftop_from)

    #set transformed vacants, commercial and community proportions to zero
    total_rooftop <- 0
    commercial <- 0

    #define categories
    rooftop_garden <- city_functions$functions[city_functions$volunteers & city_functions$location == 'rooftop']
    hydroponic_rooftop <- city_functions$functions[city_functions$jobs & city_functions$location == 'rooftop']

    if (length(rooftop_index) < nRooftop*pRooftop){
      warning(paste("Only", length(rooftop_index), "rooftops out of", nRooftop*pRooftop, "assumed satisfy the 'min_area_rooftop'\n"))
      nRooftop <- length(rooftop_index)

    } else {
      nRooftop <- round(nRooftop*pRooftop, 0)
    }

    while (total_rooftop < nRooftop && commercial < pCommercial){

      larger <- which(x$area == max(x$area[rooftop_index]) & x$Function %in% rooftop_from)
      if (length(larger) > 1) larger <- larger[1]
      rooftop_index <- rooftop_index[rooftop_index != larger]

      #if (commercial < pCommercial){

      x$Function[larger] <- hydroponic_rooftop
      commercial <- sum(x$Function == hydroponic_rooftop)/nRooftop

      # } else {
      #
      #   x$Function[larger] <- rooftop_garden
      # }

      total_rooftop <- total_rooftop + 1
    }

    comm_index <- sample(rooftop_index, nRooftop - total_rooftop)
    x$Function[comm_index] <- rooftop_garden

    new_index <- x$Function %in% c(rooftop_garden, hydroponic_rooftop)
    x$edible_area[new_index] <-
      x$area[new_index]*runif(sum(new_index), perc_rooftop[1], perc_rooftop[2])

  }


  return(x)
}
