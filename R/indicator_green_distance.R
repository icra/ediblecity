#' @title Distance to closest public green area
#' @description Thisi indicator calculates the distance from each residence to its closest public green area
#' larger than 'min_area'. It can return the summary of distances or the percentage of residence buildings further
#' than a defined distance.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of urban features.
#' @param green_cat A vector with the categories in 'Function' that must be considered in the calculations. If
#' NULL (default), the 'city_functions' dataset is used where 'public' is TRUE.
#' @param min_area A numerical value (in meters). smaller green areas are not considered in the calculations.
#' @param residence_col The column 'x' where the residences are specified.
#' @param residences A vector with the categories that represent residences.
#' @param percent_out If TRUE, the function returns the percentage of residences further than 'max_dist'.
#' (default = FALSE)
#' @param max_dist A numeric value representing the maximum distance that a residence should be from a
#' public green area.
#' @param If TRUE returns the vector of distances. Otherwise, it returns as specified in value section.
#' @return If 'percent_out' is FALSE, it returns a summary of statistics for distance. Otherwise, it
#' returns a numeric value with the percentage of residences further than 'max_dist' from its closest
#' public green area.
#' @export

green_distance <- function(x,
                           green_cat = NULL,
                           min_area = 5000,
                           residence_col = 'Function_verbose',
                           residences = 'Residence',
                           percent_out = FALSE,
                           max_dist = 300,
                           verbose = F
                          ){

  #get categories
  if (is.null(green_cat)){
    green_cat <- city_functions$functions[city_functions$public]
  }

  # get green areas layer
  green_areas <- x %>%
    dplyr::filter(Function %in% green_cat) %>%
    dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
    dplyr::filter(area > min_area)

  houses <- x %>%
    dplyr::filter(!!as.symbol(residence_col) %in% residences)

  nearest <- sf::st_nearest_feature(houses,green_areas)
  distance <- sf::st_distance(houses, green_areas[nearest,], by_element = T)

  if (verbose) return(distance)

  if (percent_out){
    return(sum(as.numeric(distance) > max_dist) / length(distance))
  } else {
    return(summary(distance))
  }

}
