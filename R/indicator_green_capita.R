#' @title Urban green per capita
#' @description This indicators calculates the amount of green per capita in the city. This may include private green
#' such as gardens and crops or exclude them.
#' @param x An 'sf' object with the urban model of your city and a 'Function' field with categories of urban features.
#' @param green_categories The categories that are considered as urban green. If NULL, categories of 'get_categories()'
#' are considered.
#' @param inhabitants A value representing the inhabitants in the city.
#' @param neighbourboods (optional) An 'sf' object with polygons representing the neighbourhoods in the city.
#' @param inh_field (optional) The field in 'neighbourhoods' indicating the inhabitants in each neighbourhood.
#' @param name_field (optional) The field in 'neighbourhoods' indicating the name of each neighbourhood
#' @param private If FALSE (default), only public areas are considered in the indicator.
#' @param verbose If FALSE (default), the indicator returns the proportion between the most and the least green neighbourhoods.
#' Otherwise, it will return a tibble with the green per capita in each neigbourhood
#' @details If neighbourhoods are provided the 'inh_field' must also be provided. Then, 'inhabitants' is ignored.
#' @return A numeric value expressing the square meters of green per capita.
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @export
#'
### FALTA EL CAS EN QUÈ JA HI HAGI EL CAMP NEIGHBOURHOOD PER CADA FEATURE.

green_capita <- function(
                        x,
                        green_categories = NULL,
                        inhabitants = NULL,
                        neighbourhoods = NULL,
                        inh_field = NULL,
                        name_field = NULL,
                        private = FALSE,
                        verbose = FALSE
                        ){
  if (is.null(green_categories)){
    if (private){
      green_categories <- unlist(ediblecity::get_categories(), use.names = F)
    } else {
      green_categories <- c(
        ediblecity::get_categories()$edible_green$on_ground[2],
        ediblecity::get_categories()$edible_green$rooftop[2],
        ediblecity::get_categories()$green$public
        )
    }
  }

  green_areas <- x %>%
    filter(Function %in% green_categories)

  if(!is.null(neighbourhoods) && !is.null(inh_field)){

    if (sf::st_crs(green_areas) != sf::st_crs(neighbourhoods)){
      neighbourhoods <- sf::st_transform(neighbourhoods, sf::st_crs(green_areas))
    }

    green_areas <- sf::st_join(green_areas, neighbourhoods) %>%
      dplyr::filter(!is.na(NOMBARRI)) %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(Function,NOMBARRI, POBLACI, area) %>%
      dplyr::group_by(NOMBARRI) %>%
      dplyr::summarise(area = sum(as.numeric(area)),
                       inh = sum(POBLACI)) %>%
      dplyr::mutate(green_capita = area/inh)

    if(verbose){
      result <- green_areas

    } else {

      result <- max(green_areas$green_capita) / min(green_areas$green_capita)
    }

  } else {

    green_areas$area <- sf::st_area(green_areas)
    result <- sum(as.numeric(green_areas$area))/inhabitants

  }

  return(result)
}
