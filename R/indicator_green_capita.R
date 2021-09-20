#' @title Urban green per capita
#' @description This indicators calculates the amount of green per capita in the city. This may include private green
#' such as gardens and crops or exclude them.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of urban features.
#' @param green_categories The categories that are considered as urban green. If NULL, categories of 'get_categories()'
#' are considered.
#' @param inhabitants A value representing the inhabitants in the city.
#' @param neighbourboods (optional) An 'sf' object with polygons representing the neighbourhoods in the city.
#' @param inh_col (optional) The col in 'x' or in 'neighbourhoods' indicating the inhabitants in each neighbourhood.
#' @param name_col (optional) The col in 'x' or in 'neighbourhoods' indicating the name of each neighbourhood
#' @param private If FALSE (default), only public areas are considered in the indicator.
#' @param verbose If FALSE (default), the indicator returns the proportion between the most and the least green neighbourhoods.
#' Otherwise, it will return a tibble with the green per capita in each neigbourhood.
#' @param min_inh If neighbourhoods are used, those with less inhabitants than 'min-inh' will be discarded.
#' @details If 'inh_col' and 'name_col' are defined and  'neighbourhoods' is NULL, the function searches
#' the columns in 'x'. If 'neighbourhoods' is defined along with previous both, the columns are searched in
#' 'neighbourhoods' and spatially joined with 'x'. In both cases, 'inhabitants' is ignored.
#' @return A numeric value expressing the square meters of green per capita. Or a numeric value expressing
#' the proportion between the greenest and the least green neighbourhood. Or a tibble with the green area,
#' inhabitants and green per capita in each neighbourhood.
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import sf
#' @export
#'


green_capita <- function(
                        x,
                        green_categories = NULL,
                        inhabitants = NULL,
                        neighbourhoods = NULL,
                        inh_col = NULL,
                        name_col = NULL,
                        private = FALSE,
                        verbose = FALSE,
                        min_inh = 0
                        ){
  if (is.null(green_categories)){
    if (private){
      green_categories <- city_functions$functions
    } else {
      green_categories <- city_functions$functions[city_functions$public]
    }
  }

  green_areas <- x %>%
    dplyr::filter(Function %in% green_categories)

  if (sum(green_areas$Function %in% city_functions$functions) == nrow(green_areas)){
    green_areas <- dplyr::left_join(green_areas, city_functions, by = c("Function" = "functions"))
  } else {
    green_areas$pGreen <- NA
    green_areas$location <- NA
  }



  if(!is.null(inh_col) && !is.null(name_col)){

    if(!is.null(neighbourhoods)){
      if (sf::st_crs(green_areas) != sf::st_crs(neighbourhoods)){
        neighbourhoods <- sf::st_transform(neighbourhoods, sf::st_crs(green_areas))
      }

      green_areas <- suppressWarnings(sf::st_intersection(green_areas, neighbourhoods))

    }

     green_areas <- green_areas %>%
      dplyr::filter(!is.na(!!as.symbol(name_col)),
                    !!as.symbol(inh_col) > min_inh) %>%
      dplyr::mutate(area = ifelse(!is.na(pGreen),
                           sf::st_area(.) * pGreen,
                           ifelse(location == "rooftop",
                                  sf::st_area(.) * 0.61,
                                  sf::st_area(.)))) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(Function,!!as.symbol(name_col), !!as.symbol(inh_col), area) %>%
      dplyr::group_by(!!as.symbol(name_col)) %>%
      dplyr::summarise(area = sum(as.numeric(area))) %>%
      inner_join(neighbourhoods, by=name_col) %>%
      select(!!as.symbol(name_col), area, !!as.symbol(inh_col)) %>%
      dplyr::mutate(green_capita = area/!!as.symbol(inh_col))

    if(verbose){
      result <- green_areas

    } else {

      result <- max(green_areas$green_capita) / min(green_areas$green_capita)
    }

  } else {

    area <-  green_areas %>%
      dplyr::mutate(area = ifelse(!is.na(pGreen),
                          (sf::st_area(.) * pGreen),
                          (ifelse(location == "rooftop",
                                 sf::st_area(.) * 0.61,
                                 sf::st_area(.))))) %>%
      .$area

    result <- sum(as.numeric(area))/inhabitants

  }

  return(result)
}
