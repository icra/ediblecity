#' @title The number of volunteers involved in urban agriculture in your city
#' @description This indicator estimates the number of volunteers potentially involved in community urban agriculture
#' initiatives in your city. It uses a range of volunteers per square meter to create the median and the
#' confidence interval of the number of volunteers by simulating a random uniform distribution of 1000 values
#' within the provided range. The default range came from required work hours in urban agriculture assessed
#' in scientific literature, assuming that a volunteers dedicates a 10% of a full-time job.
#' @param x An 'sf' object with the urban model of your city and a 'Function' field with categories of urban features.
#' @param volunteers A vector of length 2 with the range of volunteers involved by square meter of edible gardens.
#' @param edible The categories in 'Functions' that represent community edible gardens.
#' @param area_field The field to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()
#' @param interval A numeric value with the confidence interval returned by the function.
#' @export


edible_volunteers <- function(x,
                        volunteers = c(0.00163, 0.22),
                        edible = c("Community garden", "Rooftop garden"),
                        area_field = 'flat_area',
                        interval = 0.95){

  #filter x based on edible
  filtered <- x %>%
    dplyr::filter(Function %in% edible)

  #calculates area of filtered based on st_area() or in area_field
  area <- ifelse(is.null(area_field),
                 sum(sf::st_area(filtered)),
                 as.data.frame(filtered) %>%
                   dplyr::select(matches(area_field)) %>%
                   sum(.[area_field]))

  #use the jobs range to create a random uniform distribution
  dist <- area * runif(1000, min = volunteers[1], max = volunteers[2])

  #return median and confidence interval of dist
  return(c(quantile(dist,1-interval), "50%" = median(dist), quantile(dist,interval)))
}
