#' @title The jobs created by urban agriculture in your city
#' @description This indicator estimates the number of full-time jobs created by commercial urban agriculture
#' initiatives in your city. It uses a range of jobs per square meter to create the median and the
#' confidence interval of the number of jobs by simulating a random uniform distribution of 1000 values
#' within the provided range.
#' @param x An 'sf' object with the urban model of your city.
#' @param edible The categories in 'Functions' that represent commercial edible gardens.
#' @param area_field The field to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()


edible_jobs <- function(x,
                        jobs = c(0.000163, 0.022),
                        edible = c("Commercial garden", "Hydroponic rooftop"),
                        area_field = 'flat_area'){

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
    dist <- area * runif(1000, min = jobs[1], max = jobs[2])

    #return 5%, 50% and 95% of dist
    return(c(quantile(dist,0.05), "50%" = median(dist), quantile(dist,0.95)))
}




