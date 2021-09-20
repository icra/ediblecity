#' @title The jobs created by urban agriculture in your city
#' @description This indicator estimates the number of full-time jobs created by commercial urban agriculture
#' initiatives in your city. It uses a range of jobs per square meter to create the median and the
#' confidence interval of the number of jobs by simulating a random uniform distribution of 1000 values
#' within the provided range.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of urban features.
#' @param jobs A vector of length 2 with the range of jobs created by square meter of edible gardens.
#' @param edible The categories in 'Functions' that represent commercial edible gardens. If NULL, the
#' functions from 'city_functions' dataset are used where jobs is TRUE.
#' @param area_col The column to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()
#' @param interval A numeric value with the confidence interval returned by the function.
#' @param verbose If TRUE, the indicators returns a vector (N=1000) with all simulated values.
#' @return It returns a named vector with the median and the low and high confidence intervals
#' @export


edible_jobs <- function(x,
                        jobs = c(0.000163, 0.022),
                        edible = NULL,
                        area_col = 'flat_area',
                        interval = 0.95,
                        verbose = F){

    #get categories
    if (is.null(edible)){
      edible <- city_functions$functions[city_functions$jobs]
    }

    #filter x based on edible
    filtered <- x %>%
      dplyr::filter(Function %in% edible)

    #calculates area of filtered based on st_area() or in area_col
    area <- ifelse(is.null(area_col),
                    sum(sf::st_area(filtered)),
                    as.data.frame(filtered) %>%
                      dplyr::select(matches(area_col)) %>%
                      sum(.[area_col]))

    #use the jobs range to create a random uniform distribution
    dist <- area * runif(1000, min = jobs[1], max = jobs[2])

    #return median and confidence interval of dist
    return(c(quantile(dist,1-interval), "50%" = median(dist), quantile(dist,interval)))
}




