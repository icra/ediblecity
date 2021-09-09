#' @title The jobs created by urban agriculture in your city
#' @description This indicator estimates the number of full-time jobs created by urban agriculture
#' initiatives in your city. It uses a range of jobs per square meter to create the median and the
#' confidence interval of the number of jobs by simulating a random uniform distribution of 1000 values
#' within the provided range.
#' @param x An 'sf' object with the urban model of your city.


edible_jobs <- function(x,
                        jobs = c(0.000163, 0.022),
                        edible = c("Commercial gardens", "Hydroponic rooftops")){
  area <- x %>%
    filter(Function %in% edible) %>%
    st_area()

    dist <- area * runif(1000, min = jobs[1], max = jobs[2])
    return(c(quantile(dist,0.05), "50%" = median(dist), quantile(dist,0.95)))
}



