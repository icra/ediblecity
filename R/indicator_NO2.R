#' @title Sequestration of nitrogen dioxide
#' @description This indicator returns the amount of NO2 that is sequestered by urban green.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of
#' urban features.
#' @param green_df A dataframe of categories that are considered as urban green with four columns:
#' \enumerate{
#'   \item 'functions': Column with the function to be considered in the calculations corresponding
#'   to 'Function' column in 'x'.
#'   \item 'no2_seq1': The low range of NO2 sequestration of each function (in ug/s/m2).
#'   \item 'no2_seq2': The high range of NO2 sequestration of each function (in ug/s/m2).
#'   \item 'pGreen': The proportion of green surface in each function (0:1).
#' }
#' @return A numeric value with the total NO2 sequestration in the city (in grams/second).
#' @export
#'


no2_seq <- function(x,
                    green_df = NULL
                    ){


  if(is.null(green_df)){
    green_df <- city_functions %>%
      mutate(pGreen = ifelse(!is.na(pGreen),
                             pGreen,
                             ifelse(location == "rooftop",
                                    0.61,
                                    1))) %>%
      select(functions, no2_seq1, no2_seq2, pGreen)
  }

  x <- x %>% filter(Function %in% green_df$functions)


  x <- left_join(x,green_df, by=c("Function" = "functions"))
  x$pGreen[is.na(x$pGreen)] <- 0

  x$green_area <- as.numeric(sf::st_area(x)) * x$pGreen
  x$no2_seq <- 0

  for (i in 1:nrow(green_df)){
    f <- green_df$functions[i]

    if (green_df$no2_seq1[i] != green_df$no2_seq2[i]){
      x$no2_seq[x$Function == f] <-  x$green_area[x$Function == f] * runif(
                                                                        length(x$green_area[x$Function == f]),
                                                                        green_df$no2_seq1[i],
                                                                        green_df$no2_seq2[i])

      } else {
      x$no2_seq[x$Function == f] <-  x$green_area[x$Function == f]
    }
  }


 return(c("grams/second" = sum(x$no2_seq)/1000))



}
