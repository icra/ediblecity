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
#' If NULL, the 'city_functions' dataset is used.
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

  x_f <- x %>% filter(Function %in% green_df$functions)


  x_f <- left_join(x_f,green_df, by=c("Function" = "functions"))
  x_f$pGreen[is.na(x_f$pGreen)] <- 0

  x_f$green_area <- as.numeric(sf::st_area(x_f)) * x_f$pGreen

  x_f$no2_seq <- 0

  for (i in 1:nrow(green_df)){
    f <- green_df$functions[i]

    if (green_df$no2_seq1[i] != green_df$no2_seq2[i]){
      x_f$no2_seq[x_f$Function == f] <-  x_f$green_area[x_f$Function == f] * runif(
                                                                        length(x_f$green_area[x_f$Function == f]),
                                                                        green_df$no2_seq1[i],
                                                                        green_df$no2_seq2[i])

      } else {
      x_f$no2_seq[x_f$Function == f] <-  x_f$green_area[x_f$Function == f]
    }
  }


 return(c("gr/s" = sum(x_f$no2_seq)/1000))



}
