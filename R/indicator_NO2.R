#' @title Sequestration of nitrogen dioxide
#' @description This indicator returns the amount of NO2 that is sequestered by urban green.
#' @param x An 'sf' object with the urban model of your city and a 'Function' column with categories of
#' urban features.
#' @param green_df A dataframe of categories that are considered as urban green with two columns. 'functions'
#' with the names of 'Function' in 'x' to be considered as green and a 'no2_seq' columns with NO2
#' sequestration capacity of each function (in ug/s/m2). If NULL, categories and values of 'city_functions' dataset are
#' considered.
#' @return If 'return_raster is TRUE, a 'stars' object is returned with NO2 sequestration as attribute. If 'verbose'
#' is TRUE, a vector with the values of NO2 sequestration. If previous both are FALSE, the summary of values is returned.
#' Units are ug/s/m2.
#' @export
#'

no2_seq <- function(x,
                    green_df = NULL,
                    traffic_lanes = 'Streets',
                    max_dist = Inf,
                    verbose = F
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

  x <- x %>% filter(Function %in% green_df)


  x <- left_join(x,green_df, by=c("Function" = "functions"))
  x$pGreen[is.na(x$pGreen)] <- 0

  x$area <- sf::st_area(x)
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


### OLD


  sequest <- stars::st_rasterize(x['no2_seq'], dx=5, dy=5)

  if (return_raster) return(sequest)
  if (verbose) return (sequest %>% as_tibble() %>% filter(!is.na(no2_seq)) %>% .$no2_seq)

  vector <- sequest %>% as_tibble() %>% filter(!is.na(no2_seq)) %>% .$no2_seq
  summary_new <- c(summary(vector), "Sum" = sum(vector))

  return(summary_new)
    c(summary(as_tibble(sequest) %>% filter(!is.na(no2_seq)) %>%  .$no2_seq))


}

x %>% select(Function, green_area, no2_seq) %>% filter(Function == "Arable land") %>% mutate(area = sf::st_area(.))
