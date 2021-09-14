#' @title Categories of urban edible and non-edible green
#' @export
#'


get_categories <- function(){
  return(
    list("edible_green" = list(
        "private" = "Edible private garden",
        "on_ground"=c("Commercial garden", "Community garden"),
        "rooftop" = c("Hydroponic rooftop","Rooftop garden")
      ),
      "green" = list(
        "private" = c("Arable land","Normal garden", "Permanent crops", "Vacant"),
        "public" = c("Grass", "Mulcher", "Raised bed","Trees", "Vegetated pergola")
      )
    )
  )
}

