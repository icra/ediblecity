check_sf <- function(x){
  #check if x is a sf object
  if (all(class(x) != "sf")) rlang::abort(tr_("x must be an `sf` object"))
}
