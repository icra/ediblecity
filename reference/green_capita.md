# Urban green per capita

This indicators calculates the amount of green per capita in the city.
This may include private green such as gardens and crops or exclude
them.

## Usage

``` r
green_capita(
  x,
  green_categories = NULL,
  inhabitants = NULL,
  neighbourhoods = NULL,
  inh_col = NULL,
  name_col = NULL,
  private = FALSE,
  verbose = FALSE,
  min_inh = 0
)
```

## Arguments

- x:

  An 'sf' object with the urban model of your city and a 'land_use'
  column with categories of urban features.

- green_categories:

  The categories that are considered as urban green. If NULL, categories
  of 'city_land_uses' are considered.

- inhabitants:

  A value representing the inhabitants in the city.

- neighbourhoods:

  (optional) An 'sf' object with polygons representing the neighborhoods
  in the city.

- inh_col:

  (optional) The col in 'x' or in 'neighborhoods' indicating the
  inhabitants in each neighborhood.

- name_col:

  (optional) The col in 'x' or in 'neighborhoods' indicating the name of
  each neighborhood

- private:

  If FALSE (default), only public areas are considered in the indicator.
  If TRUE, elements in 'city_land_uses' where 'private' is TRUE are
  considered. Alternatively, a vector with land_uses to be considered.#'

- verbose:

  If FALSE (default), the indicator returns the proportion between the
  most and the least green neighbourhoods. Otherwise, it will return a
  tibble with the green per capita in each neighborhood, provided that
  'inh_col' and 'name_col' are provided.

- min_inh:

  If neighbourhoods are used, those with less inhabitants than 'min-inh'
  will be discarded.

## Value

A numeric value expressing the square meters of green per capita. Or a
numeric value expressing the proportion between the greenest and the
least green neighbourhood. Or a tibble with the green area, inhabitants
and green per capita in each neighbourhood.

## Details

If 'inh_col' and 'name_col' are defined and 'neighbourhoods' is NULL,
the function searches the columns in 'x'. If 'neighbourhoods' is defined
along with previous both, the columns are searched in 'neighbourhoods'
and spatially joined with 'x'. In both cases, 'inhabitants' is ignored.

## Author

Josep Pueyo-Ros

## Examples

``` r
# Calculate total green per capita in the city
green_capita(city_example, inhabitants = 6000)
#> [1] 5.113167

# Calculate the differences between the greenest and the least green neighbourhoods
green_capita(city_example, neighbourhoods = neighbourhoods_example,
             inh_col = "inhabitants", name_col = "name")
#> [1] 6.54263

# Get the green per capita in each neighbourhood
green_capita(city_example, neighbourhoods = neighbourhoods_example,
             inh_col = "inhabitants", name_col = "name", verbose = TRUE)
#> # A tibble: 2 × 4
#>   name              area inhabitants green_capita
#>   <chr>            <dbl>       <dbl>        <dbl>
#> 1 Sant Narcís nord 20016        1028        19.5 
#> 2 Sant Narcís sud  15743        5290         2.98

# Use a customized vector of land_uses to be considered private green
green_capita(city_example, inhabitants = 6000, private = c("Normal garden", "Commercial garden"))
#> [1] 10.67717
```
