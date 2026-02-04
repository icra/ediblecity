# The number of volunteers involved in urban agriculture in your city

This indicator estimates the number of volunteers potentially involved
in community urban agriculture initiatives in your city. It uses a range
of volunteers per square meter to create the median and the confidence
interval of the number of volunteers by simulating a random uniform
distribution of 1000 values within the provided range. The default range
came from required work hours in urban agriculture assessed in
scientific literature, assuming that a volunteers dedicates a 10

## Usage

``` r
edible_volunteers(
  x,
  volunteers = c(0.00163, 0.22),
  edible = NULL,
  area_col = "edible_area",
  interval = 0.95,
  verbose = FALSE
)
```

## Arguments

- x:

  An 'sf' object with the urban model of your city and a 'land_use'
  column with categories of urban features.

- volunteers:

  A vector of length 2 with the range of volunteers involved by square
  meter of edible gardens.

- edible:

  The categories in 'land_uses' that represent community edible gardens.
  If NULL, land_uses from 'city_land_uses' dataset area used where
  volunteers is TRUE.

- area_col:

  The column to be used as the area of each feature. If NULL, the area
  is calculated with sf::st_area().

- interval:

  A numeric value with the confidence interval returned by the function.

- verbose:

  If TRUE, the indicators returns a vector (N=1000) with all simulated
  values.

## Value

If verbose is FALSE, it returns a named vector with the median and the
low and high confidence intervals. Otherwise, it returns a vector of
length 1000 with all simulated values.

## Author

Josep Pueyo-Ros

## Examples

``` r
# Get the 95% confidence interval
edible_volunteers(city_example, interval = 0.95)
#>        5%       50%       95% 
#>  2.315715 18.203390 33.332883 

# Get the raw values from the Monte Carlo simulation
# and adjust the number of volunteers by squared meter.
result <- edible_volunteers(city_example, volunteers = c(0.1, 0.2), verbose = TRUE)
result[1:10]
#>  [1] 27.31503 23.18252 27.42799 21.72076 18.72121 23.52306 20.16345 18.18117
#>  [9] 26.12200 29.33050
```
