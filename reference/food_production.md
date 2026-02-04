# The food produced by urban agriculture in your city

This indicator estimates the food (in kg/year) produced by urban
agriculture initiatives in your city. It uses a range of production for
each type of initiative to create the median and the confidence interval
of the number of jobs by simulating a random uniform distribution of
1000 values within the provided range.

## Usage

``` r
food_production(
  x,
  edible_df = NULL,
  area_col = "edible_area",
  interval = 0.95,
  verbose = FALSE
)
```

## Arguments

- x:

  An 'sf' object with the urban model of your city and a 'land_use'
  column with categories of urban features.

- edible_df:

  A dataframe of categories that are considered as urban agriculture
  with three columns:

  1.  'land_uses': Column with the land_use to be considered in the
      calculations corresponding to 'land_use' column in 'x'.

  2.  'food1': The low range of food production in each land_use (in
      kg/year/m2).

  3.  'food2': The high range of food production of each land_use (in
      kg/year/m2).

- area_col:

  The column to be used as the area of each feature. If NULL, the area
  is calculated with sf::st_area()

- interval:

  A numeric value with the confidence interval returned by the land_use.

- verbose:

  If TRUE, the indicators returns a vector (N=1000) with all simulated
  values.

## Value

If verbose is FALSE, it returns a named vector with the median and the
low and high confidence intervals (in kg/year). Otherwise, it returns a
vector of length 1000 with all simulated values (in kg/year)

## Author

Josep Pueyo-Ros

## Examples

``` r
# Estimate the food production within 95% confidence interval
food_production(city_example, interval = 0.95, verbose = FALSE)
#>        5%       50%       95% 
#>  50.12305 189.59520 336.36133 

# Get the raw values instead of the confidence interval
result <- food_production(city_example, verbose = TRUE)
result[1:10]
#>  [1] 122.36681 350.48885  53.22304 243.99863  52.00791 156.50796 186.10658
#>  [8] 258.26437 219.80236 290.35711
```
