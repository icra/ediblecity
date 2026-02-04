# The jobs created by urban agriculture in your city

This indicator estimates the number of full-time jobs created by
commercial urban agriculture initiatives in your city. It uses a range
of jobs per square meter to create the median and the confidence
interval of the number of jobs by simulating a random uniform
distribution of 1000 values within the provided range.

## Usage

``` r
edible_jobs(
  x,
  jobs = c(0.000163, 0.022),
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

- jobs:

  A vector of length 2 with the range of jobs created by square meter of
  edible gardens.

- edible:

  The categories in 'land_uses' that represent commercial edible
  gardens. If NULL, the land_uses from 'city_land_uses' dataset are used
  where jobs is TRUE.

- area_col:

  The column to be used as the area of each feature. If NULL, the area
  is calculated with sf::st_area()

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
# First, we set a scenario with commercial gardens that create jobs
scenario <- set_scenario(city_example, pCommercial = 1, quiet = TRUE)
# Get the 95% confidence interval
edible_jobs(scenario, interval = 0.95)
#>        5%       50%       95% 
#>  170.7603 1433.8264 2761.1128 

# Get the raw values from the Monte Carlo simulation and adjust the number of jobs by square meter.
result <- edible_jobs(scenario, jobs = c(0.02, 0.03), verbose = TRUE)
result[1:10]
#>  [1] 3534.984 3459.587 3955.618 2898.176 3004.829 3369.756 2851.365 3764.384
#>  [9] 3085.142 3136.763
```
