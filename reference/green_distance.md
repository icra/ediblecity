# Distance to closest public green area

This indicator calculates the distance from each residence to its
closest public green area larger than 'min_area'. It can return the
summary of distances or the percentage of residence buildings further
than a defined distance.

## Usage

``` r
green_distance(
  x,
  green_cat = NULL,
  min_area = 5000,
  residence_col = "land_use_verbose",
  residences = "Residence",
  percent_out = FALSE,
  max_dist = 300,
  verbose = FALSE
)
```

## Arguments

- x:

  An 'sf' object with the urban model of your city and a 'land_use'
  column with categories of urban features.

- green_cat:

  A vector with the categories in 'land_use' that must be considered in
  the calculations. If NULL (default), the 'city_land_uses' dataset is
  used where 'public' is TRUE.

- min_area:

  A numerical value (in meters). smaller green areas are not considered
  in the calculations.

- residence_col:

  The column 'x' where the residences are specified.

- residences:

  A vector with the categories that represent residences.

- percent_out:

  If TRUE, the function returns the percentage of residences further
  than 'max_dist'. (default = FALSE)

- max_dist:

  A numeric value representing the maximum distance that a residence
  should be from a public green area.

- verbose:

  If TRUE returns the vector of distances. Otherwise, it returns as
  specified in value section.

## Value

If 'percent_out' is FALSE, it returns a summary of statistics for
distance. Otherwise, it returns a numeric value with the percentage of
residences further than 'max_dist' from its closest public green area.

## Author

Josep Pueyo-Ros

## Examples

``` r
# Calculate a summary of the distances to closest public green area larger than 0.5 ha.
green_distance(city_example, min_area = 5000)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   6.119 155.084 251.702 249.720 347.417 465.732 

# Get the distances from each residence to its closest public green area.
result <- green_distance(city_example, min_area = 0, verbose = TRUE)
result[1:10]
#> Units: [m]
#>  [1] 70.379516  6.119484 82.848678 18.524184 46.426160 18.767635 59.879243
#>  [8] 54.543559 12.020277 11.959444

# Get the percentage of residences further than 300 m. from a green area larger than 0.5 ha.
green_distance(city_example, percent_out = TRUE, max_dist = 300)
#> [1] 36.92615
```
