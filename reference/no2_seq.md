# Sequestration of nitrogen dioxide

This indicator returns the amount of NO2 that is sequestered by urban
green.

## Usage

``` r
no2_seq(x, green_df = NULL)
```

## Arguments

- x:

  An 'sf' object with the urban model of your city and a 'land_use'
  column with categories of urban features.

- green_df:

  A dataframe of categories that are considered as urban green with four
  columns:

  1.  'land_uses': Column with the function to be considered in the
      calculations corresponding to 'land_use' column in 'x'.

  2.  'no2_seq1': The low range of NO2 sequestration of each function
      (in ug/s/m2).

  3.  'no2_seq2': The high range of NO2 sequestration of each function
      (in ug/s/m2).

  4.  'pGreen': The proportion of green surface in each function (0:1).
      This is overriden by 'edible_are' when land_uses are community
      garden, commercial garden, rooftop garden and hydroponic rooftop.

  If NULL, the 'city_land_uses' dataset is used.

## Value

A numeric value with the total NO2 sequestration in the city (in
grams/second).

## Author

Josep Pueyo-Ros

## Examples

``` r
# Get the total nitrogen dioxide sequestered by urban green
no2_seq(city_example)
#>     gr/s 
#> 102.8228 
```
