
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ediblecity

<!-- badges: start -->

[![R-CMD-check](https://github.com/icra/edibleCity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/icra/edibleCity/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ediblecity is to is to estimate the potential of UA to
contribute to addressing several urban challenges at the city-scale.
Within this aim, we followed the urban challenges defined by the Eklipse
project that are followed for nearly all of the European projects
focused on Nature-based Solutions. We selected 8 indicators directly
related to one or several urban challenges.

## Installation

You can install the development version of ediblecity from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("icra/edibleCity")
```

## Indicators estimated

The package provides eight indicators that estimate different benefits
of urban agriculture:

-   `food_production()`: Amount of food produced in the city.
-   `green_capita()`: Green per capita can be computed as raw or as the
    difference among neighbourhoods.
-   `green_distance()`: Distance to closest public green area larger
    than certain surface. It computes also the proportion of homes that
    are further than a specific threshold.
-   `UHI()`: Urban heat island as a rasters (`stars` object) or as
    numeric values.
-   `edible_jobs()`: Number of jobs created by commercial urban
    agriculture.
-   `edible_volunteers()`: Number of volunteers involved in community
    urban agriculture.
-   `no2_seq()`: Amount of NO<sub>2</sub> sequestered by urban green (in
    gr/s).
-   `runoff_prev()`: Runoff in the city after a specific rain event. It
    also computes the amount of rainwater harvested by urban agriculture
    initiatives.

## Set a scenario

Although `ediblecity` can also estimate indicators directly from an `sf`
object, the function `set_scenario` provides a basic tool to create an
scenario combining different proportions of elements of urban
agriculture. Some warnings are triggered when the function can’t satisfy
the parameters passed by the user.

``` r
library(ediblecity)

scenario <- set_scenario(city_example,
                         pGardens = 0.7,
                         pVacant = 0.8,
                         pRooftop = 0.6,
                         pCommercial = 0.5)
#> Warning: Only 328 rooftops out of 362.4 assumed satisfy the 'min_area_rooftop'
```

All attributes of urban agriculture elements are included in
`city_functions` dataframe. This can be used as default. Otherwise, a
customized dataframe can be provided to compute each indicator.

``` r

knitr::kable(city_functions)
```

| functions             | edible | public | pGreen | jobs  | volunteers | location | no2_seq1 | no2_seq2 | food1 | food2 | CN1 | CN2 | water_storage1 | water_storage2 | water_storage | X   | X.1 | X.2 |
|:----------------------|:-------|:-------|-------:|:------|:-----------|:---------|---------:|---------:|------:|------:|----:|----:|---------------:|---------------:|:--------------|:----|----:|----:|
| Edible private garden | TRUE   | FALSE  |     NA | FALSE | FALSE      | garden   |     0.07 |     0.09 |   0.2 |   6.6 |  85 |  88 |              0 |             10 | TRUE          | NA  |  67 |  81 |
| Community garden      | TRUE   | TRUE   |     NA | FALSE | TRUE       | vacant   |     0.07 |     0.09 |   0.2 |   2.2 |  85 |  88 |              0 |             10 | TRUE          | NA  |  67 |  81 |
| Commercial garden     | TRUE   | FALSE  |     NA | TRUE  | FALSE      | vacant   |     0.07 |     0.09 |   4.0 |   6.6 |  85 |  85 |              0 |             10 | TRUE          | NA  |  67 |  78 |
| Rooftop garden        | TRUE   | TRUE   |     NA | FALSE | TRUE       | rooftop  |     0.07 |     0.07 |   0.2 |   2.2 |  67 |  88 |              0 |             10 | TRUE          | NA  |  67 |  81 |
| Hydroponic rooftop    | TRUE   | FALSE  |     NA | TRUE  | FALSE      | rooftop  |     0.07 |     0.07 |   9.0 |  19.0 |  98 |  98 |              0 |             10 | TRUE          | NA  |  98 |  98 |
| Arable land           | TRUE   | FALSE  |    0.7 | FALSE | FALSE      |          |     0.00 |     0.07 |   4.0 |   6.6 |  85 |  88 |              0 |              0 | FALSE         | NA  |  67 |  81 |
| Normal garden         | FALSE  | FALSE  |    1.0 | FALSE | FALSE      |          |     0.07 |     0.07 |    NA |    NA |  74 |  86 |              0 |             10 | TRUE          | NA  |  39 |  89 |
| Permanent crops       | TRUE   | FALSE  |    0.7 | FALSE | FALSE      |          |     0.09 |     0.09 |   4.0 |   6.6 |  65 |  77 |              0 |              0 | FALSE         | NA  |  48 |  67 |
| Vacant                | FALSE  | FALSE  |    1.0 | FALSE | FALSE      |          |     0.07 |     0.09 |    NA |    NA |  74 |  87 |              0 |              0 | FALSE         | NA  |  62 |  80 |
| Grass                 | FALSE  | TRUE   |    1.0 | FALSE | FALSE      |          |     0.07 |     0.07 |    NA |    NA |  74 |  86 |              0 |              0 | FALSE         | NA  |  39 |  79 |
| Mulcher               | FALSE  | TRUE   |    1.0 | FALSE | FALSE      |          |     0.00 |     0.00 |    NA |    NA |  88 |  88 |              0 |              0 | FALSE         | NA  |  88 |  88 |
| Raised bed            | FALSE  | TRUE   |    1.0 | FALSE | FALSE      |          |     0.07 |     0.07 |    NA |    NA |  67 |  88 |              0 |              0 | FALSE         | NA  |  67 |  81 |
| Trees                 | FALSE  | FALSE  |    1.0 | FALSE | FALSE      |          |     0.11 |     0.11 |    NA |    NA |  70 |  77 |              0 |              0 | FALSE         | NA  |  30 |  77 |
| Vegetated pergola     | FALSE  | TRUE   |    1.0 | FALSE | FALSE      |          |     0.07 |     0.07 |    NA |    NA |  98 |  98 |              0 |              0 | FALSE         | NA  |  98 |  98 |
