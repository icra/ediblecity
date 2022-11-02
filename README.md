
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

## Set a scenario

Although `ediblecity` can also esimate indicators directly from an `sf`
object, the function `set_scenario` provides a basic tool to create an
scenario combining different proportions of elements of urban
agriculture. Some warnings are triggered when the function can’t satisfy
the parametres passed by the user.

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
`city_functions` dataframe. This can be used as default or a customized
dataframe can be provided to each indicator.

``` r

datatable(city_functions)
```

<img src="man/figures/README-city_functions-1.png" width="100%" />
