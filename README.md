
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{shinyexample}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{shinyexample}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
shinyexample::run_app()
```

## About

You are reading the doc about version : 0.1.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-03-01 09:37:46 EST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading familia
#> ── R CMD check results ────────────────────────────────────── familia 0.1.0 ────
#> Duration: 9.6s
#> 
#> ❯ checking dependencies in R code ... WARNING
#>   '::' or ':::' import not declared from: ‘config’
#>   Namespaces in Imports field not imported from:
#>     ‘BIGr’ ‘DT’ ‘golem’ ‘openxlsx’ ‘scales’ ‘shiny’ ‘tidyverse’ ‘vcfR’
#>     ‘viridis’
#>     All declared Imports should be used.
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard files/directories found at top level:
#>     ‘app’ ‘app.R’ ‘dev’
#> 
#> 0 errors ✔ | 1 warning ✖ | 1 note ✖
#> Error:
#> ! R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> familia Coverage: 0.00%
#> R/app_config.R: 0.00%
```
