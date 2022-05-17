
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cropsyst

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the released version of cropsyst from github with:

``` r
devtools::install_github("pruettm/cropsyst")
```

## Vignette

An overview of the package functions input data and parameters can be
found in the vignette “cs\_overview” and can be loaded with the below
code. Please install cropsyst with `build_vignettes = TRUE` to load
included vignette.

``` r
devtools::install_github("pruettm/cropsyst", build_vignettes = TRUE)
#> Downloading GitHub repo pruettm/cropsyst@HEAD
#> 
#> * checking for file ‘/private/var/folders/ty/w9ng55c52b5frbzrv4l_b7840000gn/T/RtmpP4LEck/remotesc0c75ab2f8ca/pruettm-cropsyst-b4cade9/DESCRIPTION’ ... OK
#> * preparing ‘cropsyst’:
#> * checking DESCRIPTION meta-information ... OK
#> * installing the package to build vignettes
#> * creating vignettes ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘cropsyst_0.0.1.tar.gz’
#> Installing package into '/private/var/folders/ty/w9ng55c52b5frbzrv4l_b7840000gn/T/RtmpQXyAqf/temp_libpathbc6e3cc403b9'
#> (as 'lib' is unspecified)

library(cropsyst)

vignette("cs_overview")
#> starting httpd help server ...
#>  done
```
