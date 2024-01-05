# fed3 <a href="https://matiasandina.github.io/fed3/"><img src="man/figures/logo.png" align="right" height="138" alt="fed3 website" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/fed3)](https://CRAN.R-project.org/package=fed3)
<!-- badges: end -->

The goal of fed3 is to analyze data coming from [FED3 feeder devices from KravitzLabDevices](https://github.com/KravitzLabDevices/FED3/)

## Installation

You can install the current version of fed3 with:

``` r
# Not yet in CRAN
# install.packages("fed3")
devtools::install_github("matiasandina/fed3")
```

## Example


``` r
library(tidyverse)
library(fed3)
## Basic example code
## Reading multiple animals
fed_files <- list.files(folder, pattern = "^FED", full.names = T)
# assuming a vector of animal_ids that matches fed_files
fed_files <- set_names(fed_files, animal_ids)
fed_df <- map(fed_files, read_fed) %>% bind_rows(.id = "animal_id")
# Maybe the same animal was given more than one fed, so you need to recalculate pellets
fed_df <- fed_df %>% group_by(animal_id) %>% recalculate_pellets()
# Plotting Pellets vs datetime
ggplot(fed_df, aes(datetime, pellets, color = animal_id)) + geom_line()
```

## Docs

📖 Package documentation site [here](https://matiasandina.github.io/fed3/)

## Acknowledgements

This package draws a lot of inspiration from the [python pacakge](https://github.com/earnestt1234/fed3).

The logo reuses the fed cartoon from <a href="https://doi.org/10.5281/zenodo.5228170"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.5228170.svg" alt="DOI"></a>
