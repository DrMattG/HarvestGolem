
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Harvest Shiny Application

-----

## Quick install and run

To install the Shiny app in an Rstudio session please use the following
code.

***Please note, that in R, code that is preceded by a \# is not run (we
often use the \# symbol to write in non-code comments). You can run a
line of code that is preceded by a \# by deleting the \# symbol.***

``` r
list.of.packages <- c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)
#> Loading required package: usethis
```

``` r
devtools::install_github("DrMattG/HarvestGolem")
```

``` r
library(HarvestGolem)
#> Loading required package: tidyverse
#> Warning: package 'tidyverse' was built under R version 4.0.5
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.0     v dplyr   1.0.5
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> Welcome to Shiny Application for visualising and predicting lynx harvest. If you have any problems please contact: matthew.grainger@nina.no
```

-----

### Run the app

The package has one function (which launches the Shiny App locally).

To launch the App type this code in to the Console after you have loaded
the library:

``` r

HarvestGolem::run_app()
```
