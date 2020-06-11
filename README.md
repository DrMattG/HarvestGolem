
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Harvest Shiny Application

-----

## Quick install and run

``` r
list.of.packages <- c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)
#> Loading required package: usethis
#devtools::install_github("DrMattG/HarvestGolem")
# The repository is currently locked as private and this will code will give an error
# Error: Failed to install 'HarvestGolem' from GitHub: HTTP error 404. Not Found Did you spell the repo owner (`DrMattG`)
# and repo name (`HarvestGolem`) correctly? - If spelling is correct, check that you have the required permissions to access the repo.
```

To install the Shiny app in an Rstudio session please use the following
code.

***Please note, that in R, code that is preceeded by a \# is not run (we
often use the \# symbol to write in non-code comments). You can run a
line of code that is preceeded by a \# by deleting the \# symbol.***

``` r
library(HarvestGolem)
#> Loading required package: tidyverse
#> -- Attaching packages ------------------------------------------------------------------------------------------------------ tidyverse 1.3.0 --
#> v ggplot2 3.3.0     v purrr   0.3.4
#> v tibble  3.0.1     v dplyr   0.8.5
#> v tidyr   1.0.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts --------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> Welcome to Shiny Application for visualising and predicting lynx harvest. If you have any problems please contact: matthew.grainger@nina.no
```

``` r
library(HarvestGolem)
```

-----

### Run the app

The package has one function (which launches the Shiny App locally) with
an option of either a female-only predictive model or a two-species,
four-age class predictive model. \[Link to assessment of these two
models\]

To run the female-only model type this code in to the Console after you
have loaded the library:

``` r

run_app("female")
```

To run the two-sex model type this code in to the Console after you have
loaded the library:

``` r

run_app("full")
```
