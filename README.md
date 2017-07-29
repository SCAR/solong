
<!-- README.md is generated from README.Rmd. Please edit that file -->
solong
======

[![Travis-CI Build Status](https://travis-ci.org/SCAR/solong.svg?branch=master)](https://travis-ci.org/SCAR/solong) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SCAR/solong?branch=master&svg=true)](https://ci.appveyor.com/project/SCAR/solong)

Overview
--------

This R package provides allometric equations that relate the body size of Southern Ocean taxa to their body part measurements.

Installing
----------

``` r
install.packages("devtools")
library(devtools)
install_github("SCAR/solong")
```

Usage
-----

``` r
library(solong)
library(dplyr)
```

Let's say we have some measurements of *Architeuthis dux* squid beaks:

``` r
x <- data.frame(LRL=c(11.3,13.9),species=c("Architeuthis dux"),
  stringsAsFactors=FALSE)
x
#>    LRL          species
#> 1 11.3 Architeuthis dux
#> 2 13.9 Architeuthis dux
```

It doesn't matter what the column names are, but we do need to set the properties of the columns so that `solong` can find the appropriate data to use in each allometric equation. Here we've measured lower rostral length, so:

``` r
x$LRL <- sol_set_property(x$LRL,"lower rostral length")
```

Now we can apply allometric equations to our data. What equations do we have available for our species of interest?

``` r
sol_equations() %>%
  filter(taxon_name=="Architeuthis dux") %>%
  select(equation_id,inputs,return_property,return_units)
#> # A tibble: 3 x 4
#>            equation_id           inputs return_property return_units
#>                  <chr>           <list>           <chr>        <chr>
#> 1   342218_ML_Clar1986 <tibble [1 x 2]>   mantle length           mm
#> 2 342218_mass_Clar1986 <tibble [1 x 2]>            mass            g
#> 3   342218_ML_Roel2000 <tibble [1 x 2]>   mantle length           mm
```

Here we use the equation with ID `342218_ML_Roel2000`, which is from Roeleveld (2000) and gives the mantle length of *Architeuthis dux* based on the lower rostral length.

This equation can be applied to to all rows:

``` r
sol_allometry(x,c("342218_ML_Roel2000"))
#>       LRL          species        equation_id allometric_property
#> 1 11.3 mm Architeuthis dux 342218_ML_Roel2000       mantle length
#> 2 13.9 mm Architeuthis dux 342218_ML_Roel2000       mantle length
#>   allometric_value
#> 1      539.6881 mm
#> 2      921.0553 mm
```

Or we can apply a different equation to each row. Here we could use different allometric equations for mantle length:

``` r
xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_ML_Clar1986"))
xa
#>       LRL          species        equation_id allometric_property
#> 1 11.3 mm Architeuthis dux 342218_ML_Roel2000       mantle length
#> 2 13.9 mm Architeuthis dux 342218_ML_Clar1986       mantle length
#>   allometric_value
#> 1      539.6881 mm
#> 2      768.8090 mm
```

The `allometric_value` column contains the values that have been estimated, and the `allometric_property` column gives the name of the property that has been estimated.

We can also see that the returned `allometric_value` is of that specific property, with appropriate units:

``` r
sol_get_property(xa$allometric_value)
#> [1] "mantle length"
units(xa$allometric_value)
#> $numerator
#> [1] "mm"
#> 
#> $denominator
#> character(0)
#> 
#> attr(,"class")
#> [1] "symbolic_units"
```

If we try to apply equations that relate to different properties, we will get a warning:

``` r
xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_mass_Clar1986"))
#> Warning in sol_allometry(x, c("342218_ML_Roel2000",
#> "342218_mass_Clar1986")): return values are not all of the same property
xa
#>       LRL          species          equation_id allometric_property
#> 1 11.3 mm Architeuthis dux   342218_ML_Roel2000       mantle length
#> 2 13.9 mm Architeuthis dux 342218_mass_Clar1986                mass
#>   allometric_value
#> 1         539.6881
#> 2       28416.6920
```

And while the `allometric_property` column still says which property was estimated for each row, the property type of the returned `allometric_value` will not be set, because there are a mixture of them in this column:

``` r
sol_get_property(xa$allometric_value)
#> character(0)
```

What happens if we don't have the required information in our data to use a particular equation? The `234631_SL~OL_WiMc1990` equation is for fish length, and requires otolith length (not present in our test data).

``` r
tryCatch(
  sol_allometry(x,"234631_SL~OL_WiMc1990"),
  error=function(e) conditionMessage(e)
)
#> [1] "could not find required inputs (otolith length) in data"
```
