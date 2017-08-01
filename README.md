
<!-- README.md is generated from README.Rmd. Please edit that file -->
solong
======

[![Travis-CI Build Status](https://travis-ci.org/SCAR/solong.svg?branch=master)](https://travis-ci.org/SCAR/solong) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SCAR/solong?branch=master&svg=true)](https://ci.appveyor.com/project/SCAR/solong) [![codecov](https://codecov.io/gh/SCAR/solong/branch/master/graph/badge.svg)](https://codecov.io/gh/SCAR/solong)

Overview
--------

This R package provides allometric equations that relate the body size of Southern Ocean taxa to their body part measurements.

Packaged equations
------------------

The package currently includes 383 equations, covering mostly cephalopods and fish. A breakdown of the number of equations by taxonomic class and the allometric property that they estimate:

|                |  hood length|  mantle length|  mass|  standard length|  total length|
|----------------|------------:|--------------:|-----:|----------------:|-------------:|
| Actinopterygii |            0|              0|   168|              104|             2|
| Cephalopoda    |            1|             53|    51|                0|             0|
| Elasmobranchii |            0|              0|     1|                0|             0|
| Malacostraca   |            0|              0|     2|                0|             0|
| Mammalia       |            0|              0|     1|                0|             0|

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
x <- tibble(LRL=c(11.3,13.9),species=c("Architeuthis dux"))
x
#> # A tibble: 2 x 2
#>     LRL          species
#>   <dbl>            <chr>
#> 1  11.3 Architeuthis dux
#> 2  13.9 Architeuthis dux
```

It doesn't matter what the column names are, but we do need to set the properties of the columns so that `solong` can find the appropriate data to use in each allometric equation. Here we've measured lower rostral length, so:

``` r
x$LRL <- sol_set_property(x$LRL,"lower rostral length")
```

Now we can apply allometric equations to our data. What equations do we have available for our species of interest?

``` r
sol_equations() %>%
  filter(taxon_name=="Architeuthis dux")
#> # A tibble: 3 x 10
#>            equation_id       taxon_name taxon_aphia_id equation
#>                  <chr>            <chr>          <dbl>   <list>
#> 1   342218_ML_Clar1986 Architeuthis dux         342218    <fun>
#> 2 342218_mass_Clar1986 Architeuthis dux         342218    <fun>
#> 3   342218_ML_Roel2000 Architeuthis dux         342218    <fun>
#> # ... with 6 more variables: inputs <list>, return_property <chr>,
#> #   return_units <chr>, reliability <list>, notes <chr>, reference <chr>
```

Here we use the equation with ID `342218_ML_Roel2000`, which is from Roeleveld (2000) and gives the mantle length of *Architeuthis dux* based on the lower rostral length.

This equation can be applied to to all rows:

``` r
sol_allometry(x,c("342218_ML_Roel2000"))
#> # A tibble: 2 x 6
#>                  LRL          species   allometric_value
#>   <S3: sol_property>            <chr> <S3: sol_property>
#> 1            11.3 mm Architeuthis dux        539.6881 mm
#> 2            13.9 mm Architeuthis dux        921.0553 mm
#> # ... with 3 more variables: allometric_value_lower <S3: sol_property>,
#> #   allometric_value_upper <S3: sol_property>, allometric_property <chr>
```

Or we can apply a different equation to each row. Here we could use different allometric equations for mantle length:

``` r
xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_ML_Clar1986"))
xa
#> # A tibble: 2 x 6
#>                  LRL          species   allometric_value
#>   <S3: sol_property>            <chr> <S3: sol_property>
#> 1            11.3 mm Architeuthis dux        539.6881 mm
#> 2            13.9 mm Architeuthis dux        768.8090 mm
#> # ... with 3 more variables: allometric_value_lower <S3: sol_property>,
#> #   allometric_value_upper <S3: sol_property>, allometric_property <chr>
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

Details
-------

We can apply equations that use different inputs, provided that they estimate the same output property. For example, equation `342218_mass_Clar1986` estimates the mass of the squid *Architeuthis dux* based on lower rostral length measurements:

``` r
sol_equation("342218_mass_Clar1986")
#> equation_id: 342218_mass_Clar1986
#>   taxon_name: Architeuthis dux, taxon_aphia_id: 342218
#>   equation: function (...) tibble(allometric_value = exp(-1.773 + 4.57 * log(...)))
#>   It takes as 1st input: lower rostral length (units: mm)
#>   It estimates: mass (units: g)
#>   Indicator of reliability: N=9
#>   Reference: Clarke (1986) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.
```

And equation `195932_mass_GaBu1988` estimates the mass of male Weddell seals based on their standard length:

``` r
sol_equation("195932_mass_GaBu1988")
#> equation_id: 195932_mass_GaBu1988
#>   taxon_name: Leptonychotes weddellii, taxon_aphia_id: 195932
#>   equation: function (...) tibble(allometric_value = 3.66 * ... - 489.3)
#>   It takes as 1st input: standard length (units: cm)
#>   It estimates: mass (units: kg)
#>   Indicator of reliability: N=15
#>   Notes: Applies to male animals
#>   Reference: Gales NJ & Burton HR (1988) Use of emetics and anaesthesia for dietary assessment of Weddell seals. Australian Wildlife Research 15:423-433
```

Note that this equation estimates mass in kg, whereas `342218_mass_Clar1986` estimates mass in g. We can apply the two equations together to a single data set:

``` r
x <- tibble(LRL=c(11.3,NA),species=c("Architeuthis dux","Leptonychotes weddellii"),SL=c(NA,175)) %>%
  mutate(LRL=sol_set_property(LRL,"lower rostral length"),
         SL=sol_set_property(SL,"standard length","cm"))

xa <- sol_allometry(x,c("342218_mass_Clar1986","195932_mass_GaBu1988"))

xa %>% select(species,allometric_property,allometric_value)
#> # A tibble: 2 x 3
#>                   species allometric_property   allometric_value
#>                     <chr>               <chr> <S3: sol_property>
#> 1        Architeuthis dux                mass         11029.72 g
#> 2 Leptonychotes weddellii                mass        151200.00 g
```

The output values are of property "mass" and have all been provided in g (because the output column `allometric value` must have a single set of units):

``` r
sol_get_property(xa$allometric_value)
#> [1] "mass"

units(xa$allometric_value)
#> $numerator
#> [1] "g"
#> 
#> $denominator
#> character(0)
#> 
#> attr(,"class")
#> [1] "symbolic_units"
```

If we try to apply equations that estimate different properties, we will get a warning:

``` r
x <- tibble(LRL=c(11.3,13.9),species=c("Architeuthis dux")) %>%
  mutate(LRL=sol_set_property(LRL,"lower rostral length"))

xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_mass_Clar1986"))
#> Warning in sol_allometry(x, c("342218_ML_Roel2000",
#> "342218_mass_Clar1986")): return values are not all of the same property

xa %>% select(species,allometric_property,allometric_value)
#> # A tibble: 2 x 3
#>            species allometric_property allometric_value
#>              <chr>               <chr>            <dbl>
#> 1 Architeuthis dux       mantle length         539.6881
#> 2 Architeuthis dux                mass       28416.6920
```

And while the `allometric_property` column still says which property was estimated for each row, the property type and units of the returned `allometric_value` will not be set, because they are not consistent across the different equations:

``` r
sol_get_property(xa$allometric_value)
#> character(0)
```

### Missing information

What happens if we don't have the required information in our data to use a particular equation? The `234631_SL~OL_WiMc1990` equation is for fish length, and requires otolith length (not present in our test data).

``` r
tryCatch(
  sol_allometry(x,"234631_SL~OL_WiMc1990"),
  error=function(e) conditionMessage(e)
)
#> [1] "could not find required input properties (otolith length) in data"
```

### Reliability of equations

Older equations were typically published along with the number of samples used to fit the equation (N) and the resulting goodness-of-fit of the equation to the data (R^2). These two quantities (if provided by the original source) can be found in the `reliability` component of an equation, and can be used to help decide if a given equation is appropriate for your data.

Some equations, typically from more recent publications, also provide the standard errors of the coefficients (or similar information) and thereby allow the `allometric_value_lower` and `allometric_value_upper` values to be estimated (the upper and lower bounds on the estimate). These should give a more reliable indicator of the precision of the estimated quantities.

``` r
x <- tibble(TL=c(30.5) %>% sol_set_property("total length",with_units="cm"))
sol_allometry(x,"234606_mass~TL_Arti2003") %>%
  select(allometric_value,allometric_value_lower,allometric_value_upper)
#> # A tibble: 1 x 3
#>     allometric_value allometric_value_lower allometric_value_upper
#>   <S3: sol_property>     <S3: sol_property>     <S3: sol_property>
#> 1         125.3436 g              67.5433 g             232.6067 g
```

### Taxonomy

Equations are registered against *taxon\_name* and *taxon\_aphia\_id* (the species identifier in the World Register of Marine Species). The *taxon\_aphia\_id* may be more reliable than species names, which can change over time. Users might like to look at the [worrms package](https://cran.r-project.org/package=worrms) for interacting with the World Register of Marine Species.
