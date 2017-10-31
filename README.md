
<!-- README.md is generated from README.Rmd. Please edit that file -->
solong
======

[![Travis-CI Build Status](https://travis-ci.org/SCAR/solong.svg?branch=master)](https://travis-ci.org/SCAR/solong) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SCAR/solong?branch=master&svg=true)](https://ci.appveyor.com/project/SCAR/solong) [![codecov](https://codecov.io/gh/SCAR/solong/branch/master/graph/badge.svg)](https://codecov.io/gh/SCAR/solong)

Overview
--------

This R package provides allometric equations that relate the body size of Southern Ocean taxa to their body part measurements.

Packaged equations
------------------

The package currently includes 461 equations, covering mostly cephalopods and fish. A breakdown of the number of equations by taxonomic class and the allometric property that they estimate:

|                |  carbon weight|  dry weight|  energy density dry weight|  energy density wet weight|  hood length|  lipid content dry weight|  mantle length|  standard length|  total energy content|  total length|  water content wet weight|  wet weight|
|----------------|--------------:|-----------:|--------------------------:|--------------------------:|------------:|-------------------------:|--------------:|----------------:|---------------------:|-------------:|-------------------------:|-----------:|
| Actinopterygii |              0|           0|                          3|                          3|            0|                         7|              0|              106|                     8|             2|                         7|         178|
| Cephalopoda    |              0|           0|                          0|                          0|            8|                         0|             61|                0|                     0|             0|                         0|          58|
| Elasmobranchii |              0|           0|                          0|                          0|            0|                         0|              0|                0|                     0|             0|                         0|           1|
| Malacostraca   |              0|           0|                          0|                          0|            0|                         0|              0|                0|                     0|             3|                         0|           4|
| Mammalia       |              0|           0|                          0|                          0|            0|                         0|              0|                0|                     0|             0|                         0|           1|
| Thaliacea      |              1|           6|                          0|                          0|            0|                         0|              0|                0|                     0|             0|                         0|           4|

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
subset(sol_equations(),taxon_name=="Architeuthis dux") %>% summary
#> equation_id: 342218_ML_Clar1986
#>   taxon_name: Architeuthis dux, taxon_aphia_id: 342218
#>   equation: function (...) tibble(allometric_value = -55.6 + 59.31 * ...)
#>   It takes as 1st input: lower rostral length (units: mm, sample range: unknown to unknown)
#>   It estimates: mantle length (units: mm)
#>   Indicator of reliability: N=11
#>   Reference: Clarke MR (1986). "A handbook for the identification of cephalopod
#> beaks. Clarendon Press, Oxford." As cited in Xavier J & Cherel Y
#> (2009 updated 2016) Cephalopod beak guide for the Southern Ocean.
#> Cambridge, British Antarctic Survey, 129pp.
#> 
#> equation_id: 342218_WW_Clar1986
#>   taxon_name: Architeuthis dux, taxon_aphia_id: 342218
#>   equation: function (...) tibble(allometric_value = exp(-1.773 + 4.57 * log(...)))
#>   It takes as 1st input: lower rostral length (units: mm, sample range: unknown to unknown)
#>   It estimates: wet weight (units: g)
#>   Indicator of reliability: N=9
#>   Reference: Clarke MR (1986). "A handbook for the identification of cephalopod
#> beaks. Clarendon Press, Oxford." As cited in Xavier J & Cherel Y
#> (2009 updated 2016) Cephalopod beak guide for the Southern Ocean.
#> Cambridge, British Antarctic Survey, 129pp.
#> 
#> equation_id: 342218_ML_Roel2000
#>   taxon_name: Architeuthis dux, taxon_aphia_id: 342218
#>   equation: function (...) tibble(allometric_value = 10^((.../11.2) + 1.723214286))
#>   It takes as 1st input: lower rostral length (units: mm, sample range: 1 to 19.5)
#>   It estimates: mantle length (units: mm)
#>   Indicator of reliability: N=43
#>   Notes: Noted by Xavier & Cherel: this equation for mantle_length from LRL might be better than the Clarke (1986) one
#>   Reference: Roeleveld MAC (2000). "Giant squid beaks: implications for
#> systematics." _Journal of the Marine Biological Association of the
#> UK_, *80*, pp. 185-187.
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

We can apply equations that use different inputs, provided that they estimate the same output property. For example, equation `342218_WW_Clar1986` estimates the body weight of the squid *Architeuthis dux* based on lower rostral length measurements:

``` r
sol_equation("342218_WW_Clar1986") %>% summary
#> equation_id: 342218_WW_Clar1986
#>   taxon_name: Architeuthis dux, taxon_aphia_id: 342218
#>   equation: function (...) tibble(allometric_value = exp(-1.773 + 4.57 * log(...)))
#>   It takes as 1st input: lower rostral length (units: mm, sample range: unknown to unknown)
#>   It estimates: wet weight (units: g)
#>   Indicator of reliability: N=9
#>   Reference: Clarke MR (1986). "A handbook for the identification of cephalopod
#> beaks. Clarendon Press, Oxford." As cited in Xavier J & Cherel Y
#> (2009 updated 2016) Cephalopod beak guide for the Southern Ocean.
#> Cambridge, British Antarctic Survey, 129pp.
```

And equation `195932_WW_GaBu1988` estimates the weight of male Weddell seals based on their standard length:

``` r
sol_equation("195932_WW_GaBu1988") %>% summary
#> equation_id: 195932_WW_GaBu1988
#>   taxon_name: Leptonychotes weddellii, taxon_aphia_id: 195932
#>   equation: function (...) tibble(allometric_value = 3.66 * ... - 489.3)
#>   It takes as 1st input: standard length (units: cm, sample range: 170 to 236)
#>   It estimates: wet weight (units: kg)
#>   Indicator of reliability: N=15
#>   Notes: Applies to male animals
#>   Reference: Gales NJ and Burton HR (1988). "Use of emetics and anaesthesia for
#> dietary assessment of Weddell seals." _Australian Wildlife
#> Research_, *15*, pp. 423-433.
```

Note that this equation estimates weight in kg, whereas `342218_WW_Clar1986` estimates weight in g. We can apply the two equations together to a single data set:

``` r
x <- tibble(LRL=c(11.3,NA),species=c("Architeuthis dux","Leptonychotes weddellii"),SL=c(NA,175)) %>%
  mutate(LRL=sol_set_property(LRL,"lower rostral length"),
         SL=sol_set_property(SL,"standard length","cm"))

xa <- sol_allometry(x,c("342218_WW_Clar1986","195932_WW_GaBu1988"))

xa %>% select(species,allometric_property,allometric_value)
#> # A tibble: 2 x 3
#>                   species allometric_property   allometric_value
#>                     <chr>               <chr> <S3: sol_property>
#> 1        Architeuthis dux          wet weight         11029.72 g
#> 2 Leptonychotes weddellii          wet weight        151200.00 g
```

The output values are of property "wet weight" and have all been provided in g (because the output column `allometric value` must have a single set of units):

``` r
sol_get_property(xa$allometric_value)
#> [1] "wet weight"

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

xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_WW_Clar1986"))
#> Warning in sol_allometry(x, c("342218_ML_Roel2000", "342218_WW_Clar1986")):
#> return values are not all of the same property

xa %>% select(species,allometric_property,allometric_value)
#> # A tibble: 2 x 3
#>            species allometric_property allometric_value
#>              <chr>               <chr>            <dbl>
#> 1 Architeuthis dux       mantle length         539.6881
#> 2 Architeuthis dux          wet weight       28416.6920
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
x <- tibble(TL=c(10) %>% sol_set_property("carapace length",with_units="mm"))
sol_allometry(x,"369214_WW_Lake2003") %>%
  select(allometric_value,allometric_value_lower,allometric_value_upper)
#> # A tibble: 1 x 3
#>     allometric_value allometric_value_lower allometric_value_upper
#>   <S3: sol_property>     <S3: sol_property>     <S3: sol_property>
#> 1        0.8919773 g            0.6986516 g             1.138799 g
```

Attempts are made to avoid allowing an equation to extrapolate beyond its valid input data range. Some equations will explicitly return `NA` results for such inputs. The `inputs` component of the equation may also hold information about the range of the inputs used to fit the equation, which may help assess whether your data lie within its valid range.

### Other sources of equations

<http://www.fishbase.org> provides estimates of length-weight coefficients. These can be obtained via the `sol_fb_length_weight()` function (which uses the `rfishbase` package under the hood). For example:

``` r
myeq <- sol_fb_length_weight("Electrona antarctica")
myeq <- myeq[2,] ## use the second one of the two that are returned
summary(myeq)
#> equation_id: fishbase::11437
#>   taxon_name: Electrona antarctica, taxon_aphia_id: 217697
#>   equation: function (L) tibble(allometric_value = 0.00742 * (L^3.27))
#>   It takes as 1st input: standard length (units: cm, sample range: unknown to unknown)
#>   It estimates: wet weight (units: g)
#>   Indicator of reliability: R^2=0.957

x <- tibble(SL=10) %>%
   mutate(SL=sol_set_property(SL,"standard length",with_units="cm"))
sol_allometry(x,myeq)
#> # A tibble: 1 x 5
#>                   SL   allometric_value allometric_value_lower
#>   <S3: sol_property> <S3: sol_property>     <S3: sol_property>
#> 1              10 cm         13.81669 g                   NA g
#> # ... with 2 more variables: allometric_value_upper <S3: sol_property>,
#> #   allometric_property <chr>
```

### Adding your own equations

TODO: document, including what to do when a property is not part of the `sol_properties()` collection.

### Taxonomy

Equations are registered against *taxon\_name* and *taxon\_aphia\_id* (the species identifier in the World Register of Marine Species). The *taxon\_aphia\_id* may be more reliable than species names, which can change over time. Users might like to look at the [worrms package](https://cran.r-project.org/package=worrms) for interacting with the World Register of Marine Species.

### Other random stuff

Is equation X included in the package? Call `sol_equations()` to get all equations that are part of the package, and have a rummage through that.

To see all references from which equations have been drawn, do something like:

``` r
eqs <- sol_equations()
unique(eqs$reference)
#> [[1]]
#> Clarke MR (1986). "A handbook for the identification of cephalopod
#> beaks. Clarendon Press, Oxford." As cited in Xavier J & Cherel Y
#> (2009 updated 2016) Cephalopod beak guide for the Southern Ocean.
#> Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[2]]
#> Roeleveld MAC (2000). "Giant squid beaks: implications for
#> systematics." _Journal of the Marine Biological Association of the
#> UK_, *80*, pp. 185-187.
#> 
#> [[3]]
#> Lu CC and Williams R (1994). "Contribution to the biology of squid
#> in the Prydz Bay region, Antarctica." _Antarctic Science_, *6*,
#> pp. 223-229. doi: 10.1017/S0954102094000349 (URL:
#> http://doi.org/10.1017/S0954102094000349).
#> 
#> [[4]]
#> Rodhouse PG, Prince PA, Clarke MR and Murray AWA (1990).
#> "Cephalopod prey of the grey-headed albatross Diomedea
#> chrysostoma." _Marine Biology_, *104*, pp. 353-362. doi:
#> 10.1007/BF01314337 (URL: http://doi.org/10.1007/BF01314337), As
#> cited in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak
#> guide for the Southern Ocean. Cambridge, British Antarctic Survey,
#> 129pp.
#> 
#> [[5]]
#> Clarke M (1962). "The identification of cephalopod "beaks" and the
#> relationship between beak size and total body weight." _Bulletin
#> of the British Museum of Natural History B_, *8*, pp. 421-480. As
#> cited in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak
#> guide for the Southern Ocean. Cambridge, British Antarctic Survey,
#> 129pp.
#> 
#> [[6]]
#> Lu CC and Ickeringill R (2002). "Cephalopod beak identification
#> and biomass estimation techniques: tools for dietary studies of
#> southern Australian finfishes." _Museum Victoria Science Reports_,
#> *6*, pp. 1-65.
#> 
#> [[7]]
#> BAS (????). "Unpublished data." As cited in Xavier J & Cherel Y
#> (2009 updated 2016) Cephalopod beak guide for the Southern Ocean.
#> Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[8]]
#> Hatfield (2001). Pers. comm., as cited in Piatkowski U, Pütz K,
#> Heinemann H (2001) Cephalopod prey of king penguins (Aptenodytes
#> patagonicus) breeding at Volunteer Beach, Falkland Islands, during
#> austral winter 1996. Fisheries Research 52:79-90.
#> doi:10.1016/S0165-7836(01)00232-6.
#> 
#> [[9]]
#> Piatkowski U, Pütz K and Heinemann H (2001). "Cephalopod prey of
#> king penguins (Aptenodytes patagonicus) breeding at Volunteer
#> Beach, Falkland Islands, during austral winter 1996." _Fisheries
#> Research_, *52*, pp. 79-90. doi: 10.1016/S0165-7836(01)00232-6
#> (URL: http://doi.org/10.1016/S0165-7836(01)00232-6).
#> 
#> [[10]]
#> Rodhouse PG and Yeatman J (1990). "Redescription of Martialia
#> hyadesi Rochbrune and Mabille, 1889 (Mollusca: Cephalopoda) from
#> the Southern Ocean." _Bulletin of the British Museum of Natural
#> History (Zoology)_, *56*, pp. 135-143. As cited in Xavier J &
#> Cherel Y (2009 updated 2016) Cephalopod beak guide for the
#> Southern Ocean. Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[11]]
#> Santos RA and Haimovici M (2000). "The Argentine short-finned
#> squid Illex argentinus in the food webs of southern Brazil."
#> _Sarsia_, *85*, pp. 49-60. doi: 10.1080/00364827.2000.10414554
#> (URL: http://doi.org/10.1080/00364827.2000.10414554).
#> 
#> [[12]]
#> Brown CR and Klages NT (1987). "Seasonal and annual variation in
#> diets of macaroni (Eudyptes chrysolophus chrysolophus) and
#> southern rockhopper (E. chrysocome chrysocome) penguins at
#> sub-Antarctic Marion Island." _Journal of Zoology, London_, *212*,
#> pp. 7-28. doi: 10.1111/j.1469-7998.1987.tb05111.x (URL:
#> http://doi.org/10.1111/j.1469-7998.1987.tb05111.x), As cited in
#> Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for
#> the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[13]]
#> Jackson GD (1995). "The use of beaks as tools for biomass
#> estimation in the deepwater squid Moroteuthis ingens (Cephalopoda:
#> Onychoteuthidae) in New Zealand waters." _Polar Biology_, *15*,
#> pp. 9-14. doi: 10.1007/BF00236118 (URL:
#> http://doi.org/10.1007/BF00236118), As cited in Xavier J & Cherel
#> Y (2009 updated 2016) Cephalopod beak guide for the Southern
#> Ocean. Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[14]]
#> Cherel Y (????). "Unpublished data." As cited in Xavier J & Cherel
#> Y (2009 updated 2016) Cephalopod beak guide for the Southern
#> Ocean. Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[15]]
#> Gröger J, Piatkowski U and Heinemann H (2000). "Beak length
#> analysis of the Southern Ocean squid Psychroteuthis glacialis
#> (Cephalopoda: Psychroteuthidae) and its use for size and biomass
#> estimation." _Polar Biology_, *23*, pp. 70-74. doi:
#> 10.1007/s003000050009 (URL: http://doi.org/10.1007/s003000050009).
#> 
#> [[16]]
#> Collins (????). "Unpublished data." As cited in Xavier J & Cherel
#> Y (2009 updated 2016) Cephalopod beak guide for the Southern
#> Ocean. Cambridge, British Antarctic Survey, 129pp.
#> 
#> [[17]]
#> Smale MJ, Clarke MR, Klages TW and Roeleveld MA (1993). "Octopod
#> beak identification: resolution at a regional level (Cephalopoda,
#> Octopoda: Southern Africa)." _South African Journal of Marine
#> Sciences_, *13*, pp. 269-293.
#> 
#> [[18]]
#> Williams R and McEldowney A (1990). "A guide to the fish otoliths
#> from waters off the Australian Antarctic Territory, Heard and
#> Macquarie Islands." In _ANARE Research Notes_, volume 75.
#> Antarctic Division, Australian Government.
#> 
#> [[19]]
#> Artigues B, Morales-Nin B and Balguer<U+0EDA>s E (2003). "Fish
#> length-weight relationships in the Weddell Sea and Bransfield
#> Strait." _Polar Biology_, *26*, pp. 463-467. doi:
#> 10.1007/s00300-003-0505-0 (URL:
#> http://doi.org/10.1007/s00300-003-0505-0).
#> 
#> [[20]]
#> Goebel ME, Lipsky JD, Reiss CS and Loeb VJ (2007). "Using carapace
#> measurements to determine the sex of Antarctic krill, Euphausia
#> superba." _Polar Biology_, *30*, pp. 307-315. doi:
#> 10.1007/s00300-006-0184-8 (URL:
#> http://doi.org/10.1007/s00300-006-0184-8).
#> 
#> [[21]]
#> Morris DJ, Watkins JL, Ricketts C, Buchholz F and Priddle J
#> (1988). "An assessmant of the merits of length and weight
#> measurements of Antarctic krill Euphausia superba." _British
#> Antarctic Survey Bulletin_, *79*, pp. 27-50.
#> 
#> [[22]]
#> Hewitt RP, Watkins J, Naganobu M, Sushin V, Brierley AS, Demer D,
#> Kasatkina S, Takao Y, Goss C, Malyshko A and Brandon M (2004).
#> "Biomass of Antarctic krill in the Scotia Sea in January/February
#> 2000 and its use in revising an estimate of precautionary yield."
#> _Deep Sea Research Part II: Topical Studies in Oceanography_,
#> *51*, pp. 1215-1236. doi: 10.1016/j.dsr2.2004.06.011 (URL:
#> http://doi.org/10.1016/j.dsr2.2004.06.011).
#> 
#> [[23]]
#> Gales NJ and Burton HR (1988). "Use of emetics and anaesthesia for
#> dietary assessment of Weddell seals." _Australian Wildlife
#> Research_, *15*, pp. 423-433.
#> 
#> [[24]]
#> Lake S, Burton H and van den Hoff J (2003). "Regional, temporal
#> and fine-scale spatial variation in Weddell seal diet at four
#> coastal locations in east Antarctica." _Marine Ecology Progress
#> Series_, *254*, pp. 293-305. doi: 10.3354/meps254293 (URL:
#> http://doi.org/10.3354/meps254293).
#> 
#> [[25]]
#> Eastman JT and Devries AL (1997). "Biology and phenotypic
#> plasticity of the Antarctic nototheniid fish Trematomus newnesi in
#> McMurdo Sound." _Antarctic Science_, *9*, pp. 27-35. doi:
#> 10.1017/S0954102097000047 (URL:
#> http://doi.org/10.1017/S0954102097000047).
#> 
#> [[26]]
#> Van de Putte A, Flores H, Volckaert F and van Franeker JA (2006).
#> "Energy content of Antarctic mesopelagic fishes: implications for
#> the marine food web." _Polar Biology_, *29*, pp. 1045-1051. doi:
#> 10.1007/s00300-006-0148-z (URL:
#> http://doi.org/10.1007/s00300-006-0148-z).
#> 
#> [[27]]
#> Friedrich C and Hagen W (1994). "Lipid contents of five species of
#> notothenioid fish from high-Antarctic waters and ecological
#> implications." _Polar Biology_, *14*, pp. 359-369. doi:
#> 10.1007/BF00240256 (URL: http://doi.org/10.1007/BF00240256).
#> 
#> [[28]]
#> Vanella FA, Calvo J, Morriconi ER and Aureliano DR (2005).
#> "Somatic energy content and histological analysis of the gonads in
#> Antarctic fish from the Scotia Arc." _Scientia Marina_, *69*, pp.
#> S2 305-316. doi: 10.3989/scimar.2005.69s2305 (URL:
#> http://doi.org/10.3989/scimar.2005.69s2305).
#> 
#> [[29]]
#> Dubischar CD, Pakhomov EA, von Harbou L, Hunt BPV and Bathmann UV
#> (2012). "Salps in the Lazarev Sea, Southern Ocean: II. Biochemical
#> composition and potential prey value." _Marine Biology_, *159*,
#> pp. 15-24. doi: 10.1007/s00227-011-1785-5 (URL:
#> http://doi.org/10.1007/s00227-011-1785-5).
```

Related packages
----------------

-   [units](https://cran.r-project.org/package=units) for handling units of measurement
-   [worrms](https://cran.r-project.org/package=worrms) for taxonomy
-   [shapeR](https://cran.r-project.org/package=shapeR) for collection and analysis of otolith shape data
-   <https://github.com/James-Thorson/FishLife> for estimating fish life history parameters
-   [rfishbase](https://cran.r-project.org/package=rfishbase) an interface to www.fishbase.org
