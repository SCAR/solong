refs$EsFl2020 <- bibentry(bibtype = "Article", key = "EsFl2020",
                          author = c(person(c("P", "C"), "Escobar-Flores"),
                                     person(c("R", "L"), "O'Driscoll"),
                                     person(c("J", "C"), "Montgomery"),
                                     person(c("Y"), "Ladroit"),
                                     person(c("S"), "Jendersie")),
                          year = 2020,
                          title = "Estimates of density of mesopelagic fish in the Southern Ocean derived from bulk acoustic data collected by ships of opportunity",
                          journal = "Polar Biology", volume = 43, pages = "43-61",
                          doi = "10.1007/s00300-019-02611-3")

alleq_EsFl2020 <- function(id) {
    switch(id,
           "234631_WW~SL_EsFl2020" = list(taxon_name = "Bathylagus antarcticus",
                                           taxon_aphia_id = 234631,
                                           equation = function(SL) {
                                               a <- 0.002581; b <- 3.500192;
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "127286_WW~SL_EsFl2020" = list(taxon_name = "Cyclothone microdon",
                                           taxon_aphia_id = 127286,
                                           equation = function(SL) {
                                               a <- 0.003510; b <- 2.840000;
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "125819_WW~SL_EsFl2020" = list(taxon_name = "Diaphus",
                                           taxon_aphia_id = 125819,
                                           equation = function(SL) {
                                               a <- 0.006931; b <- 3.355468;
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "126194_WW~SL_EsFl2020" = list(taxon_name = "Vinciguerria",
                                           taxon_aphia_id = 126194,
                                           equation = function(SL) {
                                               a <- 0.009330; b <- 3.090000;
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "217719_WW~SL_EsFl2020" = list(taxon_name = "Symbolophorus boops",
                                           taxon_aphia_id = 217719,
                                           equation = function(SL) {
                                               a <- 0.007001; b <- 3.171734;
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "125832_WW~SL_EsFl2020" = list(taxon_name = "Protomyctophum",
                                           taxon_aphia_id = 125832,
                                           equation = function(SL) {
                                               a <- 0.015400; b <- 3.048990
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "712788_WW~SL_EsFl2020" = list(taxon_name = "Pleuragramma antarctica",
                                           taxon_aphia_id = 712788,
                                           equation = function(SL) {
                                               a <- 0.002462; b <- 3.408313
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "400296_WW~SL_EsFl2020" = list(taxon_name = "Photichthys argenteus",
                                           taxon_aphia_id = 400296,
                                           equation = function(SL) {
                                               a <- 0.001737; b <- 3.278296
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "234695_WW~SL_EsFl2020" = list(taxon_name = "Notolepis coatsi",
                                           taxon_aphia_id = 234695,
                                           equation = function(SL) {
                                               a <- 0.000261; b <- 3.556000
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "221503_WW~SL_EsFl2020" = list(taxon_name = "Nannobrachium achirus",
                                           taxon_aphia_id = 221503,
                                           equation = function(SL) {
                                               a <- 0.010530; b <- 2.833630
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "274968_WW~SL_EsFl2020" = list(taxon_name = "Maurolicus australis",
                                           taxon_aphia_id = 274968,
                                           equation = function(SL) {
                                               a <- 0.010863; b <- 3.182917;
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "313344_WW~SL_EsFl2020" = list(taxon_name = "Lepidonotothen kempi",
                                           taxon_aphia_id = 313344,
                                           equation = function(SL) {
                                               a <- 0.001351; b <- 3.679298
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "125825_WW~SL_EsFl2020" = list(taxon_name = "Lampanyctus",
                                           taxon_aphia_id = 125825,
                                           equation = function(SL) {
                                               a <- 0.019096; b <- 2.648692
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "217704_WW~SL_EsFl2020" = list(taxon_name = "Lampanyctodes hectoris",
                                           taxon_aphia_id = 217704,
                                           equation = function(SL) {
                                               a <- 0.014696; b <- 2.922115
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "234641_WW~SL_EsFl2020" = list(taxon_name = "Krefftichthys anderssoni",
                                           taxon_aphia_id = 234641,
                                           equation = function(SL) {
                                               a <- 0.009000; b <- 3.150000
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "205276_WW~SL_EsFl2020" = list(taxon_name = "Gymnoscopelus",
                                           taxon_aphia_id = 205276,
                                           equation = function(SL) {
                                               a <- 0.008667; b <- 3.032538
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "206941_WW~SL_EsFl2020" = list(taxon_name = "Metelectrona",
                                           taxon_aphia_id = 206941,
                                           equation = function(SL) {
                                               a <- 0.018277; b <- 2.854487
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           notes = "Used by the paper authors for species of the genus Electrona other than E. antarctica and E. carlsbergi, and Metelectrona sp.",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "125821_WW~SL_EsFl2020" = list(taxon_name = "Electrona",
                                           taxon_aphia_id = 125821,
                                           equation = function(SL) {
                                               a <- 0.018277; b <- 2.854487
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           notes = "Used by the paper authors for species of the genus Electrona other than E. antarctica and E. carlsbergi, and Metelectrona sp.",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "234638_WW~SL_EsFl2020" = list(taxon_name = "Electrona carlsbergi",
                                           taxon_aphia_id = 234638,
                                           equation = function(SL) {
                                               a <- 0.017026; b <- 2.850658
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           "217697_WW~SL_EsFl2020" = list(taxon_name = "Electrona antarctica",
                                           taxon_aphia_id = 217697,
                                           equation = function(SL) {
                                               a <- 0.018277; b <- 2.854487
                                               tibble(allometric_value = a*(SL^b))
                                           },
                                           inputs = tibble(property = "standard length", units = "cm"),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           reliability = tribble(~type, ~value,
                                                                 "Informal", "Equation was based on up to 20 individuals weighed to the nearest 1g"),
                                           reference=refs$EsFl2020),

           stop("unrecognized equation ID: ",id))
}
