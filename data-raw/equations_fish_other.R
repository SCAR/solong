refs$LaMe2000 <- bibentry(bibtype = "Article", key = "LaMe2000",
                          author = c(person(c("M"), "La Mesa"),
                                     person(c("M"), "Vacchi"),
                                     person("T", "Zunini Sertorio")),
                          year = 2000,
                          title = "Feeding plasticity of Trematomus newnesi (Pisces, Nototheniidae) in Terra Nova Bay, Ross Sea, in relation to environmental conditions",
                          journal = "Polar Biology",
                          volume = 23, pages = "38-45", doi="10.1007/s003000050006")

alleq_fish_other <- function(id) {
    switch(id,
           ## Trematomus newnesi 234628
           "234628_WW~TL_Dec_LaMe2000" = list(taxon_name = "Trematomus newnesi",
                                             taxon_aphia_id = 234628,
                                             equation = function(TL) tibble(allometric_value = 2.07e-03 * (TL ^ 2.01)),
                                             inputs = tibble(property = "total length", units = "mm"),
                                             return_property = "wet weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 43,
                                                                   "R^2", 0.74),
                                             notes="Calculated on specimens of both sexes, collected during December 1994",
                                             reference=refs$LaMe2000),

           "234628_WW~TL_Feb_LaMe2000" = list(taxon_name = "Trematomus newnesi",
                                             taxon_aphia_id = 234628,
                                             equation = function(TL) tibble(allometric_value = 2.41e-06 * (TL ^ 3.27)),
                                             inputs = tibble(property = "total length", units = "mm"),
                                             return_property = "wet weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 49,
                                                                   "R^2", 0.88),
                                             notes="Calculated on specimens of both sexes, collected during February 1998",
                                             reference=refs$LaMe2000),

           stop("unrecognized equation ID: ",id))
}
