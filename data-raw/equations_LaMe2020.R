refs$LaMe2020 <- bibentry(bibtype = "Article", key = "LaMe2020",
                          author = c(person("M", "La Mesa"),
                                     person("F", "Cal\xec"),
                                     person("E", "Riginella"),
                                     person("C", "Mazzoldi"),
                                     person(c("C", "D"), "Jones")),
                          year = 2020,
                          title = "Biological parameters of the High-Antarctic icefish, Cryodraco antarcticus (Channichthyidae) from the South Shetland Islands",
                          journal = "Polar Biology", volume = 30, doi = "10.1007/s00300-019-02617-x")

alleq_LaMe2020 <- function(id) {
    switch(id,
           "234720M_WW~TL_LaMe2020" = list(taxon_name = "Cryodraco antarcticus",
                                           taxon_aphia_id = 234720,
                                           equation = function(TL) {
                                               a = 0.0001405; b = 3.94; se_b = 0.033;
                                               tibble(allometric_value = a*(TL^b))
                                           },
                                           inputs = tibble(property = "total length", units = "cm", sample_minimum = 17.5, sample_maximum = 53.5),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           notes = "Applies to male animals",
                                           reliability = tribble(~type, ~value,
                                                                 "N", 368,
                                                                 "R^2",0.97),
                                           reference=refs$LaMe2020),
           "234720F_WW~TL_LaMe2020" = list(taxon_name = "Cryodraco antarcticus",
                                           taxon_aphia_id = 234720,
                                           equation = function(TL) {
                                               a = 0.0001604; b = 3.89; se_b = 0.025;
                                               tibble(allometric_value = a*(TL^b))
                                           },
                                           inputs = tibble(property = "total length", units = "cm", sample_minimum = 19.0, sample_maximum = 66.5),
                                           return_property = "wet weight",
                                           return_units = "g",
                                           notes = "Applies to female animals",
                                           reliability = tribble(~type, ~value,
                                                                 "N", 353,
                                                                 "R^2",0.98),
                                           reference=refs$LaMe2020),

           "234720M_TL~age_LaMe2020" = list(taxon_name = "Cryodraco antarcticus",
                                           taxon_aphia_id = 234720,
                                           equation = function(t) tibble(allometric_value = 50.8 * (1-exp(-0.15 * (t+1.02)))),
                                           inputs = tibble(property = "age", units = "yr", sample_minimum = 2, sample_maximum = 16),
                                           return_property = "total length",
                                           return_units = "cm",
                                           reliability = tribble(~type, ~value,
                                                                   "N", 137),
                                           notes = "von Bertalanffy growth function for male animals",
                                           reference=refs$LaMe2020),
           "234720F_TL~age_LaMe2020" = list(taxon_name = "Cryodraco antarcticus",
                                           taxon_aphia_id = 234720,
                                           equation = function(t) tibble(allometric_value = 75.4 * (1-exp(-0.08 * (t+1.46)))),
                                           inputs = tibble(property = "age", units = "yr", sample_minimum = 2, sample_maximum = 18),
                                           return_property = "total length",
                                           return_units = "cm",
                                           reliability = tribble(~type, ~value,
                                                                   "N", 128),
                                           notes = "von Bertalanffy growth function for female animals",
                                           reference=refs$LaMe2020),

           stop("unrecognized equation ID: ",id))
}
