refs$UrMe1998 <- bibentry(bibtype = "Article", key = "UrMe1998",
                          author = c(person(c("H", "J"), "Urban"),
                                     person(c("G"), "Mercuri")),
                          year = 1998,
                          title = "Population dynamics  of  the bivalve  Laternula  elliptica  from Potter Cove, King George Island, South Shetland Islands",
                          journal = "Antarctic Science",
                          volume = 10, pages = "153-160", doi="10.1017/S0954102098000200")

alleq_UrMe1998 <- function(id) {
    switch(id,
           ## SFDW~SHL
           ## Laternula elliptica 197217, March 1994
           "197217_SFDW~SHL_UrMe1998" = list(taxon_name = "Laternula elliptica",
                                             taxon_aphia_id = 197217,
                                             equation = function(SHL) tibble(allometric_value = 0.00003 * (SHL ^ 2.860)),
                                             inputs = tibble(property = "shell length", units = "mm"),
                                             return_property = "shell-free dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 28,
                                                                   "R^2", 0.669),
                                             notes="Calculated on specimens before or during spawning, March 1994",
                                             reference=refs$UrMe1998),
           ## Laternula elliptica 197217, May 1994
           "197217_SFDW~SHL_postspawn_UrMe1998" = list(taxon_name = "Laternula elliptica",
                                             taxon_aphia_id = 197217,
                                             equation = function(SHL) tibble(allometric_value = 0.00044 * (SHL ^ 2.177)),
                                             inputs = tibble(property = "shell length", units = "mm"),
                                             return_property = "shell-free dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 27,
                                                                   "R^2", 0.853),
                                             notes="Calculated on specimens after spawning, May 1994",
                                             reference=refs$UrMe1998),

           ## AFDW~SHL
           "197217_AFDW~SHL_UrMe1998" = list(taxon_name = "Laternula elliptica",
                                             taxon_aphia_id = 197217,
                                             equation = function(SHL) tibble(allometric_value = 0.00240 * (SHL ^ 1.718)),
                                             inputs = tibble(property = "shell length", units = "mm"),
                                             return_property = "ash-free dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 57,
                                                                   "R^2", 0.804),
                                             reference=refs$UrMe1998),
           ## AFDW~SFDW
           "197217_AFDW~SFDW_UrMe1998" = list(taxon_name = "Laternula elliptica",
                                             taxon_aphia_id = 197217,
                                             equation = function(SFDW) tibble(allometric_value = 0.2740 + 0.648 * SFDW),
                                             inputs = tibble(property = "shell-free dry weight", units = "g"),
                                             return_property = "ash-free dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 57,
                                                                   "R^2", 0.967),
                                             reference=refs$UrMe1998),

           ## von Bertalanffy growth function
           "197217_SL~age_UrMe1998" = list(taxon_name = "Laternula elliptica",
                                           taxon_aphia_id = 197217,
                                           equation = function(t) tibble(allometric_value = 112.2 * (1-exp(-0.160 * t))),
                                           inputs = tibble(property = "age", units = "yr", sample_minimum = 0, sample_maximum = 12),
                                           return_property = "shell length",
                                           return_units = "mm",
                                           reliability = tribble(~type, ~value,
                                                                   "N", 195),
                                           notes = "von Bertalanffy growth function",
                                           reference=refs$UrMe1998),

           stop("unrecognized equation ID: ",id))
}
