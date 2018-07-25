refs$AhSh1998 <- bibentry(bibtype = "Article", key = "AhSh1998",
                          author = c(person(c("I", "Y"), "Ahn"), person(c("J", "H"), "Shim")),
                          year = 1998,
                          title = "Summer metabolism of the Antarctic clam, Laternula elliptica (King and Broderip) in Maxwell Bay, King George Island and its implications",
                          journal = "Journal of Experimental Marine Biology and Ecology",
                          volume = 224, pages = "253-264", doi="10.1016/S0022-0981(97)00201-3")

alleq_AhSh1998 <- function(id) {
    switch(id,
           ## AFDW~SHL
           ## Laternula elliptica 197217
           "197217_AFDW~SHL_AhSh1998" = list(taxon_name = "Laternula elliptica",
                                             taxon_aphia_id = 197217,
                                             equation = function(...) tibble(allometric_value = 0.0000388 * (... ^ 2.677)),
                                             inputs = tibble(property = "shell length", units = "mm"),
                                             return_property = "ash-free dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 49,
                                                                   "R^2", 0.91),
                                             notes="From specimens collected by SCUBA divers in early December 1995 from 20-30 m depth in Marian Cove at sites close to the King Sejong Station on King George Island",
                                             reference=refs$AhSh1998),
           ## OCR~AFDW
           ## Laternula elliptica 197217
           "197217_OCR~AFDW_AhSh1998" = list(taxon_name = "Laternula elliptica",
                                             taxon_aphia_id = 197217,
                                             equation = function(...) tibble(allometric_value = 205.951 * (... ^ 0.73)),
                                             inputs = tibble(property = "ash-free dry weight", units = "g"),
                                             return_property = "oxygen consumption rate",
                                             return_units = "ug h-1",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 45,
                                                                   "R^2", 0.86),
                                             notes="From specimens collected by SCUBA divers in early December 1995 from 20-30 m depth in Marian Cove at sites close to the King Sejong Station on King George Island",
                                             reference=refs$AhSh1998),

           stop("unrecognized equation ID: ",id))
}
