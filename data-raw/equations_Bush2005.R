refs$Bush2005 <- bibentry(bibtype = "Article", key = "Bush2005",
                          author = c(person(c("T"), "Bushula"),
                                     person(c("E", "A"), "Pakhomov"),
                                     person(c("S"), "Kaehler"),
                                     person(c("S"), "Davis"),
                                     person(c("R", "M"), "Kalin")),
                          year = 2005,
                          title = "Diet and daily ration of two nototheniid fish on the shelf of the sub-Antarctic Prince Edward Islands",
                          journal = "Polar Biology",
                          volume = 28, pages = "585-593", doi="10.1007/s00300-005-0729-2")

alleq_Bush2005 <- function(id) {
    switch(id,
           ## WW~SL
           ## Lepidonotothen larseni 234610
           "234610_WW~SL_Bush2005" = list(taxon_name = "Lepidonotothen larseni",
                                          taxon_aphia_id = 234610,
                                          equation = function(...) tibble(allometric_value = 2e-06 * (... ^ 3.4556)),
                                             inputs = tibble(property = "standard length", units = "mm"),
                                             return_property = "wet weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 56,
                                                                   "R^2", 0.97),
                                             reference=refs$Bush2005),
           ## DW~SL
           ## Lepidonotothen larseni 234610
           "234610_DW~SL_Bush2005" = list(taxon_name = "Lepidonotothen larseni",
                                          taxon_aphia_id = 234610,
                                          equation = function(...) tibble(allometric_value = 6e-08 * (... ^ 3.898)),
                                             inputs = tibble(property = "standard length", units = "mm"),
                                             return_property = "dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 56,
                                                                   "R^2", 0.95),
                                             reference=refs$Bush2005),
           ## WW~SL
           ## Gobionotothen marionensis 234595
           "234595_WW~SL_Bush2005" = list(taxon_name = "Gobionotothen marionensis",
                                          taxon_aphia_id = 234595,
                                          equation = function(...) tibble(allometric_value = 6e-06 * (... ^ 3.2034)),
                                             inputs = tibble(property = "standard length", units = "mm"),
                                             return_property = "wet weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 45,
                                                                   "R^2", 0.98),
                                             reference=refs$Bush2005),
           ## DW~SL
           ## Gobionotothen marionensis 234595
           "234595_DW~SL_Bush2005" = list(taxon_name = "Gobionotothen marionensis",
                                          taxon_aphia_id = 234595,
                                          equation = function(...) tibble(allometric_value = 6e-07 * (... ^ 3.3796)),
                                             inputs = tibble(property = "standard length", units = "mm"),
                                             return_property = "dry weight",
                                             return_units = "g",
                                             reliability = tribble(~type, ~value,
                                                                   "N", 45,
                                                                   "R^2", 0.93),
                                             reference=refs$Bush2005),

           stop("unrecognized equation ID: ",id))
}
