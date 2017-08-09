refs$VanD2006 <- bibentry(bibtype="Article",key="VanD2006",
                          author=c(person(c("A"),"Van de Putte"),person("H","Flores"),person("F","Volckaert"),person(c("J","A"),"van Franeker")),
                          year=2006,
                          title="Energy content of Antarctic mesopelagic fishes: implications for the marine food web",
                          journal="Polar Biology",
                          volume=29,pages="1045-1051",
                          doi="10.1007/s00300-006-0148-z")

alleq_VanD2006 <- function(id) {
    switch(id,
           ## Electrona antarctica
           ## energy density dry weight as function of SL
           "217697_EDDW~SL_VanD2006"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(SL)tibble(allometric_value=18.8422*(SL^0.1164)),
                                        inputs=tibble(property="standard length",units="mm",sample_minimum=16,sample_maximum=83),
                                        return_property="energy density dry weight",
                                        return_units="kJ g-1",
                                        reliability=tribble(~type,~value,
                                                            "N",113,
                                                            "R^2",0.40),
                                        reference=refs$VanD2006),

           ## energy density wet weight as function of SL
           "217697_EDWW~SL_VanD2006"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(SL)tibble(allometric_value=2.2127*(SL^0.3745)),
                                        inputs=tibble(property="standard length",units="mm",sample_minimum=16,sample_maximum=83),
                                        return_property="energy density wet weight",
                                        return_units="kJ g-1",
                                        reliability=tribble(~type,~value,
                                                            "N",113,
                                                            "R^2",0.47),
                                        reference=refs$VanD2006),

           ## energy density dry weight as function of wet weight
           "217697_EDDW~WW_VanD2006"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(WW)tibble(allometric_value=29.3062*(WW^0.0367)),
                                        inputs=tibble(property="wet weight",units="g"),
                                        return_property="energy density dry weight",
                                        return_units="kJ g-1",
                                        reliability=tribble(~type,~value,
                                                            "N",113,
                                                            "R^2",0.44),
                                        reference=refs$VanD2006),

           ## energy density wet weight as function of wet weight
           "217697_EDWW~WW_VanD2006"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(WW)tibble(allometric_value=9.16507*(WW^0.1165)),
                                        inputs=tibble(property="wet weight",units="g"),
                                        return_property="energy density wet weight",
                                        return_units="kJ g-1",
                                        reliability=tribble(~type,~value,
                                                            "N",113,
                                                            "R^2",0.50),
                                        reference=refs$VanD2006),

           ## energy density dry weight as function of dry weight
           "217697_EDDW~DW_VanD2006"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(DW)tibble(allometric_value=30.5114*(DW^0.0347)),
                                        inputs=tibble(property="dry weight",units="g"),
                                        return_property="energy density dry weight",
                                        return_units="kJ g-1",
                                        reliability=tribble(~type,~value,
                                                            "N",113,
                                                            "R^2",0.46),
                                        reference=refs$VanD2006),

           ## energy density wet weight as function of dry weight
           "217697_EDWW~DW_VanD2006"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(DW)tibble(allometric_value=10.4856*(DW^0.1160)),
                                        inputs=tibble(property="dry weight",units="g"),
                                        return_property="energy density wet weight",
                                        return_units="kJ g-1",
                                        reliability=tribble(~type,~value,
                                                            "N",113,
                                                            "R^2",0.58),
                                        reference=refs$VanD2006),

           stop("unrecognized equation ID: ",id))
}

