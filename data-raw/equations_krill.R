refs$Goeb2007 <- bibentry(bibtype="Article",key="Goeb2007",
                             author=c(person(c("M","E"),"Goebel"),person(c("J","D"),"Lipsky"),person(c("C","S"),"Reiss"),person(c("V","J"),"Loeb")),
                             year=2007,
                             title="Using carapace measurements to determine the sex of Antarctic krill, Euphausia superba",
                             journal="Polar Biology",volume=30,pages="307-315",
                             doi="10.1007/s00300-006-0184-8")

refs$Morr1988 <- bibentry(bibtype="Article",key="Morr1988",
                             author=c(person(c("D","J"),"Morris"),person(c("J","L"),"Watkins"),person("C","Ricketts"),
                                      person("F","Buchholz"),person("J","Priddle")),
                             year=1988,
                             title="An assessmant of the merits of length and weight measurements of Antarctic krill Euphausia superba",
                             journal="British Antarctic Survey Bulletin",
                             volume=79,pages="27-50")

refs$Hewi2004 <- bibentry(bibtype="Article",key="Hewi2004",
                             author=c(person(c("R","P"),"Hewitt"),person("J","Watkins"),person("M","Naganobu"),
                                      person("V","Sushin"),person(c("A","S"),"Brierley"),person("D","Demer"),
                                      person("S","Kasatkina"),person("Y","Takao"),person("C","Goss"),
                                      person("A","Malyshko"),person("M","Brandon")),
                             year=2004,
                             title="Biomass of Antarctic krill in the Scotia Sea in January/February 2000 and its use in revising an estimate of precautionary yield",
                             journal="Deep Sea Research Part II: Topical Studies in Oceanography",
                             volume=51,pages="1215-1236",doi="10.1016/j.dsr2.2004.06.011")


##oldrefs$Goeb2007 <- "Goebel ME, Lipsky JD, Reiss CS, Loeb VJ (2007) Using carapace measurements to determine the sex of Antarctic krill, Euphausia superba. Polar Biology 30:307-315. doi:10.1007/s00300-006-0184-8"
##oldrefs$Morr1988 <- "Morris DJ, Watkins JL, Ricketts C, Buchholz F, Priddle J (1988) An assessmant of the merits of length and weight measurements of Antarctic krill Euphausia superba. British Antarctic Survey Bulletin 79:27-50"
##oldrefs$Hewi2004 <- "Hewitt RP, Watkins J, Naganobu M, Sushin V, Brierley AS, Demer D, Kasatkina S, Takao Y, Goss C, Malyshko A, Brandon M (2004) Biomass of Antarctic krill in the Scotia Sea in January/February 2000 and its use in revising an estimate of precautionary yield. Deep Sea Research Part II: Topical Studies in Oceanography 51:1215-1236. doi:10.1016/j.dsr2.2004.06.011"

alleq_krill <- function(id) {
    switch(id,
           "236217J_TL_Goeb2007"=list(taxon_name="Euphausia superba",
                                     taxon_aphia_id=236217,
                                     equation=function(RCL)tibble(allometric_value=10.43+2.26*RCL),
                                     inputs=tibble(property="removed carapace length",units="mm",sample_minimum=9,sample_maximum=12),
                                     return_property="total length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",154,
                                                         "R^2",0.40),
                                     notes="Applies to juvenile animals",
                                     reference=refs$Goeb2007),
           "236217F_TL_Goeb2007"=list(taxon_name="Euphausia superba",
                                     taxon_aphia_id=236217,
                                     equation=function(RCL)tibble(allometric_value=11.6+2.13*RCL),
                                     inputs=tibble(property="removed carapace length",units="mm",sample_minimum=9,sample_maximum=21),
                                     return_property="total length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",463,
                                                         "R^2",0.883),
                                     notes="Applies to adult female animals",
                                     reference=refs$Goeb2007),
           "236217M_TL_Goeb2007"=list(taxon_name="Euphausia superba",
                                     taxon_aphia_id=236217,
                                     equation=function(RCL)tibble(allometric_value=0.62+3.13*RCL),
                                     inputs=tibble(property="removed carapace length",units="mm",sample_minimum=9,sample_maximum=18),
                                     return_property="total length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",514,
                                                         "R^2",0.777),
                                     notes="Applies to adult male animals",
                                     reference=refs$Goeb2007),

           "236217_WW_Morr1988"=list(taxon_name="Euphausia superba",
                                     taxon_aphia_id=236217,
                                     equation=function(AT){ a <- 3.85; expon <- 3.20;
                                         out <- a*1e-06*(AT^expon)
                                         tibble(allometric_value=replace(out,AT<22 | AT>48,NA))},
                                     inputs=tibble(property="total length",units="mm",sample_minimum=22,sample_maximum=48),
                                     return_property="wet weight",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",4217),
                                     notes="Parameters from Morris et al. (1988) Table IV. Equation may not be valid outside of the range of data used to fit the equation; such values set to NA here",
                                     reference=refs$Morr1988),

           "236217_WW_Hewi2004"=list(taxon_name="Euphausia superba",
                                     taxon_aphia_id=236217,
                                     equation=function(SL){ a <- 2.236; expon <- 3.314;
                                         out <- a*1e-06*(SL^expon)
                                         tibble(allometric_value=out)},
                                     inputs=tibble(property="standard length",units="mm"),
                                     return_property="wet weight",
                                     return_units="g",
                                     notes="Parameters from Hewitt et al. (2004) equ. 3",
                                     reference=refs$Hewi2004),

           stop("unrecognized equation ID: ",id))
}
