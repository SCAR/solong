##oldrefs$Smal1993 <- "Smale MJ, Clarke MR, Klages TW, Roeleveld MA (1993) Octopod beak identification: resolution at a regional level (Cephalopoda, Octopoda: Southern Africa). South African Journal of Marine Sciences 13: 269-293"
refs$Smal1993 <- bibentry(bibtype="Article",key="Smal1993",
                             author=c(person(c("M","J"),"Smale"),person(c("M","R"),"Clarke"),person(c("T","W"),"Klages"),person(c("M","A"),"Roeleveld")),
                             year=1993,title="Octopod beak identification: resolution at a regional level (Cephalopoda, Octopoda: Southern Africa)",
                             journal="South African Journal of Marine Sciences",
                             volume=13,pages="269-293")

alleq_Smal1993 <- function(id) {
    switch(id,
           ## Octopus vulgaris
           "140605_ML_Smal1993"=list(taxon_name="Octopus vulgaris",
                                     taxon_aphia_id=140605,
                                     equation=function(...)tibble(allometric_value=-10.3825+10.5903*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=4.0,sample_maximum=18.5),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",60,
                                                         "R^2",0.87),
                                     reference=refs$Smal1993),
           "140605_HL_Smal1993"=list(taxon_name="Octopus vulgaris",
                                     taxon_aphia_id=140605,
                                     equation=function(...)tibble(allometric_value=1.1153+0.3261*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=7.2,sample_maximum=16.6),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",7,
                                                         "R^2",0.87),
                                     reference=refs$Smal1993),
           "140605_WW_Smal1993"=list(taxon_name="Octopus vulgaris",
                                       taxon_aphia_id=140605,
                                       equation=function(...)tibble(allometric_value=exp(1.1756+0.1263*...)),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=4.0,sample_maximum=18.5),
                                       return_property="wet weight",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",56,
                                                           "R^2",0.91),
                                       reference=refs$Smal1993),

           ## Octopus magnificus 225568, valid name is Enteroctopus magnificus 535784
           "225568_ML_Smal1993"=list(taxon_name="Octopus magnificus",
                                     taxon_aphia_id=225568,
                                     equation=function(...)tibble(allometric_value=12.4764+9.1742*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=5.2,sample_maximum=29.9),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",26,
                                                         "R^2",0.89),
                                     reference=refs$Smal1993),
           "225568_HL_Smal1993"=list(taxon_name="Octopus magnificus",
                                     taxon_aphia_id=225568,
                                     equation=function(...)tibble(allometric_value=-0.2624+0.4309*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=5.2,sample_maximum=29.9),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",30,
                                                         "R^2",0.96),
                                     reference=refs$Smal1993),
           "225568_WW_Smal1993"=list(taxon_name="Octopus magnificus",
                                       taxon_aphia_id=225568,
                                       equation=function(...)tibble(allometric_value=exp(1.1761+0.0940*...)),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=5.2,sample_maximum=29.9),
                                       return_property="wet weight",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",23,
                                                           "R^2",0.89),
                                       reference=refs$Smal1993),

           ## Velodona togata
           "342415_ML_Smal1993"=list(taxon_name="Velodona togata",
                                     taxon_aphia_id=342415,
                                     equation=function(...)tibble(allometric_value=-12.2509+7.2872*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=7.6,sample_maximum=20.2),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",18,
                                                         "R^2",0.87),
                                     reference=refs$Smal1993),
           "342415_HL_Smal1993"=list(taxon_name="Velodona togata",
                                     taxon_aphia_id=342415,
                                     equation=function(...)tibble(allometric_value=-0.3936+0.4854*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=7.6,sample_maximum=20.2),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",22,
                                                         "R^2",0.94),
                                     reference=refs$Smal1993),

           ## Bathypolypus valdiviae
           "341910_ML_Smal1993"=list(taxon_name="Bathypolypus valdiviae",
                                     taxon_aphia_id=341910,
                                     equation=function(...)tibble(allometric_value=-16.4188+8.0651*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=5.7,sample_maximum=9.5),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",24,
                                                         "R^2",0.61),
                                     reference=refs$Smal1993),
           "341910_HL_Smal1993"=list(taxon_name="Bathypolypus valdiviae",
                                     taxon_aphia_id=341910,
                                     equation=function(...)tibble(allometric_value=-0.3457+0.4379*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=5.7,sample_maximum=9.5),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",25,
                                                         "R^2",0.79),
                                     reference=refs$Smal1993),
           "341910_WW_Smal1993"=list(taxon_name="Bathypolypus valdiviae",
                                       taxon_aphia_id=341910,
                                       equation=function(...)tibble(allometric_value=exp(0.1398+0.1866*...)),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=5.7,sample_maximum=9.5),
                                       return_property="wet weight",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",18,
                                                           "R^2",0.87),
                                       reference=refs$Smal1993),

           ## Opisthoteuthis
           "138294_ML_Smal1993"=list(taxon_name="Opisthoteuthis",
                                     taxon_aphia_id=138294,
                                     equation=function(...)tibble(allometric_value=-26.0047+12.4858*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=4.2,sample_maximum=9.2),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",13,
                                                         "R^2",0.89),
                                     reference=refs$Smal1993),
           "138294_HL_Smal1993"=list(taxon_name="Opisthoteuthis",
                                     taxon_aphia_id=138294,
                                     equation=function(...)tibble(allometric_value=-0.3360+0.5619*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=4.2,sample_maximum=13.0),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",16,
                                                         "R^2",0.94),
                                     reference=refs$Smal1993),
           "138294_WW_Smal1993"=list(taxon_name="Opisthoteuthis",
                                       taxon_aphia_id=138294,
                                       equation=function(...)tibble(allometric_value=exp(0.5893+0.2413*...)),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=4.2,sample_maximum=9.2),
                                       return_property="wet weight",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",9,
                                                           "R^2",0.82),
                                       reference=refs$Smal1993),

           ## Tremoctopus violaceus 141694
           "141694_ML_Smal1993"=list(taxon_name="Tremoctopus violaceus",
                                     taxon_aphia_id=141694,
                                       equation=function(...)tibble(allometric_value=-46.7764+18.2608*...),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=4.6,sample_maximum=16.1),
                                       return_property="mantle length",
                                       return_units="mm",
                                       reliability=tribble(~type,~value,
                                                           "N",9,
                                                           "R^2",0.94),
                                       reference=refs$Smal1993),
           "141694_HL_Smal1993"=list(taxon_name="Tremoctopus violaceus",
                                     taxon_aphia_id=141694,
                                       equation=function(...)tibble(allometric_value=-1.4336+0.7930*...),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=4.6,sample_maximum=18.6),
                                       return_property="hood length",
                                       return_units="mm",
                                       reliability=tribble(~type,~value,
                                                           "N",13,
                                                           "R^2",0.94),
                                       reference=refs$Smal1993),
           "141694_WW_Smal1993"=list(taxon_name="Tremoctopus violaceus",
                                       taxon_aphia_id=141694,
                                         equation=function(...)tibble(allometric_value=exp(0.5318+0.1951*...)),
                                         inputs=tibble(property="crest length",units="mm",sample_minimum=4.6,sample_maximum=16.3),
                                         return_property="wet weight",
                                         return_units="g",
                                         reliability=tribble(~type,~value,
                                                             "N",11,
                                                             "R^2",0.88),
                                         reference=refs$Smal1993),

           ## Argonauta spp. 137676
           "137676_ML_Smal1993"=list(taxon_name="Argonauta",
                                     taxon_aphia_id=137676,
                                     equation=function(...)tibble(allometric_value=-1.1670+6.2816*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=3.5,sample_maximum=19.3),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",20,
                                                         "R^2",0.78),
                                     reference=refs$Smal1993),
           "137676_HL_Smal1993"=list(taxon_name="Argonauta",
                                     taxon_aphia_id=137676,
                                     equation=function(...)tibble(allometric_value=-0.1450+0.5033*...),
                                     inputs=tibble(property="crest length",units="mm",sample_minimum=3.5,sample_maximum=19.3),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",21,
                                                         "R^2",0.95),
                                     reference=refs$Smal1993),
           "137676_WW_Smal1993"=list(taxon_name="Argonauta",
                                       taxon_aphia_id=137676,
                                       equation=function(...)tibble(allometric_value=exp(-0.1110+0.1502*...)),
                                       inputs=tibble(property="crest length",units="mm",sample_minimum=3.5,sample_maximum=19.3),
                                       return_property="wet weight",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",17,
                                                           "R^2",0.87),
                                       reference=refs$Smal1993),

           stop("unrecognized equation ID: ",id))
}
