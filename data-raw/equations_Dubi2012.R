refs$Dubi2012 <- bibentry(bibtype="Article",key="Dubi2012",
                          author=c(person(c("C","D"),"Dubischar"),person(c("E","A"),"Pakhomov"),person(c("L"),"von Harbou"),person(c("B","P","V"),"Hunt"),person(c("U","V"),"Bathmann")),
                          year=2012,title="Salps in the Lazarev Sea, Southern Ocean: II. Biochemical composition and potential prey value",
                          journal="Marine Biology",
                          volume=159,pages="15-24",doi="10.1007/s00227-011-1785-5")

alleq_Dubi2012 <- function(id) {
    switch(id,
           ## WW~OAL summer
           ## Ihlea racovitzai 266542
           "266542_WW~OAL_summer_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                                taxon_aphia_id=266542,
                                                equation=function(...)tibble(allometric_value=0.008*(...^3.092)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="wet weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.809),
                                                notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_WW~OAL_summer_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(...)tibble(allometric_value=0.136*(...^2.376)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="wet weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.988),
                                                notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
                                                reference=refs$Dubi2012),
           ## WW~OAL autumn
           ## Ihlea racovitzai 266542
           ##"266542_WW~OAL_autumn_Dubi2012"=list(taxon_name="Ihlea racovitzai",
           ##                                     taxon_aphia_id=266542,
           ##                                     equation=function(...)tibble(allometric_value=0.001*(...^3.792)),
           ##                                     inputs=tibble(property="oral-atrial length",units="mm"),
           ##                                     return_property="wet weight",
           ##                                     return_units="mg",
           ##                                     reliability=tribble(~type,~value,
           ##                                                         "R^2",0.756),
           ##                                     notes="From specimens caught during an autumn cruise, Apr-May 2004",
           ##                                     reference=refs$Dubi2012),
           #### Salpa thompsoni 266545
           ##"266545_WW~OAL_autumn_Dubi2012"=list(taxon_name="Salpa thompsoni",
           ##                                     taxon_aphia_id=266545,
           ##                                     equation=function(...)tibble(allometric_value=0.483*(...^2.144)),
           ##                                     inputs=tibble(property="oral-atrial length",units="mm"),
           ##                                     return_property="wet weight",
           ##                                     return_units="mg",
           ##                                     reliability=tribble(~type,~value,
           ##                                                         "R^2",0.939),
           ##                                     notes="From specimens caught during an autumn cruise, Apr-May 2004",
           ##                                     reference=refs$Dubi2012),

           ## NOTE neither of the autumn equations appear to match the graphs shown in Fig 2B. Excluded here.


           ## WW~OAL winter
           ## Ihlea racovitzai 266542
           "266542_WW~OAL_winter_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                                taxon_aphia_id=266542,
                                                equation=function(...)tibble(allometric_value=0.057*(...^2.462)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="wet weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.875),
                                                notes="From specimens caught during a winter cruise, Jul-Aug 2006",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_WW~OAL_winter_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(...)tibble(allometric_value=0.011*(...^3.049)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="wet weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.98),
                                                notes="From specimens caught during a winter cruise, Jul-Aug 2006",
                                                reference=refs$Dubi2012),

           ## ---
           ## DW~OAL summer
           ## Ihlea racovitzai 266542
           "266542_DW~OAL_summer_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                                taxon_aphia_id=266542,
                                                equation=function(...)tibble(allometric_value=0.002*(...^2.731)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="dry weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.76),
                                                notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_DW~OAL_summer_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(...)tibble(allometric_value=0.005*(...^2.44)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="dry weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.998),
                                                notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
                                                reference=refs$Dubi2012),
           ## DW~OAL autumn
           ## Ihlea racovitzai 266542
           "266542_DW~OAL_autumn_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                                taxon_aphia_id=266542,
                                                equation=function(...)tibble(allometric_value=1.4e-05*(...^4.043)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="dry weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.774),
                                                notes="From specimens caught during an autumn cruise, Apr-May 2004",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_DW~OAL_autumn_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(...)tibble(allometric_value=0.047*(...^1.96)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="dry weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.946),
                                                notes="From specimens caught during an autumn cruise, Apr-May 2004",
                                                reference=refs$Dubi2012),
           ## DW~OAL winter
           ## Ihlea racovitzai 266542
           "266542_DW~OAL_winter_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                                taxon_aphia_id=266542,
                                                equation=function(...)tibble(allometric_value=0.0037*(...^2.426)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="dry weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.906),
                                                notes="From specimens caught during a winter cruise, Jul-Aug 2006",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_DW~OAL_winter_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(...)tibble(allometric_value=0.0034*(...^2.535)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="dry weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.95),
                                                notes="From specimens caught during a winter cruise, Jul-Aug 2006",
                                                reference=refs$Dubi2012),

           ## ---
           ## CW~OAL summer
           ## the two summer equations are excluded here, the printed curves in the paper do not match the equations given
           ## Ihlea racovitzai 266542
           ##"266542_CW~OAL_summer_Dubi2012"=list(taxon_name="Ihlea racovitzai",
           ##                                     taxon_aphia_id=266542,
           ##                                     equation=function(...)tibble(allometric_value=0.004*(...^2.023)),
           ##                                     inputs=tibble(property="oral-atrial length",units="mm"),
           ##                                     return_property="carbon weight",
           ##                                     return_units="mg",
           ##                                     reliability=tribble(~type,~value,
           ##                                                         "R^2",0.707),
           ##                                     notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
           ##                                     reference=refs$Dubi2012),
           #### Salpa thompsoni 266545
           ##"266545_CW~OAL_summer_Dubi2012"=list(taxon_name="Salpa thompsoni",
           ##                                     taxon_aphia_id=266545,
           ##                                     equation=function(...)tibble(allometric_value=0.002*(...^2.148)),
           ##                                     inputs=tibble(property="oral-atrial length",units="mm"),
           ##                                     return_property="carbon weight",
           ##                                     return_units="mg",
           ##                                     reliability=tribble(~type,~value,
           ##                                                         "R^2",0.96),
           ##                                     notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
           ##                                     reference=refs$Dubi2012),
           ## CW~OAL autumn
           ## Salpa thompsoni 266545
           "266545_CW~OAL_autumn_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(...)tibble(allometric_value=0.016*(...^1.641)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="carbon weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "R^2",0.851),
                                                notes="From specimens caught during an autumn cruise, Apr-May 2004",
                                                reference=refs$Dubi2012),
           ## CW~OAL winter
           ## Ihlea racovitzai 266542
           ## error in the printed equation in Figure 4: curve is exponential but equation is linear
           ##"266542_CW~OAL_winter_Dubi2012"=list(taxon_name="Ihlea racovitzai",
           ##                                     taxon_aphia_id=266542,
           ##                                     equation=function(...)tibble(allometric_value=0.007*(...^XXX)),
           ##                                     inputs=tibble(property="oral-atrial length",units="mm"),
           ##                                     return_property="carbon weight",
           ##                                     return_units="mg",
           ##                                     reliability=tribble(~type,~value,
           ##                                                         "R^2",0.906),
           ##                                     notes="From specimens caught during a winter cruise, Jul-Aug 2006",
           ##                                     reference=refs$Dubi2012),

           ## Salpa thompsoni 266545
           ## the equation here does not match the printed curve
           ##"266545_CW~OAL_winter_Dubi2012"=list(taxon_name="Salpa thompsoni",
           ##                                     taxon_aphia_id=266545,
           ##                                     equation=function(...)tibble(allometric_value=0.024*(...)),
           ##                                     inputs=tibble(property="oral-atrial length",units="mm"),##,sample_minimum=***,sample_maximum=***),
           ##                                     return_property="carbon weight",
           ##                                     return_units="mg",
           ##                                     reliability=tribble(~type,~value,
           ##                                                         "R^2",0.766),
           ##                                     notes="From specimens caught during a winter cruise, Jul-Aug 2006",
           ##                                     reference=refs$Dubi2012),

           ## all specimens, all seasons: lipid weight~OAL
           ## printed curve does not match equation given
           ##"137217_LpW~OAL_Dubi2012"=list(taxon_name="Salpidae",
           ##                               taxon_aphia_id=137217,
           ##                               equation=function(...)tibble(allometric_value=0.002*(...^1.775)),
           ##                               inputs=tibble(property="oral-atrial length",units="mm",sample_minimum=8,sample_maximum=65),
           ##                               return_property="lipid weight",
           ##                               return_units="mg",
           ##                               reliability=tribble(~type,~value,
           ##                                                   "R^2",0.765),
           ##                               notes="Based on specimens of Salpa thomsoni and Ihlea racovitzai",
           ##                               reference=refs$Dubi2012),


           stop("unrecognized equation ID: ",id))
}
