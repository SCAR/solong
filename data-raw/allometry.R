library(dplyr)
library(assertthat)

## define each unique equation, along with its nominal taxon and reference
## cephalopod equations from Xavier & Cherel

refs <- list(
    XC="Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.",
    Clar1986="Clarke (1986) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.",##"Clarke MR (1986) A handbook for the identification of cephalopod beaks. Clarendon Press, Oxford",
    Roel2000="Roeleveld (2000) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.", ##Roeleveld MAC (2000) Giant squid beaks: implications for systematics. Journal of the Marine Biological Association of the UK 80: 185-187
    LuWi1994="Lu CC, Williams R (1994) Contribution to the biology of squid in the Prydz Bay region, Antarctica. Antarctic Science 6: 223-229",
    Rodh1990="Rodhouse et al. (1990) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.", ##Rodhouse PG, Prince PA, Clarke MR, Murray AWA (1990) Cephalopod prey of the grey-headed albatross Diomedea chrysostoma. Marine Biology 104: 353-362
    Clar1962b="Clarke (1962b) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.", ##Clarke M (1962b) The identification of cephalopod "beaks" and the relationship between beak size and total body weight. Bulletin of the British Museum of Natural History B 8(10), 421-480
    LuIc2002="Lu CC & Ickeringill R (2002) Cephalopod beak identification and biomass estimation techniques: tools for dietary studies of southern Australian finfishes. Museum Victoria Science Reports 6:1-65",
    BASUnpub="BAS (unpublished data) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.",
    Piat2001="Piatkowski U, P\uFCtz K, Heinemann H (2001) Cephalopod prey of king penguins (Aptenodytes patagonicus) breeding at Volunteer Beach, Falkland Islands, during austral winter 1996. Fisheries Research 52:79-90. doi:10.1016/S0165-7836(01)00232-6",
    RoYe1990="Rodhouse & Yeatman (1990) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.", ##Rodhouse PG, Yeatman J (1990) Redescription of Martialia hyadesi Rochbrune and Mabille, 1889 (Mollusca: Cephalopoda) from the Southern Ocean. Bulletin of the British Museum of Natural History (Zoology) 56: 135-143
    SaHa2000="Santos RA, Haimovici M (2000) The Argentine short-finned squid Illex argentinus in the food webs of southern Brazil. Sarsia 85: 49-60",
    BrKl1987="Brown & Klages (1987) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.", ##Brown CR, Klages NT (1987) Seasonal and annual variation in diets of macaroni (Eudyptes chrysolophus chrysolophus) and southern rockhopper (E. chrysocome chrysocome) penguins at sub-Antarctic Marion Island. Journal of Zoology, London 212: 7-28
    Jack1995="Jackson (1995) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.", ##Jackson GD (1995) The use of beaks as tools for biomass estimation in the deepwater squid Moroteuthis ingens (Cephalopoda: Onychoteuthidae) in New Zealand waters. Polar Biology 15: 9-14
    CherUnpub="Cherel (unpublished data) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.",
    Grog2000="Gr\uF6ger J, Piatkowski U, Heinemann H (2000) Beak length analysis of the Southern Ocean squid Psychroteuthis glacialis (Cephalopoda: Psychroteuthidae) and its use for size and biomass estimation. Polar Biology 23:70-74. doi:10.1007/s003000050009",
    CollUnpub="Collins (unpublished data) in Xavier J & Cherel Y (2009 updated 2016) Cephalopod beak guide for the Southern Ocean. Cambridge, British Antarctic Survey, 129pp.",
    Smal1993="Smale MJ, Clarke MR, Klages TW, Roeleveld MA (1993) Octopod beak identification: resolution at a regional level (Cephalopoda, Octopoda: Southern Africa). South African Journal of Marine Sciences 13: 269-293",
    Jack1996="Jackson GD, McKinnon JF (1996) Beak length analysis of arrow squid Nototodarus sloanii (Cephalopoda: Ommastrephidae) in southern New Zealand waters. Polar Biology 16:227-230. doi:10.1007/BF02329211",
    WiMc1990="Williams R & McEldowney A (1990) A guide to the fish otoliths from waters off the Australian Antarctic Territory, Heard and Macquarie Islands. ANARE Research Notes 75. Antarctic Division, Australian Government",
    GaBu1988="Gales NJ & Burton HR (1988) Use of emetics and anaesthesia for dietary assessment of Weddell seals. Australian Wildlife Research 15:423-433")

alleq_xc <- function(id) {
    switch(id,
           ## Ancistrocheirus lesueuri
           ## ML=-41.3+40.75LRL ; ln M=-0.194+3.56ln LRL (n=23 for ML, n=21 for M) (Clarke 1986)
           "138747_ML_Clar1986"=list(taxon_name="Ancistrocheirus lesueuri",
                                     taxon_aphia_id=138747,
                                     equation=function(...)-41.3+40.75*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=23),
                                     reference=refs$Clar1986),
           "138747_mass_Clar1986"=list(taxon_name="Ancistrocheirus lesueuri",
                                       taxon_aphia_id=138747,
                                       equation=function(...)exp(-0.194+3.56*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=21),
                                       reference=refs$Clar1986),
           ## Architeuthis dux
           ## ML=-55.6+59.31LRL ; ln M=-1.773+4.57ln LRL (n=11 for ML; n=9 for M) (Clarke 1986)
           ## For relationships between ML and LRL, ML=10((LRL/11.2)+1.723214286) (n=43) might be
           ## better (Roeleveld 2000) with ML= mantle length (in mm), M= mass (in g) and LRL=
           ## lower rostral length (in mm).
           "342218_ML_Clar1986"=list(taxon_name="Architeuthis dux",
                                     taxon_aphia_id=342218,
                                     equation=function(...)-55.6+59.31*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=11),
                                     reference=refs$Clar1986),
           "342218_mass_Clar1986"=list(taxon_name="Architeuthis dux",
                                       taxon_aphia_id=342218,
                                       equation=function(...)exp(-1.773+4.57*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=9),
                                       reference=refs$Clar1986),
           "342218_ML_Roel2000"=list(taxon_name="Architeuthis dux",
                                     taxon_aphia_id=342218,
                                     equation=function(...)10^((.../11.2)+1.723214286),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=43),
                                     reference=refs$Roel2000),
           ## Bathyteuthis abyssicola
           ## ML=1.68+51.59LRL; ln M=2.855+3.38ln LRL (n=17 for both ML and M) (Clarke 1986)
           "138848_ML_Clar1986"=list(taxon_name="Bathyteuthis abyssicola",
                                     taxon_aphia_id=138848,
                                     equation=function(...)1.68+51.59*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=17),
                                     reference=refs$Clar1986),
           "138848_mass_Clar1986"=list(taxon_name="Bathyteuthis abyssicola",
                                       taxon_aphia_id=138848,
                                       equation=function(...)exp(2.855+3.38*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=17),
                                       reference=refs$Clar1986),

           ## Batoteuthis skolops (no specific equations)
           ## May use formulas for close families
           ## ML=11.4+24.46LRL; ln M=-0.241+2.7ln LRL (n=23 for ML, n=14 for M) (Clarke 1986),
           ## based on Chiroteuthis spp. formulas
           "137777_ML_Clar1986"=list(taxon_name="Chiroteuthis",
                                     taxon_aphia_id=137777,
                                     equation=function(...)11.4+24.46*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=23),
                                     reference=refs$Clar1986),
           "137777_mass_Clar1986"=list(taxon_name="Chiroteuthis",
                                     taxon_aphia_id=137777,
                                     equation=function(...)exp(-0.241+2.7*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tibble(type="N",value=14),
                                     reference=refs$Clar1986),
           ## ML=-1.8+29.08LRL ; ln M=0.184+2.88ln LRL (n=47 for ML, n=45 for M) (Clarke 1986),
           ## based on Mastigoteuthis spp. formulas
           "138168_ML_Clar1986"=list(taxon_name="Mastigoteuthis",
                                     taxon_aphia_id=138168,
                                     equation=function(...)-1.8+29.08*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=47),
                                     reference=refs$Clar1986),
           "138168_mass_Clar1986"=list(taxon_name="Mastigoteuthis",
                                     taxon_aphia_id=138168,
                                     equation=function(...)exp(0.184+2.88*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tibble(type="N",value=45),
                                     reference=refs$Clar1986),

           ## for the species of the family Brachioteuthidae
           ## ML= 16.31+20.18LRL ; ln M=0.55+1.41ln LRL (n= 11 for both ML and M) (Clarke 1986)
           "11758_ML_Clar1986"=list(taxon_name="Brachioteuthidae",
                                    taxon_aphia_id=11758,
                                    equation=function(...)16.31+20.18*...,
                                    inputs=tibble(property="lower rostral length",
                                    units="mm"),
                                    return_property="mantle length",
                                    return_units="mm",
                                    reliability=tibble(type="N",value=11),
                                    reference=refs$Clar1986),
           "11758_mass_Clar1986"=list(taxon_name="Brachioteuthidae",
                                      taxon_aphia_id=11758,
                                      equation=function(...)exp(0.55+1.41*log(...)),
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mass",
                                      return_units="g",
                                      reliability=tibble(type="N",value=11),
                                      reference=refs$Clar1986),
           ## Galiteuthis glacialis
           ##ML=6.676+83.785LRL; log M= 0.415+2.20 log LRL (n=25 for ML and M) (Lu & Williams 1994)
           "325297_ML_LuWi1994"=list(taxon_name="Galiteuthis glacialis",
                                     taxon_aphia_id=325297,
                                     equation=function(...)6.676+83.785*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",25,
                                                         "R^2",0.96),
                                     reference=refs$LuWi1994),
           "325297_mass_LuWi1994"=list(taxon_name="Galiteuthis glacialis",
                                       taxon_aphia_id=325297,
                                       equation=function(...)exp(0.415+2.20*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",25,
                                                           "R^2",0.94),
                                       reference=refs$LuWi1994),

           ## Taonius spp. formulas
           ## ML=-12.3+61.43LRL ; ln M=0.786+2.19 ln LRL (n=72 for ML, n=74 for M) (Rodhouse et al. 1990)
           "137853_ML_Rodh1990"=list(taxon_name="Taonius",
                                     taxon_aphia_id=137853,
                                     equation=function(...)-12.3+61.43*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=72),
                                     reference=refs$Rodh1990),
           "137853_mass_Rodh1990"=list(taxon_name="Taonius",
                                     taxon_aphia_id=137853,
                                     equation=function(...)exp(0.786+2.19*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tibble(type="N",value=74),
                                     reference=refs$Rodh1990),

           ## Teuthowenia pellucida
           ## ML=22.27+29.90LRL ; ln M=0.71+1.94 ln LRL (n=41 for ML and M) (Rodhouse et al. 1990)
           "341823_ML_Rodh1990"=list(taxon_name="Teuthowenia pellucida",
                                     taxon_aphia_id=341823,
                                     equation=function(...)22.27+29.90*(...),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=41),
                                     reference=refs$Rodh1990),
           "341823_mass_Rodh1990"=list(taxon_name="Teuthowenia pellucida",
                                     taxon_aphia_id=341823,
                                     equation=function(...)exp(0.71+1.94*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tibble(type="N",value=41),
                                     reference=refs$Rodh1990),

           ## Mesonychoteuthis hamiltoni
           ## ML=-12.3+61.43LRL (n=72) (Rodhouse et al. 1990), although the relationship is weak and
           ## therefore evaluate carefully if it applies well to your data
           "325299_ML_Rodh1990"=list(taxon_name="Mesonychoteuthis hamiltoni",
                                     taxon_aphia_id=325299,
                                     equation=function(...)-12.3+61.43*(...),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=72),
                                     notes="Noted by Xavier & Cherel: the relationship is weak and therefore evaluate carefully if it applies well to your data",
                                     reference=refs$Rodh1990),

           ## family Cranchiidae:
           ## ln M=ln 3.24 + 2.80 ln LRL (Clarke 1962b).
           "11774_mass_Clar1962"=list(taxon_name="Cranchiidae",
                                      taxon_aphia_id=11774,
                                      equation=function(...)exp(log(3.24)+2.80*log(...)),
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mass",
                                      return_units="g",
                                      reference=refs$Clar1962b),

           ## Cycloteuthis akimushkini
           "341824_ML_Clar1986"=list(taxon_name="Cycloteuthis akimushkini",
                                     taxon_aphia_id=341824,
                                     equation=function(...)31*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reference=refs$Clar1986),
           "341824_mass_Clar1986"=list(taxon_name="Cycloteuthis akimushkini",
                                     taxon_aphia_id=341824,
                                     equation=function(...)exp(1.89+1.95*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reference=refs$Clar1986),

           ## Gonatus spp.
           "138036_ML_Clar1986"=list(taxon_name="Gonatus",
                                     taxon_aphia_id=138036,
                                     equation=function(...)-43.4+42.87*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=17),
                                     reference=refs$Clar1986),
           "138036_mass_Clar1986"=list(taxon_name="Gonatus",
                                       taxon_aphia_id=138036,
                                       equation=function(...)exp(-0.655+3.33*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=20),
                                       reference=refs$Clar1986),

           ## The following equations is better for small beaks/specimens:
           ## ML=12.82+19.02LRL ; ln M=0.086+2.13ln LRL (Clarke 1986)
           "138036small_ML_Clar1986"=list(taxon_name="Gonatus",
                                          taxon_aphia_id=138036,
                                          equation=function(...)12.82+19.02*...,
                                          inputs=tibble(property="lower rostral length",
                                          units="mm"),
                                          return_property="mantle length",
                                          return_units="mm",
                                          notes="Noted by Xavier & Cherel: This equation is better for small beaks/specimens",
                                          reference=refs$Clar1986),
           "138036small_mass_Clar1986"=list(taxon_name="Gonatus",
                                            taxon_aphia_id=138036,
                                            equation=function(...)exp(0.086+2.13*log(...)),
                                            inputs=tibble(property="lower rostral length",
                                            units="mm"),
                                            return_property="mass",
                                            return_units="g",
                                            notes="Noted by Xavier & Cherel: This equation is better for small beaks/specimens",
                                            reference=refs$Clar1986),

           ## Histioteuthis bonnellii corpuscula (valid name Histioteuthis bonnellii)
           ## ML=17.1+8.99LRL (n=19) (Clarke 1986)
           "140111_ML_Clar1986"=list(taxon_name="Histioteuthis bonnellii",
                                     taxon_aphia_id=140111,
                                     equation=function(...)17.1+8.99*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=19),
                                     reference=refs$Clar1986),
           ## ML=1.82+15.24LRL; ln M= 1.16+2.70lnLRL (n=21 for ML and M, using total
           ## weight of preserved specimens) (Lu & Ickeringill 2002)
           "140111_ML_LuIc2002"=list(taxon_name="Histioteuthis bonnellii",
                                     taxon_aphia_id=140111,
                                     equation=function(...)1.82+15.24*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",21,
                                                         "R^2",0.93),
                                     reference=refs$LuIc2002),
           "140111_mass_LuIc2002"=list(taxon_name="Histioteuthis bonnellii",
                                     taxon_aphia_id=140111,
                                     equation=function(...)exp(1.16+2.70*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",21,
                                                         "R^2",0.86),
                                     notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                     reference=refs$LuIc2002),

           ## Histioteuthis macrohista
           ## ML=2.36+14.46LRL; ln M= 1.16+2.72lnLRL (n=8 for ML and for M, using total
           ## weight of preserved specimens) (Lu & Ickeringill 2002)
           "341870_ML_LuIc2002"=list(taxon_name="Histioteuthis macrohista",
                                     taxon_aphia_id=341870,
                                     equation=function(...)2.36+14.46*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",8,
                                                         "R^2",0.96),
                                     reference=refs$LuIc2002),
           "341870_mass_LuIc2002"=list(taxon_name="Histioteuthis macrohista",
                                       taxon_aphia_id=341870,
                                       equation=function(...)exp(1.16+2.72*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",8,
                                                           "R^2",0.96),
                                       notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                       reference=refs$LuIc2002),

           ## Histioteuthis miranda
           ## ML=-7.0+25.82LRL ; ln M=1.783+2.44ln LRL (n=27 for ML, n=14 for M) (Clarke
           ## 1986)
           "341871_ML_Clar1986"=list(taxon_name="Histioteuthis miranda",
                                     taxon_aphia_id=341871,
                                     equation=function(...)-7.0+25.82*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=27),
                                     reference=refs$Clar1986),
           "341871_mass_Clar1986"=list(taxon_name="Histioteuthis miranda",
                                       taxon_aphia_id=341871,
                                       equation=function(...)exp(1.783+2.44*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=14),
                                       reference=refs$Clar1986),

           ## ML=-26.51+34.21LRL; ln M= 0.86+3.04lnLRL (n=31 for ML, n=22 for M, using
           ## total weight of preserved specimens) (Lu & Ickeringill 2002)
           "341871_ML_LuIc2002"=list(taxon_name="Histioteuthis miranda",
                                     taxon_aphia_id=341871,
                                     equation=function(...)-26.51+34.21*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",31,
                                                         "R^2",0.86),
                                     reference=refs$LuIc2002),
           "341871_mass_LuIc2002"=list(taxon_name="Histioteuthis miranda",
                                       taxon_aphia_id=341871,
                                       equation=function(...)exp(0.86+3.04*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                                     units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                         "N",22,
                                                         "R^2",0.95),
                                       notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                       reference=refs$LuIc2002),

           ## Histioteuthis atlantica
           ## ML=-10.42+25.66LRL; ln M= 1.49+2.45lnLRL (n=21 for ML, n=19 for M, using
           ## total weight of preserved specimens) (Lu & Ickeringill 2002)
           "341864_ML_LuIc2002"=list(taxon_name="Histioteuthis atlantica",
                                     taxon_aphia_id=341864,
                                     equation=function(...)-10.42+25.66*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",21,
                                                         "R^2",0.89),
                                     reference=refs$LuIc2002),
           "341864_mass_LuIc2002"=list(taxon_name="Histioteuthis atlantica",
                                     taxon_aphia_id=341864,
                                     equation=function(...)exp(1.49+2.45*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",19,
                                                         "R^2",0.91),
                                     notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                     reference=refs$LuIc2002),

           ## Histioteuthis eltaninae
           ## ML=-3.65+24.48LRL; ln M= 0.33+3.11lnLRL (n=6 for ML, n=5 for M, using total
           ## weight of preserved specimens) (Lu & Ickeringill 2002)
           "341867_ML_LuIc2002"=list(taxon_name="Histioteuthis eltaninae",
                                     taxon_aphia_id=341867,
                                     equation=function(...)-3.65+24.48*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",6,
                                                         "R^2",0.99),
                                     notes="Noted by Lu & Ickeringill: no mature specimens examined.",
                                     reference=refs$LuIc2002),
           "341867_mass_LuIc2002"=list(taxon_name="Histioteuthis eltaninae",
                                     taxon_aphia_id=341867,
                                     equation=function(...)exp(0.33+3.11*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",5,
                                                         "R^2",0.79),
                                     notes="Noted by Lu & Ickeringill: no mature specimens examined. Used total weight of preserved specimens",
                                     reference=refs$LuIc2002),


           ## Lepidoteuthis grimaldii
           ## ML=36.2LRL ; ln M=-0.17+3.0ln LRL (British Antarctic Survey, unpublished data)
           "140193_ML_BASUnpub"=list(taxon_name="Lepidoteuthis grimaldii",
                                     taxon_aphia_id=140193,
                                     equation=function(...)36.2*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reference=refs$BASUnpub),
           "140193_mass_BASUnpub"=list(taxon_name="Lepidoteuthis grimaldii",
                                       taxon_aphia_id=140193,
                                       equation=function(...)exp(-0.17+3.0*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reference=refs$BASUnpub),

           ## ML=-10.60+50.57LRL (n=2, using total weight of preserved specimens) (Lu & Ickeringill
           ## 2002) but this relationship is obviously not strong
           "140193_ML_LuIc2002"=list(taxon_name="Lepidoteuthis grimaldii",
                                     taxon_aphia_id=140193,
                                     equation=function(...)-10.60+50.57*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",2,
                                                         "R^2",1.0),
                                     reference=refs$LuIc2002),


           ## Loligo gahi (accepted name Doryteuthis (Amerigo) gahi)
           ## ln ML= 4.23+1.01lnLRL ; ln M=2.25+2.39lnLRL (n=446) (British Antarctic Survey, unpublished data)
           "410351_ML_HatfUnpub"=list(taxon_name="Doryteuthis (Amerigo) gahi",
                                     taxon_aphia_id=410351,
                                     equation=function(...)exp(4.23+1.01*log(...)),
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=446),
                                     reference=paste0("Hatfield (pers. comm.) in ",refs$Piat2001)),
           "410351_mass_HatfUnpub"=list(taxon_name="Doryteuthis (Amerigo) gahi",
                                       taxon_aphia_id=410351,
                                       equation=function(...)exp(2.25+2.39*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=446),
                                       reference=paste0("Hatfield (pers. comm.) in ",refs$Piat2001)),

           ## Lycoteuthis lorigera
           ## ML=-13.04+34.56LRL; ln M= 0.32+3.00lnLRL (n=45 for ML and M, using total weight
           ## of preserved specimens) (Lu & Ickeringill 2002)
           "342361_ML_LuIc2002"=list(taxon_name="Lycoteuthis lorigera",
                                     taxon_aphia_id=342361,
                                     equation=function(...)-13.04+34.56*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",45,
                                                         "R^2",0.92),
                                     reference=refs$LuIc2002),
           "342361_mass_LuIc2002"=list(taxon_name="Lycoteuthis lorigera",
                                       taxon_aphia_id=342361,
                                       equation=function(...)exp(0.32+3.00*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",45,
                                                           "R^2",0.95),
                                       notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                       reference=refs$LuIc2002),


           ## Mastigoteuthis psychrophila
           ## ML=94.424+6.203LRL ; log M=0.701+1.779logLRL (n=19 for ML and M) (British
           ## Antarctic Survey, unpublished data)
           "341904_ML_BASUnpub"=list(taxon_name="Mastigoteuthis psychrophila",
                                     taxon_aphia_id=341904,
                                     equation=function(...)94.424+6.203*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=19),
                                     reference=refs$BASUnpub),
           "341904_mass_BASUnpub"=list(taxon_name="Mastigoteuthis psychrophila",
                                       taxon_aphia_id=341904,
                                       equation=function(...)exp(0.701+1.779*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=19),
                                       reference=refs$BASUnpub),


           ## Alluroteuthis antarcticus
           ## ML=-4.301+34.99LRL ; ln M=1.229+2.944ln LRL (n=22) (Piatkowski et al. 2001).
           "325302_ML_Piat2001"=list(taxon_name="Alluroteuthis antarcticus",
                                     taxon_aphia_id=325302,
                                     equation=function(...)-4.301+34.99*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=22),
                                     reference=refs$Piat2001),
           "325302_mass_Piat2001"=list(taxon_name="Alluroteuthis antarcticus",
                                       taxon_aphia_id=325302,
                                       equation=function(...)exp(1.229+2.944*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=22),
                                       reference=refs$Piat2001),


           ## Taningia danae
           ## ML=-556.9+75.22LRL; ln M=-0.874+3.42ln LRL (n=15 for ML and M) (Clarke 1986)
           "140609_ML_Clark1986"=list(taxon_name="Taningia danae",
                                      taxon_aphia_id=140609,
                                      equation=function(...)-556.9+75.22*...,
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tibble(type="N",value=15),
                                     reference=refs$Clar1986),
           "140609_mass_Clark1986"=list(taxon_name="Taningia danae",
                                        taxon_aphia_id=140609,
                                        equation=function(...)exp(-0.874+3.42*log(...)),
                                        inputs=tibble(property="lower rostral length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tibble(type="N",value=15),
                                        reference=refs$Clar1986),

           ## Octopoteuthis sp.
           ## ML=-0.4+17.33LRL; ln M=0.166+2.31ln LRL (n=30 for ML, n=22 M) (Clarke 1986)
           "138271_ML_Clark1986"=list(taxon_name="Octopoteuthis",
                                      taxon_aphia_id=138271,
                                      equation=function(...)-0.4+17.33*...,
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tibble(type="N",value=30),
                                      reference=refs$Clar1986),
           "138271_mass_Clark1986"=list(taxon_name="Octopoteuthis",
                                        taxon_aphia_id=138271,
                                        equation=function(...)exp(0.166+2.31*log(...)),
                                        inputs=tibble(property="lower rostral length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tibble(type="N",value=22),
                                        reference=refs$Clar1986),

           ## Martialia hyadesi
           ## ML= 102.0+29.47LRL ; ln M=2.405+2.012 ln LRL (n=67 for ML and M) (Rodhouse
           ## & Yeatman 1990)
           "325305_ML_RoYe1990"=list(taxon_name="Martialia hyadesi",
                                     taxon_aphia_id=325305,
                                     equation=function(...)102.0+29.47*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=67),
                                     reference=refs$RoYe1990),
           "325305_mass_RoYe1990"=list(taxon_name="Martialia hyadesi",
                                       taxon_aphia_id=325305,
                                       equation=function(...)exp(2.405+2.012*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=67),
                                       reference=refs$RoYe1990),

           ## Illex argentinus
           ## ML=-12.228+55.187LRL ; M=2.2750 LRL3.1210 (n=131for ML and M) (Santos & Haimovici 2000)
           "342064_ML_SaHa2000"=list(taxon_name="Illex argentinus",
                                     taxon_aphia_id=342064,
                                     equation=function(...)-12.228+55.187*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",131,
                                                         "R^2",0.989^2),
                                     reference=refs$SaHa2000),
           "342064_mass_SaHa2000"=list(taxon_name="Illex argentinus",
                                       taxon_aphia_id=342064,
                                       equation=function(...)2.2750*(...^3.1210),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",131,
                                                           "R^2",0.996^2),
                                       reference=refs$SaHa2000),


           ## Todarodes sp.
           ## ML=-11.3+41.36LRL ; ln M=0.783+2.83 ln LRL (Clarke 1986)
           "138281_ML_Clar1986"=list(taxon_name="Todarodes",
                                     taxon_aphia_id=138281,
                                     equation=function(...)-11.3+41.36*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reference=refs$Clar1986),
           "138281_mass_Clar1986"=list(taxon_name="Todarodes",
                                       taxon_aphia_id=138281,
                                       equation=function(...)exp(0.783+2.83*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reference=refs$Clar1986),

           ## Kondakovia longimana
           ## ML=-22.348+37.318LRL ; M=0.713LRL3.152 (n=13 for ML; n=22 for M) (Brown &
           ## Klages 1987)
           "325308_ML_BrKl1987"=list(taxon_name="Kondakovia longimana",
                                     taxon_aphia_id=325308,
                                     equation=function(...)-22.348+37.318*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",13,
                                                         "R^2",0.95),
                                     reference=refs$BrKl1987),
           "325308_mass_BrKl1987"=list(taxon_name="Kondakovia longimana",
                                       taxon_aphia_id=325308,
                                       equation=function(...)0.713*(...^3.152),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",22,
                                                           "R^2",0.99),
                                       reference=refs$BrKl1987),

           ## Moroteuthis ingens (valid name Onykia ingens)
           ## (Jackson 1995):
           ## Males: ML= 98.59+24.40LRL (n=82); females: ML=-27.84+44.63LRL (n=68)
           "410381M_ML_Jack1995"=list(taxon_name="Onykia ingens",
                                      taxon_aphia_id=410381,
                                      equation=function(...)98.59+24.40*...,
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tibble(type="N",value=82),
                                      notes="Applies to male animals",
                                      reference=refs$Jack1995),
           "410381F_ML_Jack1995"=list(taxon_name="Onykia ingens",
                                      taxon_aphia_id=410381,
                                      equation=function(...)-27.84+44.63*...,
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tibble(type="N",value=68),
                                      notes="Applies to female animals",
                                      reference=refs$Jack1995),
           ## Males: logM= 1.22+1.80logLRL (n=82); females: logM= 0.15+3.25logLRL (n=68)
           "410381M_mass_Jack1995"=list(taxon_name="Onykia ingens",
                                        taxon_aphia_id=410381,
                                        equation=function(...)exp(1.22+1.80*log(...)),
                                        inputs=tibble(property="lower rostral length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tibble(type="N",value=82),
                                        notes="Applies to male animals",
                                        reference=refs$Jack1995),
           "410381F_mass_Jack1995"=list(taxon_name="Onykia ingens",
                                        taxon_aphia_id=410381,
                                        equation=function(...)exp(0.15+3.25*log(...)),
                                        inputs=tibble(property="lower rostral length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tibble(type="N",value=68),
                                        notes="Applies to female animals",
                                        reference=refs$Jack1995),


           ## Moroteuthis knipovitchi (valid name is Filippovia knipovitchi)
           ## ML=-105.707+62.369LRL; ln M=-0.881+3.798lnLRL (n=7 for ML, n=5 for M) (Cherel,
           ## unpublished data)
           "550403_ML_CherUnpub"=list(taxon_name="Filippovia knipovitchi",
                                      taxon_aphia_id=550403,
                                      equation=function(...)-105.707+62.369*...,
                                      inputs=tibble(property="lower rostral length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tibble(type="N",value=7),
                                      reference=refs$CherUnpub),
           "550403_mass_CherUnpub"=list(taxon_name="Filippovia knipovitchi",
                                        taxon_aphia_id=550403,
                                        equation=function(...)exp(-0.881+3.798*log(...)),
                                        inputs=tibble(property="lower rostral length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tibble(type="N",value=5),
                                        reference=refs$CherUnpub),


    ## Moroteuthis robsoni (valid name Onykia robsoni)
    ## ML=-652.91+151.03LRL; ln M= -9.15+8.07lnLRL (n=8 for ML, n=6 for M, using total
    ## weight of preserved specimens) (Lu & Ickeringill 2002)
           "410384_ML_LuIc2002"=list(taxon_name="Onykia robsoni",
                                     taxon_aphia_id=410384,
                                     equation=function(...)-652.91+151.03*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",8,
                                                         "R^2",0.87),
                                     reference=refs$LuIc2002),
           "410384_mass_LuIc2002"=list(taxon_name="Onykia robsoni",
                                       taxon_aphia_id=410384,
                                       equation=function(...)exp(-9.15+8.07*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",6,
                                                           "R^2",0.94),
                                       notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                       reference=refs$LuIc2002),


           ## Onychoteuthis banksii
           ## ML=2.31+32.75LRL; ln M= -0.04+2.80lnLRL (n=10 for ML and M, using total weight
           ## of preserved specimens) (Lu & Ickeringill 2002)
           "140649_ML_LuIc2002"=list(taxon_name="Onychoteuthis banksii",
                                     taxon_aphia_id=140649,
                                     equation=function(...)2.31+32.75*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",10,
                                                         "R^2",0.86),
                                     reference=refs$LuIc2002),
           "140649_mass_LuIc2002"=list(taxon_name="Onychoteuthis banksii",
                                       taxon_aphia_id=140649,
                                       equation=function(...)exp(-0.04+2.80*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",10,
                                                           "R^2",0.94),
                                       notes="Noted by Lu & Ickeringill: used total weight of preserved specimens",
                                       reference=refs$LuIc2002),

           ## Pholidoteuthis massyae
           ## ML=11.3+41.09LRL ; ln M=0.976+2.83ln LRL (n=12 for ML, n=15 for M) (Clarke 1986)
           "410388_ML_Clar1986"=list(taxon_name="Pholidoteuthis massyae",
                                     taxon_aphia_id=410388,
                                     equation=function(...)11.3+41.09*...,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tibble(type="N",value=12),
                                     reference=refs$Clar1986),
           "410388_mass_Clar1986"=list(taxon_name="Pholidoteuthis massyae",
                                       taxon_aphia_id=410388,
                                       equation=function(...)exp(0.976+2.83*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tibble(type="N",value=15),
                                       reference=refs$Clar1986),

           ## Psychroteuthis glacialis
           ## ML= 50.6895LRL-8.6008LRL2+1.0823LRL3-8.7019 (n=211, R^2=0.93) ; ln M = 0.3422+2.1380
           ## lnLRL+0.2214lnLRL3 (n=211,R^2=0.95) (Groger et al. 2000)
           ## Groger J, Piatkowski U, Heinemann H (2000) Beak length analysis of the Southern Ocean squid Psychroteuthis glacialis (Cephalopoda: Psychroteuthidae) and its use for size and biomass estimation. Polar Biology 23:70-74. doi:10.1007/s003000050009
           "325312_ML_Grog2000"=list(taxon_name="Psychroteuthis glacialis",
                                     taxon_aphia_id=325312,
                                     equation=function(...)50.6895*...-8.6008*(...^2)+1.0823*(...^3)-8.7019,
                                     inputs=tibble(property="lower rostral length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",211,
                                                         "R^2",0.93),
                                     reference=refs$Grog2000),
           "325312_mass_Grog2000"=list(taxon_name="Psychroteuthis glacialis",
                                       taxon_aphia_id=325312,
                                       equation=function(...)exp(0.3422+2.1380*log(...)+0.2214*(log(...)^3)^3),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",211,
                                                           "R^2",0.95),
                                       reference=refs$Grog2000),

           ## Haliphron atlanticus
           ## Ln M=2.5+1.45ln LRL (British Antarctic Survey, unpublished data)
           "341781_mass_BASUnpub"=list(taxon_name="Haliphron atlanticus",
                                       taxon_aphia_id=341781,
                                       equation=function(...)exp(2.5+1.45*log(...)),
                                       inputs=tibble(property="lower rostral length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reference=refs$BASUnpub),


           ## Pareledone turqueti
           ## ML=17.70487+ 13.32812LHL; LnM =0.689269+2.542938LnLHL (n=7 for ML, n=23 for
           ## M), where LHL= lower hood length (in mm) (Collins, unpublished data)
           "239393_ML_CollUnpub"=list(taxon_name="Pareledone turqueti",
                                      taxon_aphia_id=239393,
                                      equation=function(...)17.70487+13.32812*...,
                                      inputs=tibble(property="lower hood length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tribble(~type,~value,
                                                         "N",7),
                                      reference=refs$CollUnpub),
           "239393_mass_CollUnpub"=list(taxon_name="Pareledone turqueti",
                                      taxon_aphia_id=239393,
                                      equation=function(...)exp(0.689269+2.542938*log(...)),
                                      inputs=tibble(property="lower hood length",
                                      units="mm"),
                                      return_property="mass",
                                      return_units="g",
                                      reliability=tribble(~type,~value,
                                                         "N",23),
                                      reference=refs$CollUnpub),


           ## Adelieledone polymorpha
           ## ML= -7,426229508+25,16393443LHL; Ln M =1,077552+3,200449LnLHL
           ##  (n=3 for ML, n= 39 for M) (Collins, unpublished data)
           "325319_ML_CollUnpub"=list(taxon_name="Adelieledone polymorpha",
                                      taxon_aphia_id=325319,
                                      equation=function(...)-7.426229508+25.16393443*...,
                                      inputs=tibble(property="lower hood length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tribble(~type,~value,
                                                         "N",3),
                                      reference=refs$CollUnpub),
           "325319_mass_CollUnpub"=list(taxon_name="Adelieledone polymorpha",
                                        taxon_aphia_id=325319,
                                        equation=function(...)exp(1.077552+3.200449*log(...)),
                                        inputs=tibble(property="lower hood length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tribble(~type,~value,
                                                            "N",39),
                                        reference=refs$CollUnpub),

           ## Benthoctopus thielei (valid name is Muusoctopus thielei)
           ## ML = 7.398+12.569LHL; lnM= 0.471+2.706lnLHL (n=48 for ML and M) (Cherel,
           ## unpublished data)
           "884005_ML_CherUnpub"=list(taxon_name="Muusoctopus thielei",
                                      taxon_aphia_id=884005,
                                      equation=function(...)7.398+12.569*...,
                                      inputs=tibble(property="lower hood length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tribble(~type,~value,
                                                         "N",48),
                                      reference=refs$CherUnpub),
           "884005_mass_CherUnpub"=list(taxon_name="Muusoctopus thielei",
                                        taxon_aphia_id=884005,
                                        equation=function(...)exp(0.471+2.706*log(...)),
                                        inputs=tibble(property="lower hood length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tribble(~type,~value,
                                                            "N",48),
                                        reference=refs$CherUnpub),

           ## Graneledone gonzalezi
           ## ML = 5.047+13.004LHL; lnM= 0.288+2.967lnLHL (n=54 for ML and M) (Cherel,
           ## unpublished data)
           "342224_ML_CherUnpub"=list(taxon_name="Graneledone gonzalezi",
                                      taxon_aphia_id=342224,
                                      equation=function(...)5.047+13.004*...,
                                      inputs=tibble(property="lower hood length",
                                      units="mm"),
                                      return_property="mantle length",
                                      return_units="mm",
                                      reliability=tribble(~type,~value,
                                                         "N",54),
                                      reference=refs$CherUnpub),
           "342224_mass_CherUnpub"=list(taxon_name="Graneledone gonzalezi",
                                        taxon_aphia_id=342224,
                                        equation=function(...)exp(0.288+2.967*log(...)),
                                        inputs=tibble(property="lower hood length",
                                        units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tribble(~type,~value,
                                                            "N",54),
                                        reference=refs$CherUnpub),

           ## Opisthoteuthis sp.
           ## ML=-26.0047+12.4858CL; logM=0.5893+0.2413CL (n= 13 for ML, n=9 for M) (Smale
           ## et al. 1993) where CL = Crest length (in mm)
           "138294_ML_Smal1993"=list(taxon_name="Opisthoteuthis",
                                     taxon_aphia_id=138294,
                                     equation=function(...)-26.0047+12.4858*...,
                                     inputs=tibble(property="crest length",
                                     units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",13,
                                                         "R^2",0.89),
                                     reference=refs$Smal1993),
           "138294_HL_Smal1993"=list(taxon_name="Opisthoteuthis",
                                     taxon_aphia_id=138294,
                                     equation=function(...)-0.3360+0.5619*...,
                                     inputs=tibble(property="crest length",
                                     units="mm"),
                                     return_property="hood length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",16,
                                                         "R^2",0.94),
                                     reference=refs$Smal1993),
           "138294_mass_Smal1993"=list(taxon_name="Opisthoteuthis",
                                       taxon_aphia_id=138294,
                                       equation=function(...)exp(0.5893+0.2413*...),
                                       inputs=tibble(property="crest length",
                                       units="mm"),
                                       return_property="mass",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",9,
                                                           "R^2",0.82),
                                       reference=refs$Smal1993),

           ## Nototodarus sloanii
           "342378_ML_Jack1996"=list(taxon_name="Nototodarus sloanii",
                                     taxon_aphia_id=342378,
                                     equation=function(...)168.83*log(...)+25.52,
                                     inputs=tibble(property="lower rostral length",units="mm"),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",170,
                                                         "R^2",0.90),
                                     reference=refs$Jack1996),
           "342378_ML_Jack1996"=list(taxon_name="Nototodarus sloanii",
                                     taxon_aphia_id=342378,
                                     equation=function(...)236.10*...-512.99,
                                     inputs=tibble(property="lower rostral length",units="mm"),
                                     return_property="mass",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",170,
                                                         "R^2",0.90),
                                     reference=refs$Jack1996),

           stop("unrecognized equation ID: ",id))
}


## otolith equations from Williams & McEldowney 1990
alleq_wm <- function(id) {
    switch(id,
           ## Bathylagus antarcticus
           "234631_SL~OL_WiMc1990"=list(taxon_name="Bathylagus antarcticus",
                                       taxon_aphia_id=234631,
                                       equation=function(...)56.16975*(...)-39.7831,
                                       inputs=tibble(property="otolith length",units="mm"),
                                       return_property="standard length",
                                       return_units="mm",
                                       reliability=tribble(~type,~value,
                                                           "N",17,
                                                           "R^2",0.875^2),
                                       reference=refs$WiMc1990),
           "234631_SL~OW_WiMc1990"=list(taxon_name="Bathylagus antarcticus",
                                       taxon_aphia_id=234631,
                                       equation=function(...)115.7744*(...)-47.0855,
                                       inputs=tibble(property="otolith width",units="mm"),
                                       return_property="standard length",
                                       return_units="mm",
                                       reliability=tribble(~type,~value,
                                                           "N",17,
                                                           "R^2",0.95^2),
                                       reference=refs$WiMc1990),
           "234631_mass~SL_WiMc1990"=list(taxon_name="Bathylagus antarcticus",
                                          taxon_aphia_id=234631,
                                          equation=function(...)(1.05e-06)*(...^3.446),
                                          inputs=tibble(property="standard length",units="mm"),
                                          return_property="mass",
                                          return_units="g",
                                          reliability=tribble(~type,~value,
                                                              "N",196,
                                                              "R^2",0.993^2),
                                          reference=refs$WiMc1990),

           ## "Photichthys argenteus" valid name is Phosichthys argenteus
           ## Aphia ID 220007
           ## only raw data (N=4) given in WiMc1990

           ## Krefftichthys anderssoni
           "234641_SL~OL_WiMc1990"=list(taxon_name="Krefftichthys anderssoni",
                                        taxon_aphia_id=234641,
                                        equation=function(...)41.21040*(...)-13.8900,
                                        inputs=tibble(property="otolith length",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",28,
                                                            "R^2",0.962^2),
                                        reference=refs$WiMc1990),
           "234641_SL~OW_WiMc1990"=list(taxon_name="Krefftichthys anderssoni",
                                        taxon_aphia_id=234641,
                                        equation=function(...)47.28366*(...)-9.10800,
                                        inputs=tibble(property="otolith width",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",28,
                                                            "R^2",0.94^2),
                                        reference=refs$WiMc1990),
           "234641_mass~SL_WiMc1990"=list(taxon_name="Krefftichthys anderssoni",
                                          taxon_aphia_id=234641,
                                          equation=function(...)(3.220e-06)*(...^3.296),
                                          inputs=tibble(property="standard length",units="mm"),
                                          return_property="mass",
                                          return_units="g",
                                          reliability=tribble(~type,~value,
                                                              "N",28,
                                                              "R^2",0.981^2),
                                          reference=refs$WiMc1990),

           ## Protomyctophum bolini
           "234714_SL~OL_WiMc1990"=list(taxon_name="Protomyctophum bolini",
                                        taxon_aphia_id=234714,
                                        equation=function(...)26.90736*...-5.68285,
                                        inputs=tibble(property="otolith length",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",31,
                                                            "R^2",0.733^2),
                                        reference=refs$WiMc1990),
           "234714_SL~OW_WiMc1990"=list(taxon_name="Protomyctophum bolini",
                                        taxon_aphia_id=234714,
                                        equation=function(...)27.02152*...-6.38535,
                                        inputs=tibble(property="otolith width",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",31,
                                                            "R^2",0.880^2),
                                        reference=refs$WiMc1990),
           "234714_mass~SL_WiMc1990"=list(taxon_name="Protomyctophum bolini",
                                          taxon_aphia_id=234714,
                                          equation=function(...)2.34e-05*(...^2.859),
                                          inputs=tibble(property="standard length",units="mm"),
                                          return_property="mass",
                                          return_units="g",
                                          reliability=tribble(~type,~value,
                                                              "N",36,
                                                              "R^2",0.981^2),
                                          reference=refs$WiMc1990),

           ## Protomyctophum parallelum
           ## no equations, n=8 data only

           ## Electrona antarctica
           "217697_SL~OL_WiMc1990"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(...)42.69686*...+0.278033,
                                        inputs=tibble(property="otolith length",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",86,
                                                            "R^2",0.984^2),
                                        reference=refs$WiMc1990),
           "217697_SL~OW_WiMc1990"=list(taxon_name="Electrona antarctica",
                                        taxon_aphia_id=217697,
                                        equation=function(...)33.30505*...+2.023806,
                                        inputs=tibble(property="otolith width",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",85,
                                                            "R^2",0.988^2),
                                        reference=refs$WiMc1990),
           "217697_mass~SL_WiMc1990"=list(taxon_name="Electrona antarctica",
                                          taxon_aphia_id=217697,
                                          equation=function(...)9.53e-06*(...^3.080),
                                          inputs=tibble(property="standard length",units="mm"),
                                          return_property="mass",
                                          return_units="g",
                                          reliability=tribble(~type,~value,
                                                              "N",227,
                                                              "R^2",0.988^2),
                                          reference=refs$WiMc1990),


           ## Pleuragramma antarctica
           "712788_SL~OL_WiMc1990"=list(taxon_name="Pleuragramma antarctica",
                                        taxon_aphia_id=712788,
                                        equation=function(...)76.67621*(...)+0.1705014,
                                        inputs=tibble(property="otolith length",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",766,
                                                            "R^2",0.868^2),
                                        reference=refs$WiMc1990),
           "712788_SL~OW_WiMc1990"=list(taxon_name="Pleuragramma antarctica",
                                        taxon_aphia_id=712788,
                                        equation=function(...)69.21882*(...)+15.81990,
                                        inputs=tibble(property="otolith width",units="mm"),
                                        return_property="standard length",
                                        return_units="mm",
                                        reliability=tribble(~type,~value,
                                                            "N",759,
                                                            "R^2",0.906^2),
                                        reference=refs$WiMc1990),
           "712788_mass~SL_WiMc1990"=list(taxon_name="Pleuragramma antarctica",
                                        taxon_aphia_id=712788,
                                        equation=function(...)2.71e-06*(...^3.200),
                                        inputs=tibble(property="standard length",units="mm"),
                                        return_property="mass",
                                        return_units="g",
                                        reliability=tribble(~type,~value,
                                                            "N",1297,
                                                            "R^2",0.992^2),
                                        reference=refs$WiMc1990),

           ## Bayesian length-weight: a=0.00575 (0.00309 - 0.01073), b=3.18 (3.01 - 3.35), in cm Total Length, based on LWR estimates for this species & (Sub)family-body (Ref. 93245).
           ## Froese, R., J. Thorson and R.B. Reyes Jr., 2013. A Bayesian approach for estimating length-weight relationships in fishes. J. Appl. Ichthyol. (2013):1-7.
           ## also http://oceanrep.geomar.de/21875/
           ## W=a*(TL^b)



           ## Brown and Klages 1987
           ## OD = otolith diameter. OD , SL, TL all mm. M in g
           ##The general relationship relating standard length to total length in myctophid fish was:
           ##SL = 0.17+0.80 TL (r2 = 0.97, S.D. = 0.018)
           ##Protomyctophum tenisoni and P. normani
           ##OD = 0.3 +0.027 SL (r2 = 0.85, n = 46)
           ##M = 1.282e-05 TL^2.868(r2 = 0.90, n = 28)
           ##Krefftichthys anderssoni
           ##OD = 0.416+0.022 SL (r2 = 0.88, n = 27)
           ##M = 5.36e-06 TL^3.080 (r2 = 0.90, n = 16)
           ##Electrona carlsbergi and E. subaspera
           ##OD = 0.254 + 0.042 SL (r2 = 0.9 1, n = 90)
           ##M = 7.43e-06 SL^3.159 ( r2 = 0.90, n = 60)
           ##Notothenia magellanica (Hecht & Cooper, 1986)
           ##TL = 30.96 OD^1.801 (r2 = 0.75, n = 82)
           ##M = 2.19e-05 TL^3.00 (r2 = 0.99, n = 133)


           ##Species: Leptonychotes weddellii
           ##Gender: male
           ##Equation: WEIGHT_KG=3.66*STANDARD_LENGTH_CM-489.3 (N=15)
           ##Source: Gales, N.J. and Burton, H.R. (1988) Use of emetics and anaesthesia for dietary assessment of Weddell seals. Australian Wildlife Research 15:423--433
           "195932_mass_GaBu1988"=list(taxon_name="Leptonychotes weddellii",
                                       taxon_aphia_id=195932,
                                       equation=function(...) 3.66*...-489.3,
                                       inputs=tibble(property="standard length",units="cm"),
                                       return_property="mass",
                                       return_units="kg",
                                       reliability=tribble(~type,~value,
                                                           "N",15),
                                       notes="Applies to male animals",
                                       reference=refs$GaBu1988),

           stop("unrecognized equation ID: ",id))
}


## wrapper function around alleq. Can override the taxon, if e.g. applying an equation developed for one species to another species
## can override reference
alleq_tbl <- function(id,taxon_name,taxon_aphia_id,notes,reference) {
    thiseq <- NULL
    try(thiseq <- alleq_xc(id),silent=TRUE)
    if (is.null(thiseq)) try(thiseq <- alleq_wm(id),silent=TRUE)
    if (is.null(thiseq)) stop("equation id not recognized: ",id)

    ## use the equation defaults for some thing, if not already specified
    if (missing(taxon_name)) taxon_name <- thiseq$taxon_name
    if (missing(taxon_aphia_id)) taxon_aphia_id <- thiseq$taxon_aphia_id
    if (missing(reference)) reference <- thiseq$reference
    if (missing(notes)) {
        notes <- if (is.null(thiseq$notes)) "" else thiseq$notes
    }
    if (is.null(thiseq$reliability)) thiseq$reliability <- tibble(type=character(),value=numeric())

    tribble(~equation_id,~taxon_name,~taxon_aphia_id,~equation,~inputs,~return_property,~return_units,~reliability,~notes,~reference,
            id,taxon_name,taxon_aphia_id,thiseq$equation,thiseq$inputs,thiseq$return_property,thiseq$return_units,thiseq$reliability,notes,reference)
}

build_allometry_df <- function() {
    ## equations from Xavier and Cherel
    ## Ancistrocheirus lesueuri
    x <- bind_rows(alleq_tbl("138747_ML_Clar1986"),alleq_tbl("138747_mass_Clar1986"))

    ## Architeuthis dux
    x <- bind_rows(x,alleq_tbl("342218_ML_Clar1986"),alleq_tbl("342218_mass_Clar1986"),alleq_tbl("342218_ML_Roel2000",notes="Noted by Xavier & Cherel: this equation for mantle_length from LRL might be better than the Clarke (1986) one"))

    ## Bathyteuthis abyssicola
    x <- bind_rows(x,alleq_tbl("138848_ML_Clar1986"),alleq_tbl("138848_mass_Clar1986"))

    ## Batoteuthis skolops (no specific equations)
    x <- bind_rows(x,alleq_tbl("137777_ML_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                               notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"),
                   alleq_tbl("137777_mass_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                              notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"),
                   alleq_tbl("138168_ML_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                             notes="Based on Mastigoteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"),
                   alleq_tbl("138168_mass_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                             notes="Based on Mastigoteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"))

    ## family Brachioteuthidae
    x <- bind_rows(x,alleq_tbl("11758_ML_Clar1986"),alleq_tbl("11758_mass_Clar1986"))
    ## note that this will require some additional coding in the user function, so that e.g. an input taxon_name of "Brachioteuthis picta" will find its way to this equation even though it does not directly match the taxon_name or taxon_aphia_id specified here


    ## Chiroteuthis veranyi (no specific equations)
    x <- bind_rows(x,alleq_tbl("137777_ML_Clar1986",taxon_name="Chiroteuthis veranii",taxon_aphia_id=139125,
                               notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Chiroteuthis veranii"),
                   alleq_tbl("137777_mass_Clar1986",taxon_name="Chiroteuthis veranii",taxon_aphia_id=139125,
                             notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Chiroteuthis veranii"))

    ## Galiteuthis glacialis
    x <- bind_rows(x,alleq_tbl("325297_ML_LuWi1994"),alleq_tbl("325297_mass_LuWi1994"))

    ## Galiteuthis stC sp. (Imber) (no specific equations)
    ## Galiteuthis sp. 3 (Imber) (no specific equations)
    ## Taonius sp. B (Voss) (no specific equations)
    ## Taonius sp. (Clarke) (no specific equations)

    ## Taonius sp
    ## Note that this is given as Taonius pavo in Rodhouse et al. 1990, but Taonius sp. in X&C
    x <- bind_rows(x,alleq_tbl("137853_ML_Rodh1990"),alleq_tbl("137853_mass_Rodh1990"))

    ## Teuthowenia pellucida
    ## Note that the Rodhouse et al 1990 paper does NOT have equations for T.p.
    ## this info taken from X&C
    x <- bind_rows(x,alleq_tbl("341823_ML_Rodh1990"),alleq_tbl("341823_mass_Rodh1990"))

    ## Mesonychoteuthis hamiltoni
    x <- bind_rows(x,alleq_tbl("325299_ML_Rodh1990"))

    ## Cranchiidae
    x <- bind_rows(x,alleq_tbl("11774_mass_Clar1962"))

    ## Cycloteuthis akimushkini
    x <- bind_rows(x,alleq_tbl("341824_ML_Clar1986"),alleq_tbl("341824_mass_Clar1986"))

    ## Gonatus
    x <- bind_rows(x,alleq_tbl("138036_ML_Clar1986"),alleq_tbl("138036_mass_Clar1986"))
    ## Gonatus (small specimens)
    x <- bind_rows(x,alleq_tbl("138036small_ML_Clar1986"),alleq_tbl("138036small_mass_Clar1986"))

    ## Gonatus antarcticus (no specific equations)
    x <- bind_rows(x,alleq_tbl("138036_ML_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                               notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"),
                   alleq_tbl("138036_mass_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                             notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"))
    ## (small specimens)
    x <- bind_rows(x,alleq_tbl("138036small_ML_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                               notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"),
                   alleq_tbl("138036small_mass_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                             notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"))

    ## Histioteuthis A, which has a deep notch in back of hood and a well-developed ridge
    ##  running to free corner of lateral wall:
    ## Histioteuthis arcturi (no specific equations)

    ## Histioteuthis bonnellii corpuscula (valid name Histioteuthis bonnellii)
    x <- bind_rows(x,alleq_tbl("140111_ML_Clar1986"),alleq_tbl("140111_ML_LuIc2002"),
                   alleq_tbl("140111_mass_LuIc2002"))

    ## Histioteuthis macrohista
    x <- bind_rows(x,alleq_tbl("341870_ML_LuIc2002"),alleq_tbl("341870_mass_LuIc2002"))

    ## Histioteuthis miranda
    x <- bind_rows(x,alleq_tbl("341871_ML_Clar1986"),alleq_tbl("341871_mass_Clar1986"))
    x <- bind_rows(x,alleq_tbl("341871_ML_LuIc2002"),alleq_tbl("341871_mass_LuIc2002"))

    ## Histioteuthis B has a shallow notch in back of hood and a weakly-developed ridge under
    ## the hood (evident in Histioteuthis atlantica juveniles) that becomes a slight fold running to free corner
    ## of lateral wall:

    ## Histioteuthis atlantica
    x <- bind_rows(x,alleq_tbl("341864_ML_LuIc2002"),alleq_tbl("341864_mass_LuIc2002"))

    ## Histioteuthis eltaninae
    x <- bind_rows(x,alleq_tbl("341867_ML_LuIc2002"),alleq_tbl("341867_mass_LuIc2002"))

    ## Lepidoteuthis grimaldii
    x <- bind_rows(x,alleq_tbl("140193_ML_BASUnpub"),alleq_tbl("140193_mass_BASUnpub"),alleq_tbl("140193_ML_LuIc2002"))

    ## Loligo gahi (accepted name Doryteuthis (Amerigo) gahi)
    x <- bind_rows(x,alleq_tbl("410351_ML_HatfUnpub"),alleq_tbl("410351_mass_HatfUnpub"))
    ## also with common name alternatives
    x <- bind_rows(x,alleq_tbl("410351_ML_HatfUnpub",taxon_name="Loligo gahi"),
                   alleq_tbl("410351_mass_HatfUnpub",taxon_name="Loligo gahi"))
    x <- bind_rows(x,alleq_tbl("410351_ML_HatfUnpub",taxon_name="Doryteuthis gahi"),
                   alleq_tbl("410351_mass_HatfUnpub",taxon_name="Doryteuthis gahi"))


    ## Lycoteuthis lorigera
    x <- bind_rows(x,alleq_tbl("342361_ML_LuIc2002"),alleq_tbl("342361_mass_LuIc2002"))

    ## Mastigoteuthis psychrophila
    x <- bind_rows(x,alleq_tbl("341904_ML_BASUnpub"),alleq_tbl("341904_mass_BASUnpub"))

    ## ?Mastigoteuthis A (Clarke) (no specific equations)

    ## Alluroteuthis antarcticus
    x <- bind_rows(x,alleq_tbl("325302_ML_Piat2001"),alleq_tbl("325302_mass_Piat2001"))

    ## Nototeuthis dimegacotyle (no specific equations)

    ## Taningia danae
    x <- bind_rows(x,alleq_tbl("140609_ML_Clark1986"),alleq_tbl("140609_mass_Clark1986"))

    ## Octopoteuthis sp.
    x <- bind_rows(x,alleq_tbl("138271_ML_Clark1986"),alleq_tbl("138271_mass_Clark1986"))

    ## Martialia hyadesi
    x <- bind_rows(x,alleq_tbl("325305_ML_RoYe1990"),alleq_tbl("325305_mass_RoYe1990"))

    ## Illex argentinus
    x <- bind_rows(x,alleq_tbl("342064_ML_SaHa2000"),alleq_tbl("342064_mass_SaHa2000"))

    ## Todarodes sp. (no specific equations)
    x <- bind_rows(x,alleq_tbl("138281_ML_Clar1986"),alleq_tbl("138281_mass_Clar1986"))

    ## Kondakovia longimana
    x <- bind_rows(x,alleq_tbl("325308_ML_BrKl1987"),alleq_tbl("325308_mass_BrKl1987"))

    ## Moroteuthis ingens
    x <- bind_rows(x,alleq_tbl("410381M_ML_Jack1995"),alleq_tbl("410381F_ML_Jack1995"),
                   alleq_tbl("410381M_mass_Jack1995"),alleq_tbl("410381F_mass_Jack1995"))

    ## Moroteuthis knipovitchi (valid name is Filippovia knipovitchi)
    x <- bind_rows(x,alleq_tbl("550403_ML_CherUnpub"),alleq_tbl("550403_mass_CherUnpub"))
    x <- bind_rows(x,alleq_tbl("550403_ML_CherUnpub",taxon_name="Moroteuthis knipovitchi"),
                   alleq_tbl("550403_mass_CherUnpub",taxon_name="Moroteuthis knipovitchi"))

    ## Moroteuthis robsoni (valid name Onykia robsoni)
    x <- bind_rows(x,alleq_tbl("410384_ML_LuIc2002"),alleq_tbl("410384_mass_LuIc2002"))
    x <- bind_rows(x,alleq_tbl("410384_ML_LuIc2002",taxon_name="Moroteuthis robsoni"),
                   alleq_tbl("410384_mass_LuIc2002",taxon_name="Moroteuthis robsoni"))

    ## Moroteuthis sp . B (Imber) (no specific equations)

    ## Onychoteuthis banksii
    x <- bind_rows(x,alleq_tbl("140649_ML_LuIc2002"),alleq_tbl("140649_mass_LuIc2002"))

    ## Pholidoteuthis massyae
    x <- bind_rows(x,alleq_tbl("410388_ML_Clar1986"),alleq_tbl("410388_mass_Clar1986"))

    ## Psychroteuthis glacialis
    x <- bind_rows(x,alleq_tbl("325312_ML_Grog2000"),alleq_tbl("325312_mass_Grog2000"))

    ## FAMILY SEPIOLIDAE
    ## cf. Stoloteuthis leucoptera (no allometric equations available)
    ## Oegopsida sp. A (Cherel) (no allometric equations available)

    ## Haliphron atlanticus
    x <- bind_rows(x,alleq_tbl("341781_mass_BASUnpub"))

    ## Cirrata sp. A (Cherel) (no allometric equations available)

    ## Pareledone turqueti
    x <- bind_rows(x,alleq_tbl("239393_ML_CollUnpub"),alleq_tbl("239393_mass_CollUnpub"))

    ## Adelieledone polymorpha
    x <- bind_rows(x,alleq_tbl("325319_ML_CollUnpub"),alleq_tbl("325319_mass_CollUnpub"))

    ## Benthoctopus thielei (valid name is Muusoctopus thielei)
    x <- bind_rows(x,alleq_tbl("884005_ML_CherUnpub"),alleq_tbl("884005_mass_CherUnpub"))
    x <- bind_rows(x,alleq_tbl("884005_ML_CherUnpub",taxon_name="Benthoctopus thielei"),
                   alleq_tbl("884005_mass_CherUnpub",taxon_name="Benthoctopus thielei"))

    ## Graneledone gonzalezi
    x <- bind_rows(x,alleq_tbl("342224_ML_CherUnpub"),alleq_tbl("342224_mass_CherUnpub"))

    ## Opisthoteuthis sp.
    ## ML=-26.0047+12.4858CL; logM=0.5893+0.2413CL (n= 13 for ML, n=9 for M) (Smale
    ## et al. 1993) where CL = Crest length (in mm)
    x <- bind_rows(x,alleq_tbl("138294_ML_Smal1993"),
                   alleq_tbl("138294_HL_Smal1993"),
                   alleq_tbl("138294_mass_Smal1993"))

    ## Stauroteuthis gilchristi (no allometric equations available)

    ## Williams & McEldowney 1990 otoliths
    ## Bathylagus antarcticus
    x <- bind_rows(x,alleq_tbl("234631_SL~OL_WiMc1990"),
                   alleq_tbl("234631_SL~OW_WiMc1990"),
                   alleq_tbl("234631_mass~SL_WiMc1990"))

    ## Krefftichthys anderssoni
    x <- bind_rows(x,alleq_tbl("234641_SL~OL_WiMc1990"),
                   alleq_tbl("234641_SL~OW_WiMc1990"),
                   alleq_tbl("234641_mass~SL_WiMc1990"))

    ## Pleuragramma antarctica
    x <- bind_rows(x,alleq_tbl("712788_SL~OL_WiMc1990"),
                   alleq_tbl("712788_SL~OW_WiMc1990"),
                   alleq_tbl("712788_mass~SL_WiMc1990"))
    ## used to be P.antarcticum, still a common misuse
    x <- bind_rows(x,alleq_tbl("712788_SL~OL_WiMc1990",taxon_name="Pleuragramma antarcticum"),
                   alleq_tbl("712788_SL~OW_WiMc1990",taxon_name="Pleuragramma antarcticum"),
                   alleq_tbl("712788_mass~SL_WiMc1990",taxon_name="Pleuragramma antarcticum"))
    ## Protomyctophum bolini
    x <- bind_rows(x,alleq_tbl("234714_SL~OL_WiMc1990"),
                   alleq_tbl("234714_SL~OW_WiMc1990"),
                   alleq_tbl("234714_mass~SL_WiMc1990"))

    ## Electrona antarctica
    x <- bind_rows(x,alleq_tbl("217697_SL~OL_WiMc1990"),
                   alleq_tbl("217697_SL~OW_WiMc1990"),
                   alleq_tbl("217697_mass~SL_WiMc1990"))

    ## Leptonychotes weddellii
    x <- bind_rows(x,alleq_tbl("195932_mass_GaBu1988"))

    x
}

allometric_equations <- build_allometry_df()
## todo: check each row that taxon_name and taxon_aphia_id match (or are expected mismatches, at least)

## some checks
assert_that(all(grepl("mass",allometric_equations$equation_id[allometric_equations$return_property=="mass"])))
assert_that(all(grepl("ML",allometric_equations$equation_id[allometric_equations$return_property=="mantle length"])))

devtools::use_data(allometric_equations,internal=FALSE,overwrite=TRUE)
