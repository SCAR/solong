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
    JaMc1996="Jackson GD, McKinnon JF (1996) Beak length analysis of arrow squid Nototodarus sloanii (Cephalopoda: Ommastrephidae) in southern New Zealand waters. Polar Biology 16:227-230. doi:10.1007/BF02329211",
    WiMc1990="Williams R & McEldowney A (1990) A guide to the fish otoliths from waters off the Australian Antarctic Territory, Heard and Macquarie Islands. ANARE Research Notes 75. Antarctic Division, Australian Government",
    Arti2003="Artigues B, Morales-Nin B, Balguer\uEDas E (2003) Fish length-weight relationships in the Weddell Sea and Bransfield Strait. Polar Biology 26:463-467. doi:10.1007/s00300-003-0505-0",
    GaBu1988="Gales NJ & Burton HR (1988) Use of emetics and anaesthesia for dietary assessment of Weddell seals. Australian Wildlife Research 15:423-433",
    Lake2003="Lake S, Burton H, van den Hoff J (2003) Regional, temporal and fine-scale spatial variation in Weddell seal diet at four coastal locations in east Antarctica. Marine Ecology Progress Series 254:293-305. doi:10.3354/meps254293",
    EaDe1997="Eastman JT, Devries AL (1997) Biology and phenotypic plasticity of the Antarctic nototheniid fish Trematomus newnesi in McMurdo Sound. Antarctic Science 9:27-35. doi:10.1017/S0954102097000047")

source("data-raw/equations_XaCh2016.R")
source("data-raw/equations_Smal1993.R")
source("data-raw/equations_WiMc1990.R")
source("data-raw/equations_Arti2003.R")
source("data-raw/equations_Goeb2007.R")

alleq_other <- function(id) {
    switch(id,


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
           "195932_WW_GaBu1988"=list(taxon_name="Leptonychotes weddellii",
                                       taxon_aphia_id=195932,
                                       equation=function(...)tibble(allometric_value= 3.66*...-489.3),
                                       inputs=tibble(property="standard length",units="cm",sample_minimum=170,sample_maximum=236),
                                       return_property="wet weight",
                                       return_units="kg",
                                       reliability=tribble(~type,~value,
                                                           "N",15),
                                       notes="Applies to male animals",
                                       reference=refs$GaBu1988),

           ## Chorismus antarcticus from Lake et al. 2003
           "369214_WW_Lake2003"=list(taxon_name="Chorismus antarcticus",
                                       taxon_aphia_id=369214,
                                       equation=function(...)tibble(allometric_value=0.000943*(...^2.976)),
                                       inputs=tibble(property="carapace length",units="mm",sample_minimum=6,sample_maximum=16),
                                       return_property="wet weight",
                                       return_units="g",
                                       reliability=tribble(~type,~value,
                                                           "N",35,
                                                           "R^2",0.976),
                                       reference=refs$Lake2003),
           ## Nototodarus sloanii
           "342378_ML_JaMc1996"=list(taxon_name="Nototodarus sloanii",
                                     taxon_aphia_id=342378,
                                     equation=function(...)tibble(allometric_value=168.83*log(...)+25.52),
                                     inputs=tibble(property="lower rostral length",units="mm",sample_minimum=2,sample_maximum=7.2),
                                     return_property="mantle length",
                                     return_units="mm",
                                     reliability=tribble(~type,~value,
                                                         "N",170,
                                                         "R^2",0.90),
                                     reference=refs$JaMc1996),

           "342378_ML_JaMc1996"=list(taxon_name="Nototodarus sloanii",
                                     taxon_aphia_id=342378,
                                     equation=function(...)tibble(allometric_value=236.10*...-512.99),
                                     inputs=tibble(property="lower rostral length",units="mm",sample_minimum=2,sample_maximum=7.2),
                                     return_property="wet weight",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",170,
                                                         "R^2",0.90),
                                     reference=refs$JaMc1996),

           ## Trematomus newnesi from EaDe1997
           "234628_WW~SL_EaDe1997"=list(taxon_name="Trematomus newnesi",
                                     taxon_aphia_id=234628,
                                     equation=function(...)tibble(allometric_value=3.17e-06*(...^3.34)),
                                     inputs=tibble(property="standard length",units="mm",sample_minimum=93,sample_maximum=239),
                                     return_property="wet weight",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",67,
                                                         "R^2",0.95),
                                     reference=refs$EaDe1997),

           stop("unrecognized equation ID: ",id))
}


## wrapper function around alleq. Can override the taxon, if e.g. applying an equation developed for one species to another species
## can override reference
alleq_tbl <- function(id,taxon_name,taxon_aphia_id,notes,reference) {
    thiseq <- NULL
    try(thiseq <- alleq_XaCh2016(id),silent=TRUE)
    if (is.null(thiseq)) try(thiseq <- alleq_WiMc1990(id),silent=TRUE)
    if (is.null(thiseq)) try(thiseq <- alleq_Smal1993(id),silent=TRUE)
    if (is.null(thiseq)) try(thiseq <- alleq_Arti2003(id),silent=TRUE)
    if (is.null(thiseq)) try(thiseq <- alleq_Goeb2007(id),silent=TRUE)
    if (is.null(thiseq)) try(thiseq <- alleq_other(id),silent=TRUE)
    if (is.null(thiseq)) stop("equation id not recognized or has an error: ",id)

    ## use the equation defaults for some thing, if not already specified
    if (missing(taxon_name)) taxon_name <- thiseq$taxon_name
    if (missing(taxon_aphia_id)) taxon_aphia_id <- thiseq$taxon_aphia_id
    if (missing(reference)) reference <- thiseq$reference
    if (missing(notes)) {
        notes <- if (is.null(thiseq$notes)) "" else thiseq$notes
    }
    if (is.null(thiseq$reliability)) thiseq$reliability <- tibble(type=character(),value=numeric())

    ## old direct code
    ## tribble(~equation_id,~taxon_name,~taxon_aphia_id,~equation,~inputs,~return_property,~return_units,~reliability,~notes,~reference,
    ##        id,taxon_name,taxon_aphia_id,thiseq$equation,thiseq$inputs,thiseq$return_property,thiseq$return_units,thiseq$reliability,notes,reference)
    e2 <- sol_make_equation(equation_id=id,
                      taxon_name=taxon_name,
                      taxon_aphia_id=taxon_aphia_id,
                      equation=thiseq$equation,
                      inputs=thiseq$inputs,
                      return_property=thiseq$return_property,
                      return_units=thiseq$return_units,
                      reliability=thiseq$reliability,
                      notes=notes,
                      reference=reference,
                      check_packaged_ids=FALSE)
    class(e2) <- setdiff(class(e2),"sol_equation")
    e2
}

build_allometry_df <- function() {
    ## equations from Xavier and Cherel
    ## Ancistrocheirus lesueuri
    x <- bind_rows(alleq_tbl("138747_ML_Clar1986"),alleq_tbl("138747_WW_Clar1986"))

    ## Architeuthis dux
    x <- bind_rows(x,alleq_tbl("342218_ML_Clar1986"),alleq_tbl("342218_WW_Clar1986"),alleq_tbl("342218_ML_Roel2000",notes="Noted by Xavier & Cherel: this equation for mantle_length from LRL might be better than the Clarke (1986) one"))

    ## Bathyteuthis abyssicola
    x <- bind_rows(x,alleq_tbl("138848_ML_Clar1986"),alleq_tbl("138848_WW_Clar1986"))

    ## Batoteuthis skolops (no specific equations)
    x <- bind_rows(x,alleq_tbl("137777_ML_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                               notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"),
                   alleq_tbl("137777_WW_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                              notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"),
                   alleq_tbl("138168_ML_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                             notes="Based on Mastigoteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"),
                   alleq_tbl("138168_WW_Clar1986",taxon_name="Batoteuthis skolops",taxon_aphia_id=325293,
                             notes="Based on Mastigoteuthis spp. formula from Clarke (1986). There are no specific equations for Batoteuthis skolops"))

    ## family Brachioteuthidae
    x <- bind_rows(x,alleq_tbl("11758_ML_Clar1986"),alleq_tbl("11758_WW_Clar1986"))
    ## note that this will require some additional coding in the user function, so that e.g. an input taxon_name of "Brachioteuthis picta" will find its way to this equation even though it does not directly match the taxon_name or taxon_aphia_id specified here


    ## Chiroteuthis veranyi (no specific equations)
    x <- bind_rows(x,alleq_tbl("137777_ML_Clar1986",taxon_name="Chiroteuthis veranii",taxon_aphia_id=139125,
                               notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Chiroteuthis veranii"),
                   alleq_tbl("137777_WW_Clar1986",taxon_name="Chiroteuthis veranii",taxon_aphia_id=139125,
                             notes="Based on Chiroteuthis spp. formula from Clarke (1986). There are no specific equations for Chiroteuthis veranii"))

    ## Galiteuthis glacialis
    x <- bind_rows(x,alleq_tbl("325297_ML_LuWi1994"),alleq_tbl("325297_WW_LuWi1994"))

    ## Galiteuthis stC sp. (Imber) (no specific equations)
    ## Galiteuthis sp. 3 (Imber) (no specific equations)
    ## Taonius sp. B (Voss) (no specific equations)
    ## Taonius sp. (Clarke) (no specific equations)

    ## Taonius sp
    ## Note that this is given as Taonius pavo in Rodhouse et al. 1990, but Taonius sp. in X&C
    x <- bind_rows(x,alleq_tbl("137853_ML_Rodh1990"),alleq_tbl("137853_WW_Rodh1990"))

    ## Teuthowenia pellucida
    ## Note that the Rodhouse et al 1990 paper does NOT have equations for T.p.
    ## this info taken from X&C
    x <- bind_rows(x,alleq_tbl("341823_ML_Rodh1990"),alleq_tbl("341823_WW_Rodh1990"))

    ## Mesonychoteuthis hamiltoni
    x <- bind_rows(x,alleq_tbl("325299_ML_Rodh1990"))

    ## Cranchiidae
    x <- bind_rows(x,alleq_tbl("11774_WW_Clar1962"))

    ## Cycloteuthis akimushkini
    x <- bind_rows(x,alleq_tbl("341824_ML_Clar1986"),alleq_tbl("341824_WW_Clar1986"))

    ## Gonatus
    x <- bind_rows(x,alleq_tbl("138036_ML_Clar1986"),alleq_tbl("138036_WW_Clar1986"))
    ## Gonatus (small specimens)
    x <- bind_rows(x,alleq_tbl("138036small_ML_Clar1986"),alleq_tbl("138036small_WW_Clar1986"))

    ## Gonatus antarcticus (no specific equations)
    x <- bind_rows(x,alleq_tbl("138036_ML_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                               notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"),
                   alleq_tbl("138036_WW_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                             notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"))
    ## (small specimens)
    x <- bind_rows(x,alleq_tbl("138036small_ML_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                               notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"),
                   alleq_tbl("138036small_WW_Clar1986",taxon_name="Gonatus antarcticus",taxon_aphia_id=325300,
                             notes="Based on Gonatus spp. formula from Clarke (1986). There are no specific equations for Gonatus antarcticus"))

    ## Histioteuthis A, which has a deep notch in back of hood and a well-developed ridge
    ##  running to free corner of lateral wall:
    ## Histioteuthis arcturi (no specific equations)

    ## Histioteuthis bonnellii corpuscula (valid name Histioteuthis bonnellii)
    x <- bind_rows(x,alleq_tbl("140111_ML_Clar1986"),alleq_tbl("140111_ML_LuIc2002"),
                   alleq_tbl("140111_WW_LuIc2002"))

    ## Histioteuthis macrohista
    x <- bind_rows(x,alleq_tbl("341870_ML_LuIc2002"),alleq_tbl("341870_WW_LuIc2002"))

    ## Histioteuthis miranda
    x <- bind_rows(x,alleq_tbl("341871_ML_Clar1986"),alleq_tbl("341871_WW_Clar1986"))
    x <- bind_rows(x,alleq_tbl("341871_ML_LuIc2002"),alleq_tbl("341871_WW_LuIc2002"))

    ## Histioteuthis B has a shallow notch in back of hood and a weakly-developed ridge under
    ## the hood (evident in Histioteuthis atlantica juveniles) that becomes a slight fold running to free corner
    ## of lateral wall:

    ## Histioteuthis atlantica
    x <- bind_rows(x,alleq_tbl("341864_ML_LuIc2002"),alleq_tbl("341864_WW_LuIc2002"))

    ## Histioteuthis eltaninae
    x <- bind_rows(x,alleq_tbl("341867_ML_LuIc2002"),alleq_tbl("341867_WW_LuIc2002"))

    ## Lepidoteuthis grimaldii
    x <- bind_rows(x,alleq_tbl("140193_ML_BASUnpub"),alleq_tbl("140193_WW_BASUnpub"),alleq_tbl("140193_ML_LuIc2002"))

    ## Loligo gahi (accepted name Doryteuthis (Amerigo) gahi)
    x <- bind_rows(x,alleq_tbl("410351_ML_HatfUnpub"),alleq_tbl("410351_WW_HatfUnpub"))
    ## also with common name alternatives
    x <- bind_rows(x,alleq_tbl("410351_ML_HatfUnpub",taxon_name="Loligo gahi",taxon_aphia_id=341880,notes="Accepted taxon name is Doryteuthis (Amerigo) gahi"),
                   alleq_tbl("410351_WW_HatfUnpub",taxon_name="Loligo gahi",taxon_aphia_id=341880,notes="Accepted taxon name is Doryteuthis (Amerigo) gahi"))
    x <- bind_rows(x,alleq_tbl("410351_ML_HatfUnpub",taxon_name="Doryteuthis gahi",taxon_aphia_id=574538,notes="Accepted taxon name is Doryteuthis (Amerigo) gahi"),
                   alleq_tbl("410351_WW_HatfUnpub",taxon_name="Doryteuthis gahi",taxon_aphia_id=574538,notes="Accepted taxon name is Doryteuthis (Amerigo) gahi"))


    ## Lycoteuthis lorigera
    x <- bind_rows(x,alleq_tbl("342361_ML_LuIc2002"),alleq_tbl("342361_WW_LuIc2002"))

    ## Mastigoteuthis psychrophila
    x <- bind_rows(x,alleq_tbl("341904_ML_BASUnpub"),alleq_tbl("341904_WW_BASUnpub"))

    ## ?Mastigoteuthis A (Clarke) (no specific equations)

    ## Alluroteuthis antarcticus
    x <- bind_rows(x,alleq_tbl("325302_ML_Piat2001"),alleq_tbl("325302_WW_Piat2001"))

    ## Nototeuthis dimegacotyle (no specific equations)

    ## Taningia danae
    x <- bind_rows(x,alleq_tbl("140609_ML_Clark1986"),alleq_tbl("140609_WW_Clark1986"))

    ## Octopoteuthis sp.
    x <- bind_rows(x,alleq_tbl("138271_ML_Clark1986"),alleq_tbl("138271_WW_Clark1986"))

    ## Martialia hyadesi
    x <- bind_rows(x,alleq_tbl("325305_ML_RoYe1990"),alleq_tbl("325305_WW_RoYe1990"))

    ## Illex argentinus
    x <- bind_rows(x,alleq_tbl("342064_ML_SaHa2000"),alleq_tbl("342064_WW_SaHa2000"))

    ## Todarodes sp. (no specific equations)
    x <- bind_rows(x,alleq_tbl("138281_ML_Clar1986"),alleq_tbl("138281_WW_Clar1986"))

    ## Kondakovia longimana
    x <- bind_rows(x,alleq_tbl("325308_ML_BrKl1987"),alleq_tbl("325308_WW_BrKl1987"))

    ## Moroteuthis ingens
    x <- bind_rows(x,alleq_tbl("410381M_ML_Jack1995"),alleq_tbl("410381F_ML_Jack1995"),
                   alleq_tbl("410381M_WW_Jack1995"),alleq_tbl("410381F_WW_Jack1995"))

    ## Moroteuthis knipovitchi (valid name is Filippovia knipovitchi)
    x <- bind_rows(x,alleq_tbl("550403_ML_CherUnpub"),alleq_tbl("550403_WW_CherUnpub"))
    x <- bind_rows(x,alleq_tbl("550403_ML_CherUnpub",taxon_name="Moroteuthis knipovitchi",taxon_aphia_id=325310,notes="Accepted taxon name is Filippovia knipovitchi"),
                   alleq_tbl("550403_WW_CherUnpub",taxon_name="Moroteuthis knipovitchi",taxon_aphia_id=325310,notes="Accepted taxon name is Filippovia knipovitchi"))

    ## Moroteuthis robsoni (valid name Onykia robsoni)
    x <- bind_rows(x,alleq_tbl("410384_ML_LuIc2002"),alleq_tbl("410384_WW_LuIc2002"))
    x <- bind_rows(x,alleq_tbl("410384_ML_LuIc2002",taxon_name="Moroteuthis robsoni",taxon_aphia_id=342256,notes="Valid_name is Onykia robsoni"),
                   alleq_tbl("410384_WW_LuIc2002",taxon_name="Moroteuthis robsoni",taxon_aphia_id=342256,notes="Valid_name is Onykia robsoni"))

    ## Moroteuthis sp . B (Imber) (no specific equations)

    ## Onychoteuthis banksii
    x <- bind_rows(x,alleq_tbl("140649_ML_LuIc2002"),alleq_tbl("140649_WW_LuIc2002"))

    ## Pholidoteuthis massyae
    x <- bind_rows(x,alleq_tbl("410388_ML_Clar1986"),alleq_tbl("410388_WW_Clar1986"))

    ## Psychroteuthis glacialis
    x <- bind_rows(x,alleq_tbl("325312_ML_Grog2000"),alleq_tbl("325312_WW_Grog2000"))

    ## FAMILY SEPIOLIDAE
    ## cf. Stoloteuthis leucoptera (no allometric equations available)
    ## Oegopsida sp. A (Cherel) (no allometric equations available)

    ## Haliphron atlanticus
    x <- bind_rows(x,alleq_tbl("341781_WW_BASUnpub"))

    ## Cirrata sp. A (Cherel) (no allometric equations available)

    ## Pareledone turqueti
    x <- bind_rows(x,alleq_tbl("239393_ML_CollUnpub"),alleq_tbl("239393_WW_CollUnpub"))

    ## Adelieledone polymorpha
    x <- bind_rows(x,alleq_tbl("325319_ML_CollUnpub"),alleq_tbl("325319_WW_CollUnpub"))

    ## Benthoctopus thielei (valid name is Muusoctopus thielei)
    x <- bind_rows(x,alleq_tbl("884005_ML_CherUnpub"),alleq_tbl("884005_WW_CherUnpub"))
    x <- bind_rows(x,alleq_tbl("884005_ML_CherUnpub",taxon_name="Benthoctopus thielei",taxon_aphia_id=341931,notes="Accepted taxon name is Muusoctopus thielei"),
                   alleq_tbl("884005_WW_CherUnpub",taxon_name="Benthoctopus thielei",taxon_aphia_id=341931,notes="Accepted taxon name is Muusoctopus thielei"))

    ## Graneledone gonzalezi
    x <- bind_rows(x,alleq_tbl("342224_ML_CherUnpub"),alleq_tbl("342224_WW_CherUnpub"))

    ## Opisthoteuthis sp.
    ## ML=-26.0047+12.4858CL; logM=0.5893+0.2413CL (n= 13 for ML, n=9 for M) (Smale
    ## et al. 1993) where CL = Crest length (in mm)
    ## see equations for Smale et al. 1993

    ## Stauroteuthis gilchristi (no allometric equations available)

    ## ---
    ## Smale et al. 1993
    ## Octopus vulgaris
    x <- bind_rows(x,alleq_tbl("140605_ML_Smal1993"),
                   alleq_tbl("140605_HL_Smal1993"),
                   alleq_tbl("140605_WW_Smal1993"))

    ## Enteroctopus magnificus
    x <- bind_rows(x,alleq_tbl("535784_ML_Smal1993"),
                   alleq_tbl("535784_HL_Smal1993"),
                   alleq_tbl("535784_WW_Smal1993"))
    ## old name Octopus magnificus 225568
    x <- bind_rows(x,alleq_tbl("535784_ML_Smal1993",taxon_name="Octopus magnificus",taxon_aphia_id=225568,notes="Accepted taxon name is Enteroctopus magnificus"),
                   alleq_tbl("535784_HL_Smal1993",taxon_name="Octopus magnificus",taxon_aphia_id=225568,notes="Accepted taxon name is Enteroctopus magnificus"),
                   alleq_tbl("535784_WW_Smal1993",taxon_name="Octopus magnificus",taxon_aphia_id=225568,notes="Accepted taxon name is Enteroctopus magnificus"))

    ## Velodona togata
    x <- bind_rows(x,alleq_tbl("342415_ML_Smal1993"),
                   alleq_tbl("342415_HL_Smal1993"))

    ## Bathypolypus valdiviae
    x <- bind_rows(x,alleq_tbl("341910_ML_Smal1993"),
                   alleq_tbl("341910_HL_Smal1993"),
                   alleq_tbl("341910_WW_Smal1993"))

    ## Opisthoteuthis
    x <- bind_rows(x,alleq_tbl("138294_ML_Smal1993"),
                   alleq_tbl("138294_HL_Smal1993"),
                   alleq_tbl("138294_WW_Smal1993"))

    ## Tremoctopus violaceus
    x <- bind_rows(x,alleq_tbl("141694_ML_Smal1993"),
                   alleq_tbl("141694_HL_Smal1993"),
                   alleq_tbl("141694_WW_Smal1993"))

    ## Argonauta
    x <- bind_rows(x,alleq_tbl("137676_ML_Smal1993"),
                   alleq_tbl("137676_HL_Smal1993"),
                   alleq_tbl("137676_WW_Smal1993"))


    ## ---
    ## Williams & McEldowney 1990 otoliths
    ## Bathylagus antarcticus
    x <- bind_rows(x,alleq_tbl("234631_SL~OL_WiMc1990"),
                   alleq_tbl("234631_SL~OW_WiMc1990"),
                   alleq_tbl("234631_WW~SL_WiMc1990"))

    ## Krefftichthys anderssoni
    x <- bind_rows(x,alleq_tbl("234641_SL~OL_WiMc1990"),
                   alleq_tbl("234641_SL~OW_WiMc1990"),
                   alleq_tbl("234641_WW~SL_WiMc1990"))

    ## Pleuragramma antarctica
    x <- bind_rows(x,alleq_tbl("712788_SL~OL_WiMc1990"),
                   alleq_tbl("712788_SL~OW_WiMc1990"),
                   alleq_tbl("712788_WW~SL_WiMc1990"))
    ## used to be P.antarcticum, still a common misuse
    x <- bind_rows(x,alleq_tbl("712788_SL~OL_WiMc1990",taxon_name="Pleuragramma antarcticum",taxon_aphia_id=234721,notes="Accepted taxon name is Pleuragramma antarctica"),
                   alleq_tbl("712788_SL~OW_WiMc1990",taxon_name="Pleuragramma antarcticum",taxon_aphia_id=234721,notes="Accepted taxon name is Pleuragramma antarctica"),
                   alleq_tbl("712788_WW~SL_WiMc1990",taxon_name="Pleuragramma antarcticum",taxon_aphia_id=234721,notes="Accepted taxon name is Pleuragramma antarctica"))
    ## Protomyctophum bolini
    x <- bind_rows(x,alleq_tbl("234714_SL~OL_WiMc1990"),
                   alleq_tbl("234714_SL~OW_WiMc1990"),
                   alleq_tbl("234714_WW~SL_WiMc1990"))

    ## Electrona antarctica
    x <- bind_rows(x,alleq_tbl("217697_SL~OL_WiMc1990"),
                   alleq_tbl("217697_SL~OW_WiMc1990"),
                   alleq_tbl("217697_WW~SL_WiMc1990"))

    ## Electrona carlsbergi
    x <- bind_rows(x,alleq_tbl("234638_SL~OL_WiMc1990"),
                   alleq_tbl("234638_SL~OW_WiMc1990"),
                   alleq_tbl("234638_WW~SL_WiMc1990"))

    ## Electrona subaspera
    x <- bind_rows(x,alleq_tbl("234791_SL~OL_WiMc1990"),
                   alleq_tbl("234791_SL~OW_WiMc1990"),
                   alleq_tbl("234791_WW~SL_WiMc1990"))

    ## Gymnoscopelus braueri
    x <- bind_rows(x,alleq_tbl("234642_SL~OL_WiMc1990"),
                   alleq_tbl("234642_SL~OW_WiMc1990"),
                   alleq_tbl("234642_WW~SL_WiMc1990"))

    ## Gymnoscopelus nicholsi
    x <- bind_rows(x,alleq_tbl("234821_SL~OL_WiMc1990"),
                   alleq_tbl("234821_SL~OW_WiMc1990"),
                   alleq_tbl("234821_WW~SL_WiMc1990"))

    ## Gymnoscopelus fraseri
    x <- bind_rows(x,alleq_tbl("234657_SL~OL_WiMc1990"),
                   alleq_tbl("234657_SL~OW_WiMc1990"),
                   alleq_tbl("234657_WW~SL_WiMc1990"))

    ## Muraenolepis marmoratus, valid name Muraenolepis marmorata
    x <- bind_rows(x,alleq_tbl("313346_SL~OL_WiMc1990"),
                   alleq_tbl("313346_SL~OW_WiMc1990"),
                   alleq_tbl("313346_WW~SL_WiMc1990"))
    x <- bind_rows(x,alleq_tbl("313346_SL~OL_WiMc1990",taxon_name="Muraenolepis marmoratus",taxon_aphia_id=234705,notes="Accepted taxon name is Muraenolepis marmorata"),
                   alleq_tbl("313346_SL~OW_WiMc1990",taxon_name="Muraenolepis marmoratus",taxon_aphia_id=234705,notes="Accepted taxon name is Muraenolepis marmorata"),
                   alleq_tbl("313346_WW~SL_WiMc1990",taxon_name="Muraenolepis marmoratus",taxon_aphia_id=234705,notes="Accepted taxon name is Muraenolepis marmorata"))

    ## Macrourus holotrachys
    x <- bind_rows(x,alleq_tbl("234831_TL~OL_WiMc1990"),
                   alleq_tbl("234831_TL~OW_WiMc1990"),
                   alleq_tbl("234831_WW~TL_WiMc1990"))

    ## Zanclorhynchus spinifer
    x <- bind_rows(x,alleq_tbl("234683_SL~OL_WiMc1990"),
                   alleq_tbl("234683_SL~OW_WiMc1990"),
                   alleq_tbl("234683_WW~SL_WiMc1990"))

    ## Dolloidraco longedorsali
    x <- bind_rows(x,alleq_tbl("234696_SL~OL_WiMc1990"),
                   alleq_tbl("234696_SL~OW_WiMc1990"),
                   alleq_tbl("234696_WW~SL_WiMc1990"))



    ## Pogonophryne phyllopogon, valid name is Pogonophryne scotti
    x <- bind_rows(x,alleq_tbl("234837_SL~OL_WiMc1990"),
                   alleq_tbl("234837_SL~OW_WiMc1990"),
                   alleq_tbl("234837_WW~SL_WiMc1990"))

    x <- bind_rows(x,alleq_tbl("234837_SL~OL_WiMc1990",taxon_name="Pogonophryne phyllopogon",taxon_aphia_id=313416,notes="Accepted taxon name is Pogonophryne scotti"),
                   alleq_tbl("234837_SL~OW_WiMc1990",taxon_name="Pogonophryne phyllopogon",taxon_aphia_id=313416,notes="Accepted taxon name is Pogonophryne scotti"),
                   alleq_tbl("234837_WW~SL_WiMc1990",taxon_name="Pogonophryne phyllopogon",taxon_aphia_id=313416,notes="Accepted taxon name is Pogonophryne scotti"))

    ## Dissostichus eleginoides
    x <- bind_rows(x,alleq_tbl("234700_SL~OL_WiMc1990"),
                   alleq_tbl("234700_SL~OW_WiMc1990"),
                   alleq_tbl("234700_WW~SL_WiMc1990"))

    ## Dissostichus mawsoni
    x <- bind_rows(x,alleq_tbl("234836_SL~OL_WiMc1990"),
                   alleq_tbl("234836_SL~OW_WiMc1990"),
                   alleq_tbl("234836_WW~SL_WiMc1990"))

    ## Notothenia coriiceps
    x <- bind_rows(x,alleq_tbl("234679_SL~OL_WiMc1990"),
                   alleq_tbl("234679_SL~OW_WiMc1990"),
                   alleq_tbl("234679_WW~SL_WiMc1990"))

    ## Notothenia neglecta
    x <- bind_rows(x,alleq_tbl("234740_SL~OL_WiMc1990"),
                   alleq_tbl("234740_SL~OW_WiMc1990"),
                   alleq_tbl("234740_WW~SL_WiMc1990"))

    ## Notothenia rossii
    x <- bind_rows(x,alleq_tbl("234840_SL~OL_WiMc1990"),
                   alleq_tbl("234840_SL~OW_WiMc1990"),
                   alleq_tbl("234840_WW~SL_WiMc1990"))

    ## Notothenia kempi (now considered to be synonymous with L. squamifrons according to WoRMS, but enter both here
    ## N. kempi uses an "unaccepted" aphia_id here
    x <- bind_rows(x,alleq_tbl("313363_SL~OL_WiMc1990"),
                   alleq_tbl("313363_SL~OW_WiMc1990"),
                   alleq_tbl("313363_WW~SL_WiMc1990"))

    ## Lepidonotothen squamifrons
    x <- bind_rows(x,alleq_tbl("234788_SL~OL_WiMc1990"),
                   alleq_tbl("234788_SL~OW_WiMc1990"),
                   alleq_tbl("234788_WW~SL_WiMc1990"))

    ## Notothenia acuta
    x <- bind_rows(x,alleq_tbl("234624_SL~OL_WiMc1990"),
                   alleq_tbl("234624_SL~OW_WiMc1990"),
                   alleq_tbl("234624_WW~SL_WiMc1990"))

    ## Lepidonotothen mizops
    x <- bind_rows(x,alleq_tbl("234812_SL~OL_WiMc1990"),
                   alleq_tbl("234812_SL~OW_WiMc1990"),
                   alleq_tbl("234812_WW~SL_WiMc1990"))
    x <- bind_rows(x,alleq_tbl("234812_SL~OL_WiMc1990",taxon_name="Nototheniops mizops",taxon_aphia_id=313401,notes="Accepted taxon name is Lepidonotothen mizops"),
                   alleq_tbl("234812_SL~OW_WiMc1990",taxon_name="Nototheniops mizops",taxon_aphia_id=313401,notes="Accepted taxon name is Lepidonotothen mizops"),
                   alleq_tbl("234812_WW~SL_WiMc1990",taxon_name="Nototheniops mizops",taxon_aphia_id=313401,notes="Accepted taxon name is Lepidonotothen mizops"))

    ## Trematomus bernacchii
    x <- bind_rows(x,alleq_tbl("234802_SL~OL_WiMc1990"),
                   alleq_tbl("234802_SL~OW_WiMc1990"),
                   alleq_tbl("234802_WW~SL_WiMc1990"))
    x <- bind_rows(x,alleq_tbl("234802_SL~OL_WiMc1990",taxon_name="Pagothenia bernacchii",taxon_aphia_id=313391,notes="Accepted taxon name is Trematomus bernacchii"),
                   alleq_tbl("234802_SL~OW_WiMc1990",taxon_name="Pagothenia bernacchii",taxon_aphia_id=313391,notes="Accepted taxon name is Trematomus bernacchii"),
                   alleq_tbl("234802_WW~SL_WiMc1990",taxon_name="Pagothenia bernacchii",taxon_aphia_id=313391,notes="Accepted taxon name is Trematomus bernacchii"))

    ## Pagothenia borchgrevinki
    x <- bind_rows(x,alleq_tbl("234801_SL~OL_WiMc1990"),
                   alleq_tbl("234801_SL~OW_WiMc1990"),
                   alleq_tbl("234801_WW~SL_WiMc1990"))

    ## Trematomus hansoni
    x <- bind_rows(x,alleq_tbl("234772_SL~OL_WiMc1990"),
                   alleq_tbl("234772_SL~OW_WiMc1990"),
                   alleq_tbl("234772_WW~SL_WiMc1990"))
    x <- bind_rows(x,alleq_tbl("234772_SL~OL_WiMc1990",taxon_name="Pagothenia hansoni",taxon_aphia_id=313392,notes="Accepted taxon name is Trematomus hansoni"),
                   alleq_tbl("234772_SL~OW_WiMc1990",taxon_name="Pagothenia hansoni",taxon_aphia_id=313392,notes="Accepted taxon name is Trematomus hansoni"),
                   alleq_tbl("234772_WW~SL_WiMc1990",taxon_name="Pagothenia hansoni",taxon_aphia_id=313392,notes="Accepted taxon name is Trematomus hansoni"))

    ## Trematomus pennellii
    x <- bind_rows(x,alleq_tbl("234709_SL~OL_WiMc1990"),
                   alleq_tbl("234709_SL~OW_WiMc1990"),
                   alleq_tbl("234709_WW~SL_WiMc1990"))
    x <- bind_rows(x,alleq_tbl("234709_SL~OL_WiMc1990",taxon_name="Trematomus centronotus",taxon_aphia_id=313409,notes="Accepted taxon name is Trematomus pennellii"),
                   alleq_tbl("234709_SL~OW_WiMc1990",taxon_name="Trematomus centronotus",taxon_aphia_id=313409,notes="Accepted taxon name is Trematomus pennellii"),
                   alleq_tbl("234709_WW~SL_WiMc1990",taxon_name="Trematomus centronotus",taxon_aphia_id=313409,notes="Accepted taxon name is Trematomus pennellii"))

    ## Trematomus eulepidotus
    x <- bind_rows(x,alleq_tbl("234754_SL~OL_WiMc1990"),
                   alleq_tbl("234754_SL~OW_WiMc1990"),
                   alleq_tbl("234754_WW~SL_WiMc1990"))

    ## Trematomus lepidorhinus
    x <- bind_rows(x,alleq_tbl("234770_SL~OL_WiMc1990"),
                   alleq_tbl("234770_SL~OW_WiMc1990"),
                   alleq_tbl("234770_WW~SL_WiMc1990"))

    ## Trematomus loennbergii
    x <- bind_rows(x,alleq_tbl("234828_SL~OL_WiMc1990"),
                   alleq_tbl("234828_SL~OW_WiMc1990"),
                   alleq_tbl("234828_WW~SL_WiMc1990"))

    ## Trematomus newnesi
    x <- bind_rows(x,alleq_tbl("234628_SL~OL_WiMc1990"),
                   alleq_tbl("234628_SL~OW_WiMc1990"),
                   alleq_tbl("234628_WW~SL_WiMc1990"))

    ## Trematomus nicolai
    x <- bind_rows(x,alleq_tbl("234644_SL~OL_WiMc1990"),
                   alleq_tbl("234644_SL~OW_WiMc1990"),
                   alleq_tbl("234644_WW~SL_WiMc1990"))

    ## Trematomus scotti
    x <- bind_rows(x,alleq_tbl("234665_SL~OL_WiMc1990"),
                   alleq_tbl("234665_SL~OW_WiMc1990"),
                   alleq_tbl("234665_WW~SL_WiMc1990"))

    ## Akarotaxis nudiceps
    x <- bind_rows(x,alleq_tbl("234600_SL~OL_WiMc1990"),
                   alleq_tbl("234600_SL~OW_WiMc1990"),
                   alleq_tbl("234600_WW~SL_WiMc1990"))

    ## Bathydraco marri
    x <- bind_rows(x,alleq_tbl("234816_SL~OL_WiMc1990"),
                   alleq_tbl("234816_SL~OW_WiMc1990"),
                   alleq_tbl("234816_WW~SL_WiMc1990"))

    ## Cygnodraco mawsoni
    x <- bind_rows(x,alleq_tbl("234834_SL~OL_WiMc1990"),
                   alleq_tbl("234834_SL~OW_WiMc1990"),
                   alleq_tbl("234834_WW~SL_WiMc1990"))

    ## Gymnodraco acuticeps
    x <- bind_rows(x,alleq_tbl("234800_SL~OL_WiMc1990"),
                   alleq_tbl("234800_SL~OW_WiMc1990"),
                   alleq_tbl("234800_WW~SL_WiMc1990"))

    ## Prionodraco evansii
    x <- bind_rows(x,alleq_tbl("234777_SL~OL_WiMc1990"),
                   alleq_tbl("234777_SL~OW_WiMc1990"),
                   alleq_tbl("234777_WW~SL_WiMc1990"))

    ## Racovitzia glacialis
    x <- bind_rows(x,alleq_tbl("234799_SL~OL_WiMc1990"),
                   alleq_tbl("234799_SL~OW_WiMc1990"),
                   alleq_tbl("234799_WW~SL_WiMc1990"))

    ## Chaenodraco wilsoni
    x <- bind_rows(x,alleq_tbl("234609_SL~OL_WiMc1990"),
                   alleq_tbl("234609_SL~OW_WiMc1990"),
                   alleq_tbl("234609_WW~SL_WiMc1990"))

    ## Champsocephalus gunnari
    x <- bind_rows(x,alleq_tbl("234797_SL~OL_WiMc1990"),
                   alleq_tbl("234797_SL~OW_WiMc1990"),
                   alleq_tbl("234797_WW~SL_WiMc1990"))

    ## Channichthys rhinoceratus
    x <- bind_rows(x,alleq_tbl("234619_SL~OL_WiMc1990"),
                   alleq_tbl("234619_SL~OW_WiMc1990"),
                   alleq_tbl("234619_WW~SL_WiMc1990"))

    ## Chionodraco hamatus
    x <- bind_rows(x,alleq_tbl("234795_SL~OL_WiMc1990"),
                   alleq_tbl("234795_SL~OW_WiMc1990"),
                   alleq_tbl("234795_WW~SL_WiMc1990"))

    ## Chionodraco myersi
    x <- bind_rows(x,alleq_tbl("234646_SL~OL_WiMc1990"),
                   alleq_tbl("234646_SL~OW_WiMc1990"),
                   alleq_tbl("234646_WW~SL_WiMc1990"))

    ## Cryodraco antarcticus
    x <- bind_rows(x,alleq_tbl("234720_SL~OL_WiMc1990"),
                   alleq_tbl("234720_SL~OW_WiMc1990"),
                   alleq_tbl("234720_WW~SL_WiMc1990"))

    ## Paradiplospinus gracilis
    x <- bind_rows(x,alleq_tbl("219693_SL~OL_WiMc1990"),
                   alleq_tbl("219693_SL~OW_WiMc1990"))##,
    ##                   alleq_tbl("219693_WW~SL_WiMc1990"))
    warning("Paradiplospinus gracilis WW equation looks wrong, needs checking")


    ## ---
    ## Artigues 2003 equations

    x <- bind_rows(x,alleq_tbl("234661_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234661_WW~TL_Arti2003",taxon_name="Aethotaxis mitopteryx",taxon_aphia_id=234660,
                               notes="Accepted taxon name is Aethotaxis mitopteryx mitopteryx"))
    x <- bind_rows(x,alleq_tbl("234661M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234661M_WW~TL_Arti2003",taxon_name="Aethotaxis mitopteryx",taxon_aphia_id=234660,
                               notes="Accepted taxon name is Aethotaxis mitopteryx mitopteryx"))
    x <- bind_rows(x,alleq_tbl("234661F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234661F_WW~TL_Arti2003",taxon_name="Aethotaxis mitopteryx",taxon_aphia_id=234660,
                               notes="Accepted taxon name is Aethotaxis mitopteryx mitopteryx"))
    x <- bind_rows(x,alleq_tbl("234836_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234704_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234704M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234704F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234788_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234788M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234788F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234610_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234610M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234610F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234608_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234608M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234608F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712788_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712788M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712788F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234754_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234754M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234754F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234772_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234770_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234770M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234770F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234828_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234644_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234644M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234644F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234709_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234709M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234709F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234665_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234665M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234665F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712789_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712789_WW~TL_Arti2003",taxon_name="Artedidraco loennbergi",taxon_aphia_id=234615,
                               notes="Accepted taxon name is Artedidraco lonnbergi"))
    x <- bind_rows(x,alleq_tbl("712789M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712789M_WW~TL_Arti2003",taxon_name="Artedidraco loennbergi",taxon_aphia_id=234615,
                               notes="Accepted taxon name is Artedidraco lonnbergi"))
    x <- bind_rows(x,alleq_tbl("712789F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("712789F_WW~TL_Arti2003",taxon_name="Artedidraco loennbergi",taxon_aphia_id=234615,
                               notes="Accepted taxon name is Artedidraco lonnbergi"))
    x <- bind_rows(x,alleq_tbl("234592_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234592M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234592F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234817_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234817M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234817F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234621_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234621M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234621F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234696_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234696M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234696F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234790_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234826_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234730_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234730M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234730F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234611_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234837_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234816_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234816M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234816F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234834_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234834M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234834F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234808_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234808M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234808F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234800_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234800M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234800F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234777_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234777M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234777F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234799_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234799M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234799F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234720_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234720M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234720F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234725_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234725M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234725F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234609_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234797_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234797M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234797F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234650_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234650M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234650F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234795_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234795M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234795F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234646_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234646M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234646F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234678_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234678M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234678F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234614_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234685_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234796_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234695_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("217697_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234729_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234821_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234831_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234831M_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234831F_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234606_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("126486_WW~TL_Arti2003"))
    x <- bind_rows(x,alleq_tbl("234675_WW~TL_Arti2003"))

    ## ---
    ## Goebel et al. 2007
    x <- bind_rows(x,alleq_tbl("236217J_TL_Goeb2007"),
                   alleq_tbl("236217F_TL_Goeb2007"),
                   alleq_tbl("236217M_TL_Goeb2007"))

    ## ---
    ## Miscellaneous others
    ##
    ## Leptonychotes weddellii
    x <- bind_rows(x,alleq_tbl("195932_WW_GaBu1988"))

    ## Chorismus antarcticus
    x <- bind_rows(x,alleq_tbl("369214_WW_Lake2003"))
    ## also applicable to Notocrangon antarcticus
    x <- bind_rows(x,alleq_tbl("369214_WW_Lake2003",
                               taxon_name="Notocrangon antarcticus",
                               taxon_aphia_id=369204,
                               notes="Noted by Lake et al.: equation developed for Chorismus antarcticus but also applicable to Notocrangon antarcticus"))

    ## Tremtomus newnesi
    x <- bind_rows(x,alleq_tbl("234628_WW~SL_EaDe1997"))
    x
}

allometric_equations <- build_allometry_df()

## todo: check each row that taxon_name and taxon_aphia_id match (or are expected mismatches, at least)
## check worrms aphia_id
## unique rows

## check that all eqs return a tibble/df with a_v col at least
chk <- vapply(seq_len(nrow(allometric_equations)),function(z) {
    tmp <- allometric_equations[z,]$equation[[1]](0)
    is.data.frame(tmp) && "allometric_value" %in% names(tmp)},
   FUN.VALUE=TRUE)
assert_that(all(chk))

## more checks
assert_that(all(grepl("WW",allometric_equations$equation_id[allometric_equations$return_property=="wet weight"])))
assert_that(all(grepl("ML",allometric_equations$equation_id[allometric_equations$return_property=="mantle length"])))

## check that masses look ok
idx <- grepl("WiMc1990",allometric_equations$equation_id) & allometric_equations$return_property=="wet weight" & vapply(seq_len(nrow(allometric_equations)),function(z)allometric_equations[z,]$inputs[[1]]$property=="standard length",FUN.VALUE=TRUE)
tmp <- vapply(which(idx),function(z)allometric_equations[z,]$equation[[1]](100)$allometric_value,FUN.VALUE = 1)
assert_that(!any(abs(tmp)<1 | abs(tmp)>50))

## SL from OL
idx <- grepl("WiMc1990",allometric_equations$equation_id) & allometric_equations$return_property=="standard length" & vapply(seq_len(nrow(allometric_equations)),function(z)allometric_equations[z,]$inputs[[1]]$property=="otolith length",FUN.VALUE=TRUE)
tmp <- vapply(which(idx),function(z)allometric_equations[z,]$equation[[1]](4)$allometric_value,FUN.VALUE = 1)
assert_that(!any(abs(tmp)<50 | abs(tmp)>500))

## SL from OW
idx <- grepl("WiMc1990",allometric_equations$equation_id) & allometric_equations$return_property=="standard length" & vapply(seq_len(nrow(allometric_equations)),function(z)allometric_equations[z,]$inputs[[1]]$property=="otolith width",FUN.VALUE=TRUE)
tmp <- vapply(which(idx),function(z)allometric_equations[z,]$equation[[1]](4)$allometric_value,FUN.VALUE = 1)
assert_that(!any(abs(tmp)<50 | abs(tmp)>1000))

devtools::use_data(allometric_equations,internal=FALSE,overwrite=TRUE)
