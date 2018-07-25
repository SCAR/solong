refs$Dubi2012 <- bibentry(bibtype = "Article",key = "Dubi2012",
                          author = c(person(c("C","D"),"Dubischar"),person(c("E","A"),"Pakhomov"),person(c("L"),"von Harbou"),person(c("B","P","V"),"Hunt"),person(c("U","V"),"Bathmann")),
                          year = 2012,title = "Salps in the Lazarev Sea, Southern Ocean: II. Biochemical composition and potential prey value",
                          journal = "Marine Biology",
                          volume = 159,pages = "15-24",doi = "10.1007/s00227-011-1785-5")

if (FALSE) {
    ## refit some equations because the published ones appear to be incorrect
    library(dplyr)
    library(readxl)
    x <- read_xls("../../data/dubischar/Data Dubischar et al 2012.xls")
    names(x) <- sub("_$", "", gsub("\\.+", "_", make.names(tolower(names(x)))))

    ## autumn WW~OAL equations
    ## Ihlea
    myx <- x %>% dplyr::filter(season == "Fall 2004" & salp_species == "I. racovitzai" & oal_mm < 42)
    fit <- lm(log(ww_ind_1_mg) ~ log(oal_mm), data = myx)
    a <- coefficients(fit)[1] ## -3.911235
    b <- coefficients(fit)[2] ## 2.785379
    df_residual <- fit$df.residual ## 9
    s2_residual <- sum(fit$residuals^2) ## 0.3130059
    V <- vcov(fit) ## matrix(c(2.9891509, -0.8306561, -0.8306561, 0.2310757), nrow = 2, byrow = TRUE)
    r2 <- summary(fit)$r.squared ## 0.789
    N <- nrow(myx) ## 11
    ## Salpa
    myx <- x %>% dplyr::filter(season == "Fall 2004" & salp_species == "S. thompsoni")
    fit <- lm(log(ww_ind_1_mg) ~ log(oal_mm), data = myx)
    a <- coefficients(fit)[1] ## -0.8037035
    b <- coefficients(fit)[2] ## 2.152319
    r2 <- summary(fit)$r.squared ## 0.885
    N <- nrow(myx) ## 30

    ## CW~OAL summer
    ## for the two summer equations, the printed curves in the paper do not match the equations given
    ## Ihlea
    myx <- x %>% dplyr::filter(season == "Summer 2006" & salp_species == "I. racovitzai")
    fit <- lm(log(c_mg_ind_1) ~ log(oal_mm), data = myx)
    a <- coefficients(fit)[1] ## -6.341833
    b <- coefficients(fit)[2] ## 2.239424
    r2 <- summary(fit)$r.squared ## 0.883
    N <- nrow(myx) ## 62
    ## Salpa
    myx <- x %>% dplyr::filter(season == "Summer 2006" & salp_species == "S. thompsoni")
    fit <- lm(log(c_mg_ind_1) ~ log(oal_mm), data = myx)
    a <- coefficients(fit)[1] ## -5.254129
    b <- coefficients(fit)[2] ## 1.855174
    r2 <- summary(fit)$r.squared ## 0.947
    N <- nrow(myx) ## 73

    ## CW~OAL winter
    ## Ihlea
    myx <- x %>% dplyr::filter(season == "Winter 2006" & salp_species == "I. racovitzai")
    fit <- lm(log(c_mg_ind_1) ~ log(oal_mm), data = myx)
    a <- coefficients(fit)[1] ## -8.395922
    b <- coefficients(fit)[2] ## 2.719844
    r2 <- summary(fit)$r.squared ## 0.981
    N <- nrow(myx) ## 59
    ## Salpa
    myx <- x %>% dplyr::filter(season == "Winter 2006" & salp_species == "S. thompsoni")
    fit <- lm(log(c_mg_ind_1) ~ log(oal_mm), data = myx)
    a <- coefficients(fit)[1] ## -4.380123
    b <- coefficients(fit)[2] ## 1.578182
    r2 <- summary(fit)$r.squared ## 0.693
    N <- nrow(myx) ## 17

    ## lipid weight vs OAL
    myx <- tibble(OAL = c(60, 40, 30, 35, 55, 31, 54, 55, 40, 30, 24, 32, 9, 8, 56, 65, 9, 33, 33, 52, 50, 44, 47, 18, 27, 18, 27, 26, 25, 20, 20, 24, 26, 25, 50, 60, 65, 57, 28, 39, 34),
                  LW = c(2.178, 1.170, 0.413, 1.555, 2.795, 0.628, 1.492, 1.898, 1.573, 0.995, 0.679, 0.734, 0.318, 0.167, 2.841, 4.462, 0.065, 1.073, 0.585, 1.825, 3.925, 2.750, 3.100, 0.275, 1.325, 0.280, 0.850, 0.950, 0.750, 0.575, 0.380, 0.950, 0.750, 1.150, 4.080, 3.513, 4.811, 2.413, 0.972, 2.005, 0.749))
    fit <- lm(log(LW) ~ log(OAL), data = myx)
    a <- coefficients(fit)[1] ## -5.606497
    b <- coefficients(fit)[2] ## 1.63873
    r2 <- summary(fit)$r.squared ## 0.824
    N <- nrow(myx) ## 41

}

alleq_Dubi2012 <- function(id) {
    switch(id,
           ## WW~OAL summer
           ## Ihlea racovitzai 266542
           "266542_WW~OAL_summer_Dubi2012" = list(taxon_name = "Ihlea racovitzai",
                                                taxon_aphia_id = 266542,
                                                equation = function(OAL) tibble(allometric_value = 0.008 * (OAL ^ 3.092)),
                                                inputs = tibble(property = "oral-atrial length",units = "mm"),
                                                return_property = "wet weight",
                                                return_units = "mg",
                                                reliability = tribble(~type,~value,
                                                                    "R^2",0.809),
                                                notes = "From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
                                                reference = refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_WW~OAL_summer_Dubi2012" = list(taxon_name = "Salpa thompsoni",
                                                taxon_aphia_id = 266545,
                                                equation = function(OAL) tibble(allometric_value = 0.136*(OAL ^ 2.376)),
                                                inputs = tibble(property = "oral-atrial length",units = "mm"),
                                                return_property = "wet weight",
                                                return_units = "mg",
                                                reliability = tribble(~type,~value,
                                                                    "R^2",0.988),
                                                notes = "From specimens caught during a summer cruise, Dec-2005 to Jan-2006",
                                                reference = refs$Dubi2012),
           ## WW~OAL autumn
           ## Ihlea racovitzai 266542
           ## refitted eq
           "266542_WW~OAL_autumn_Dubi2012" = list(taxon_name = "Ihlea racovitzai",
                                                  taxon_aphia_id = 266542,
                                                  equation = function(OAL) tibble(allometric_value = exp(-3.911235) * (OAL ^ 2.785379)),
                                                  inputs = tibble(property = "oral-atrial length",units = "mm"),
                                                  return_property = "wet weight",
                                                  return_units = "mg",
                                                  reliability = tribble(~type,~value,
                                                                        "N", 11,
                                                                        "R^2",0.816),
                                                notes = "From specimens caught during a summer cruise, Dec-2005 to Jan-2006. Equation refitted from original data, because published equation appears to be incorrect",
                                                reference = refs$Dubi2012),
           ## Salpa thompsoni 266545
           ## refitted eq
           "266545_WW~OAL_autumn_Dubi2012" = list(taxon_name = "Salpa thompsoni",
                                                taxon_aphia_id = 266545,
                                                equation = function(OAL) tibble(allometric_value = exp(-0.8037035) * (OAL ^ 2.152319)),
                                                inputs=tibble(property = "oral-atrial length", units = "mm"),
                                                return_property = "wet weight",
                                                return_units = "mg",
                                                reliability=tribble(~type, ~value,
                                                                    "N", 30,
                                                                    "R^2", 0.885),
                                                notes = "From specimens caught during an autumn cruise, Apr-May 2004. Equation refitted from original data, because published equation appears to be incorrect",
                                                reference = refs$Dubi2012),
           ## WW~OAL winter
           ## Ihlea racovitzai 266542
           "266542_WW~OAL_winter_Dubi2012" = list(taxon_name = "Ihlea racovitzai",
                                                taxon_aphia_id = 266542,
                                                equation = function(OAL) tibble(allometric_value = 0.057 * (OAL ^ 2.462)),
                                                inputs = tibble(property = "oral-atrial length", units = "mm"),
                                                return_property = "wet weight",
                                                return_units = "mg",
                                                reliability = tribble(~type, ~value,
                                                                    "R^2", 0.875),
                                                notes = "From specimens caught during a winter cruise, Jul-Aug 2006",
                                                reference = refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_WW~OAL_winter_Dubi2012" = list(taxon_name = "Salpa thompsoni",
                                                taxon_aphia_id = 266545,
                                                equation = function(OAL) tibble(allometric_value = 0.011 * (OAL ^ 3.049)),
                                                inputs = tibble(property = "oral-atrial length", units = "mm"),
                                                return_property = "wet weight",
                                                return_units = "mg",
                                                reliability = tribble(~type, ~value,
                                                                    "R^2", 0.98),
                                                notes = "From specimens caught during a winter cruise, Jul-Aug 2006",
                                                reference = refs$Dubi2012),

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
           "266542_CW~OAL_summer_Dubi2012" = list(taxon_name = "Ihlea racovitzai",
                                                taxon_aphia_id = 266542,
                                                equation = function(OAL) tibble(allometric_value = exp(-6.341833) * (OAL ^ 2.239424)),
                                                inputs = tibble(property = "oral-atrial length", units = "mm"),
                                                return_property = "carbon weight",
                                                return_units = "mg",
                                                reliability = tribble(~type, ~value,
                                                                      "N", 62,
                                                                      "R^2", 0.883),
                                                notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006. Equation refitted from original data, because published equation appears to be incorrect",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           "266545_CW~OAL_summer_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(OAL) tibble(allometric_value = exp(-5.254129) * (OAL ^ 1.855174)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="carbon weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "N", 73,
                                                                    "R^2",0.947),
                                                notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006. Equation refitted from original data, because published equation appears to be incorrect",
                                                reference=refs$Dubi2012),
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
           "266542_CW~OAL_winter_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                                taxon_aphia_id=266542,
                                                equation=function(OAL) tibble(allometric_value = exp(-8.395922) * (OAL ^ 2.719844)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),
                                                return_property="carbon weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "N", 59,
                                                                    "R^2",0.981),
                                                notes="From specimens caught during a winter cruise, Jul-Aug 2006. Equation refitted from original data, because published equation appears to be incorrect",
                                                reference=refs$Dubi2012),
           ## Salpa thompsoni 266545
           ## the equation here does not match the printed curve
           "266545_CW~OAL_winter_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                                taxon_aphia_id=266545,
                                                equation=function(OAL) tibble(allometric_value = exp(-4.380123) * (OAL ^ 1.578182)),
                                                inputs=tibble(property="oral-atrial length",units="mm"),##,sample_minimum=***,sample_maximum=***),
                                                return_property="carbon weight",
                                                return_units="mg",
                                                reliability=tribble(~type,~value,
                                                                    "N", 17,
                                                                    "R^2",0.693),
                                                notes="From specimens caught during a winter cruise, Jul-Aug 2006. Equation refitted from original data, because published equation appears to be incorrect",
                                                reference=refs$Dubi2012),

           ## all specimens, all seasons: lipid weight~OAL
           ## printed curve does not match equation given
           "137217_LpW~OAL_Dubi2012"=list(taxon_name="Salpidae",
                                          taxon_aphia_id=137217,
                                          equation=function(OAL) tibble(allometric_value = exp(-5.606497) * (OAL ^ 1.63873)),
                                          inputs = tibble(property = "oral-atrial length", units = "mm", sample_minimum = 8, sample_maximum = 65),
                                          return_property="lipid weight",
                                          return_units="mg",
                                          reliability=tribble(~type,~value,
                                                              "N",41,
                                                              "R^2",0.824),
                                          notes="Based on specimens of Salpa thompsoni and Ihlea racovitzai. Equation refitted from original data, because published equation appears to be incorrect",
                                          reference=refs$Dubi2012),

           ## convert wet weight to energetic content
           ## Lipids correspond to 9.3 kcal/g = 9.3*4.2 kJ/g, proteins to 4.1 kcal/g = 4.2*4.2 kJ/g and carbohydrates to 4.1 kcal/g = 4.1*4.2 kJ/g
           ## summer
           "266542_EC~WW_summer_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                               taxon_aphia_id=266542,
                                               equation=function(...) {
                                                   dw <- 0.065*...
                                                   carbs <- 0.012*dw; prot <- 0.323*dw; Lp <- 0.035*dw
                                                   tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                               inputs=tibble(property="wet weight",units="g"),
                                               return_property="total energy content",
                                               return_units="kJ",
                                               reliability=tribble(~type,~value,
                                                                   "N",12),
                                               notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006. Lipid fraction based on measurements of 12 specimens, other components based on measurements of more specimens",
                                               reference=refs$Dubi2012),

           "266545_EC~WW_summer_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                               taxon_aphia_id=266545,
                                               equation=function(...) {
                                                   dw <- 0.075*...
                                                   carbs <- 0.028*dw; prot <- 0.092*dw; Lp <- 0.038*dw
                                                   tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                               inputs=tibble(property="wet weight",units="g"),
                                               return_property="total energy content",
                                               return_units="kJ",
                                               reliability=tribble(~type,~value,
                                                                   "N",4),
                                               notes="From specimens caught during a summer cruise, Dec-2005 to Jan-2006. Lipid, protein, carbohydrate fractions based on measurements of 4 specimens, dry weight based on measurements of 21 specimens",
                                               reference=refs$Dubi2012),
           ## autumn
           "266542_EC~WW_autumn_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                               taxon_aphia_id=266542,
                                               equation=function(...) {
                                                   dw <- 0.068*...
                                                   carbs <- 0.022*dw; prot <- 0.302*dw; Lp <- 0.043*dw
                                                   tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                               inputs=tibble(property="wet weight",units="g"),
                                               return_property="total energy content",
                                               return_units="kJ",
                                               reliability=tribble(~type,~value,
                                                                   "N",5),
                                               notes="From specimens caught during an autumn cruise, Apr-May 2004. Lipid fraction based on measurements of 5 specimens, other components based on measurements of more specimens",
                                               reference=refs$Dubi2012),

           "266545_EC~WW_autumn_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                               taxon_aphia_id=266545,
                                               equation=function(...) {
                                                   dw <- 0.055*...
                                                   carbs <- 0.02*dw; prot <- 0.102*dw; Lp <- 0.024*dw
                                                   tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                               inputs=tibble(property="wet weight",units="g"),
                                               return_property="total energy content",
                                               return_units="kJ",
                                               reliability=tribble(~type,~value,
                                                                   "N",12),
                                               notes="From specimens caught during an autumn cruise, Apr-May 2004. Lipid fraction based on measurements of 12 specimens, other components based on measurements of more specimens",
                                               reference=refs$Dubi2012),
           ## winter
           "266542_EC~WW_winter_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                               taxon_aphia_id=266542,
                                               equation=function(...) {
                                                   dw <- 0.058*...
                                                   carbs <- 0.013*dw; prot <- 0.317*dw; Lp <- 0.031*dw
                                                   tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                               inputs=tibble(property="wet weight",units="g"),
                                               return_property="total energy content",
                                               return_units="kJ",
                                               reliability=tribble(~type,~value,
                                                                   "N",4),
                                               notes="From specimens caught during a winter cruise, Jul-Aug 2006. Lipid fraction based on measurements of 4 specimens, other components based on measurements of more specimens",
                                               reference=refs$Dubi2012),

           "266545_EC~WW_winter_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                               taxon_aphia_id=266545,
                                               equation=function(...) {
                                                   dw <- 0.062*...
                                                   carbs <- 0.02*dw; prot <- 0.113*dw; Lp <- 0.036*dw
                                                   tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                               inputs=tibble(property="wet weight",units="g"),
                                               return_property="total energy content",
                                               return_units="kJ",
                                               reliability=tribble(~type,~value,
                                                                   "N",1),
                                               notes="From specimens caught during a winter cruise, Jul-Aug 2006. Carbohydrate and protein fractions based on measurements of 1 specimen, other components based on measurements of more specimens",
                                               reference=refs$Dubi2012),
           ## all seasons combined
           "266542_EC~WW_Dubi2012"=list(taxon_name="Ihlea racovitzai",
                                        taxon_aphia_id=266542,
                                        equation=function(...) {
                                            dw <- 0.062*...
                                            carbs <- 0.014*dw; prot <- 0.316*dw; Lp <- 0.036*dw
                                            tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)}, ##ww*0.062*4.2*(9.3*0.036+4.2*0.316+4.1*0.014) = ww*0.4477
                                        inputs=tibble(property="wet weight",units="g"),
                                        return_property="total energy content",
                                        return_units="kJ",
                                        reliability=tribble(~type,~value,
                                                                "N",21),
                                        notes="Combined across summer, autumn, and winter samples. Lipid fraction based on measurements of 21 specimens, other components based on measurements of more specimens",
                                        reference=refs$Dubi2012),

           "266545_EC~WW_Dubi2012"=list(taxon_name="Salpa thompsoni",
                                        taxon_aphia_id=266545,
                                        equation=function(...) {
                                            dw <- 0.063*...
                                            carbs <- 0.021*dw; prot <- 0.101*dw; Lp <- 0.029*dw
                                            tibble(allometric_value=9.3*4.2*Lp+4.2*4.2*prot+4.1*4.2*carbs)},
                                            inputs=tibble(property="wet weight",units="g"),
                                            return_property="total energy content",
                                            return_units="kJ",
                                            reliability=tribble(~type,~value,
                                                              "N",19),
                                          notes="Combined across summer, autumn, and winter samples. Lipid fraction based on measurements of 19 specimens, other components based on measurements of more specimens",
                                          reference=refs$Dubi2012),


           stop("unrecognized equation ID: ",id))
}
