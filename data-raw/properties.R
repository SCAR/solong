library(dplyr)
library(assertthat)

sol_properties_data <- tribble(~property,~units,~class_name,~notes,
                               "lower rostral length","mm","solprop_LRL","",
                               "lower hood length","mm","solprop_LHL","",
                               "hood length","mm","solprop_HL","",
                               "crest length","mm","solprop_CL","",
                               "mantle length","mm","solprop_ML","",
                               "standard length","mm","solprop_SL","",
                               "carapace length","mm","solprop_CPL","",
                               "removed carapace length","mm","solprop_RCPL","",
                               "total length","mm","solprop_TL","Note that the definition of total length may be species-specific, e.g. for Antarctic krill it is generally taken from the anterior margin of the eye to the posterior tip of the telson",
                               "fork length","mm","solprop_FL","",
                               "dry weight","g","solprop_DW","",
                               "wet weight","g","solprop_WW","",
                               "otolith length","mm","solprop_OL","",
                               "otolith width","mm","solprop_OW","",
                               ## energetics stuff
                               "energy density dry weight","kJ g-1","solprop_EDDW","",
                               "energy density wet weight","kJ g-1","solprop_EDWW","",
                               "lipid content dry weight","%","solprop_LpDW","",
                               "lipid content wet weight","%","solprop_LpWW","",
                               "water content wet weight","%","solprop_WC","")

assert_that(!any(duplicated(sol_properties_data$class_name)))
devtools::use_data(sol_properties_data,internal=FALSE,overwrite=TRUE)
