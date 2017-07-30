library(dplyr)
sol_properties_data <- tribble(~property,~units,~class_name,
                               "lower rostral length","mm","solprop_LRL",
                               "lower hood length","mm","solprop_LHL",
                               "hood length","mm","solprop_HL",
                               "crest length","mm","solprop_CL",
                               "mantle length","mm","solprop_ML",
                               "standard length","mm","solprop_SL",
                               "mass","g","solprop_mass",
                               "otolith length","mm","solprop_OL",
                               "otolith width","mm","solprop_OW")

devtools::use_data(sol_properties_data,internal=FALSE,overwrite=TRUE)
