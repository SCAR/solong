library(dplyr)
library(assertthat)

sol_properties_data <- tribble(~property,~units,~class_name,
                               "lower rostral length","mm","solprop_LRL",
                               "lower hood length","mm","solprop_LHL",
                               "hood length","mm","solprop_HL",
                               "crest length","mm","solprop_CL",
                               "mantle length","mm","solprop_ML",
                               "standard length","mm","solprop_SL",
                               "carapace length","mm","solprop_CPL",
                               "removed carapace length","mm","solprop_RCPL",
                               "total length","mm","solprop_TL",
                               "wet weight","g","solprop_WW",
                               "otolith length","mm","solprop_OL",
                               "otolith width","mm","solprop_OW")

assert_that(!any(duplicated(sol_properties_data$class_name)))
devtools::use_data(sol_properties_data,internal=FALSE,overwrite=TRUE)
