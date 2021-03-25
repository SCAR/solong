library(dplyr)
library(assertthat)

sol_properties_data <- tribble(~property,~units,~class_name,~notes,
                               "lower rostral length","mm","solprop_LRL","",
                               "lower hood length","mm","solprop_LHL","",
                               "hood length","mm","solprop_HL","",
                               "crest length","mm","solprop_CL","",
                               "mantle length","mm","solprop_ML","",
                               "shell length", "mm", "solprop_SHL", "",
                               "standard length","mm","solprop_SL","",
                               "carapace length","mm","solprop_CPL","",
                               "removed carapace length","mm","solprop_RCPL","",
                               "anterior carapace width","mm","solprop_ACW","see e.g. Tapella & Lovrich 2006",
                               "maximum carapace width","mm","solprop_MCW","see e.g. Tapella & Lovrich 2006",
                               "rostrum basis width","mm","solprop_RBW","see e.g. Tapella & Lovrich 2006",
                               "rostrum length","mm","solprop_RL","see e.g. Tapella & Lovrich 2006",
                               "total length","mm","solprop_TL","Note that the definition of total length may be species-specific, e.g. for Antarctic krill it is generally taken from the anterior margin of the eye to the posterior tip of the telson; this was defined as 'Standard length 1' by Mauchline (1980)",
                               "pre-anal length","mm","solprop_PAL","",
                               "maximum total length", "mm", "solprop_maxTL", "",
                               "fork length","mm","solprop_FL","",
                               "dry weight","g","solprop_DW","",
                               "wet weight","g","solprop_WW","",
                               "carbon weight","g","solprop_CW","",
                               "lipid weight","g","solprop_LpW","",
                               "otolith length","mm","solprop_OL","",
                               "otolith width","mm","solprop_OW","",
                               "oral-atrial length","mm","solprop_OAL","Used for body length of salps, see e.g. Foxton P (1966) The distribution and life-history of Salpa thompsoni Foxton with observations on a related species, Salpa gerlachei Foxton. Discov Rep 34:1-116",
                               "eye diameter","mm","solprop_ED","",
                               ## energetics stuff
                               "ash-free dry weight", "g", "solprop_AFDW", "",
                               "energy density dry weight","kJ g-1","solprop_EDDW","",
                               "energy density wet weight","kJ g-1","solprop_EDWW","",
                               "lipid content dry weight","%","solprop_LpDW","",
                               "lipid content wet weight","%","solprop_LpWW","",
                               "shell-free dry weight", "g", "solprop_SFDW", "",
                               "water content wet weight","%","solprop_WCWW","",
                               "total energy content","kJ","solprop_TEC","",
                               "oxygen consumption rate", "ug h-1", "solprop_OCR", "",
                               ## growth curves
                               "age", "yr", "solprop_age", "")

assert_that(!any(duplicated(sol_properties_data$class_name)))
usethis::use_data(sol_properties_data, internal = FALSE, overwrite = TRUE)
