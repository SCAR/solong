#' Allometric equation data.
#'
#' A dataset containing allometric equations (relationships between
#' the size of an organism and the size of its body parts).
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{equation_id}{the identifier of this equation}
#'   \item{taxon_name}{name of the taxon}
#'   \item{taxon_aphia_id}{the Aphia ID of the taxon (identifier within the World Register of Marine Species)}
#'   \item{equation}{a function encoding the allometric equation}
#'   \item{inputs}{a data.frame specifying (in order) the inputs to the equation, with columns 'property' (the property name of the measurement needed, e.g. 'lower rostral length'), and 'units' (the units of the measurement needed, e.g. 'mm')}
#'   \item{return_property}{the name of the body size characteristic that is estimated by this equation (e.g. 'mantle length')}
#'   \item{return_units}{the units of measurement of the returned property}
#'   \item{reliability}{a data.frame with indicators of the reliability of the equation: type (a description of how the reliability was assessed, e.g. 'R^2' or 'N' the sample size used by the authors of the equation) and value (its value)}
#'   \item{notes}{notes}
#'   \item{reference}{the source of the equation}
#' }
"allometric_equations"

#' Properties relating to allometric equations.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{property}{the property name}
#'   \item{units}{its units of measurement}
#'   \item{class_name}{the corresponding class name used internally by the solong package}
#'   \item{notes}{notes, including a specific definition of the property if appropriate}
#' }
"sol_properties_data"
