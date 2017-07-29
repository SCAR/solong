globalVariables("sol_properties_data") # To make R CMD Check happy


#' Set or get the property name
#'
#' @param x vector: data
#' @param prop string: property name
#' @param with_units string: units of measurement to use. If missing, the default units for the property will be used
#' @param ... : extra arguments, currently ignored
#'
#' @return x with additional class set
#'
#' @seealso \code{\link{sol_properties}}
#'
#' @examples
#' x <- data.frame(LRL=c(11.3,13.9),species=c("Architeuthis dux"),
#'   stringsAsFactors=FALSE)
#' ## it doesn't matter what the column names are, but we
#' ## need to set the property types correctly
#' x$LRL <- sol_set_property(x$LRL,"lower rostral length")
#'
#' ## remove the property
#' x$LRL <- sol_set_property(x$LRL,NULL)
#'
#' @export
sol_set_property <- function(x,prop,with_units,...) {
    if (is.null(prop)) {
        ## remove property
        class(x) <- setdiff(class(x),c("sol_property",sol_properties()$class_name))
        strip_units(x)
    } else {
        thisprop <- sol_properties(prop)
        if (missing(with_units)) with_units <- thisprop$units
        units(x) <- ud_units[[with_units]]
        class(x) <- c("sol_property",thisprop$class_name,class(x))
        x
    }
}

#' @rdname sol_set_property
#' @export
sol_get_property <- function(x) {
    cls <- intersect(class(x),sol_properties()$class_name)
    sol_properties()$property[sol_properties()$class_name==cls]
}

#' Properties
#'
#' @param prop string: if provided, return only the property matching this name
#'
#' @return data.frame
#'
#' @seealso \code{\link{sol_set_property}}
#'
#' @examples
#' sol_properties() ## all properties that solong knows about
#'
#' @export
sol_properties <- function(prop) {
    if (missing(prop)) return(sol_properties_data)
    out <- sol_properties_data %>% filter_(~property==prop)
    if (nrow(out)==1) {
        out
    } else {
        stop("property ",prop," not recognized")
    }
}

## so as not to lose class info when subsetting
`[.sol_property` <- function(x,i,...) {
    cls <- intersect(class(x),sol_properties()$class_name)
    r <- NextMethod("[")
    class(r) <- c("sol_property",cls,class(r))
    r
}
