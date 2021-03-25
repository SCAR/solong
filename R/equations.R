
#' Allometric equations
#'
#' @param id string: the identifier of the equation to return
#'
#' @return data.frame
#'
#' @examples
#' sol_equations()
#' sol_equation("342218_ML_Roel2000")
#'
#' @export
sol_equation <- function(id) {
    assert_that(is.string(id))
    eqn <- dplyr::filter(allometric_equations, .data$equation_id == id)
    if (nrow(eqn)<1) {
        stop("no equations matching id: ",id)
    } else if (nrow(eqn)>1) {
        stop("multiple equations matching id: ",id)
    }
    add_eq_class(eqn)
}

#' @rdname sol_equation
#' @export
sol_equations <- function(id) {
    add_eq_class(allometric_equations)
}

add_eq_class <- function(z) {
    class(z) <- c("sol_equation",class(z))
    z
}

## so as not to lose class info when subsetting
#' @method "[" sol_equation
#' @export
`[.sol_equation` <- function(x,i,j,...) {
    add_eq_class(NextMethod("["))
}

## comment these out for now until figure out dplyr methods
## #' @importFrom dplyr filter
## #' @export
## filter.sol_equation <- function(.data,...) {
##     add_eq_class(NextMethod("filter"))
## }
## #' @method filter_ sol_equation
## #' @export
## filter_.sol_equation <- function(.data,...) {
##     add_eq_class(NextMethod("filter_"))
## }


#' @export
summary.sol_equation <- function(object,...) {
    jth <- function(j) switch(j,
                              "1"="1st",
                              "2"="2nd",
                              "3"="3rd",
                              paste0(j,"th"))
    na_unknown <- function(z) if (is.na(z)) "unknown" else z
    rows_to_print <- round(getOption("max.print")/10)
    if (length(rows_to_print)<1 || is.null(rows_to_print) || is.na(rows_to_print)) rows_to_print <- 100
    rows_to_print <- min(rows_to_print,nrow(object))
    for (i in seq_len(rows_to_print)) {
        if ("equation_id" %in% names(object))
            cat("equation_id: ",object$equation_id[i],"\n",sep="")
        if ("taxon_name" %in% names(object))
            cat("  taxon_name: ",object$taxon_name[i],", taxon_aphia_id: ",object$taxon_aphia_id[i],"\n",sep="")

        if ("equation" %in% names(object))
            cat("  equation: ",paste(deparse(object$equation[i][[1]]),sep=" "),"\n",sep="")
        if ("inputs" %in% names(object)) {
            ips <- object$inputs[i][[1]]
            for (j in seq_len(nrow(ips))) {
                cat("  It takes as ",jth(j)," input: ",ips$property[j]," (units: ",ips$units[j],", ",sep="")
                cat("sample range: ",na_unknown(ips$sample_minimum[j])," to ",na_unknown(ips$sample_maximum[j]),")\n",sep="")
            }
        }
        if ("return_property" %in% names(object)) {
            cat("  It estimates: ",object$return_property[i],sep="")
            if ("return_units" %in% names(object))
                cat(" (units: ",object$return_units[i],")",sep="")
            cat("\n")
        }
        if ("reliability" %in% names(object)) {
            rel <- object$reliability[i][[1]]
            if (nrow(rel)>0) {
                ind <- if (nrow(rel)>1) "Indicators" else "Indicator"
                cat("  ",ind," of reliability: ",paste(rel$type,rel$value,sep="=",collapse=", "),"\n",sep="")
            }
        }
        if ("notes" %in% names(object) && nzchar(object$notes[i]))
            cat("  Notes: ",object$notes[i],"\n",sep="")
        if ("reference" %in% names(object) && inherits(object$reference[[i]],"bibentry"))
            cat("  Reference: ",format(object$reference[[i]]),"\n",sep="")
        cat("\n")
    }
    nomit <- nrow(object)-rows_to_print
    if (nomit>0)
        cat("  [ reached getOption(\"max.print\") -- omitted ",nomit," row",if (nomit>1)"s"," ]\n",sep="")

}




#' Create an allometric equation object
#'
#' @param equation_id string: a unique identifier for the equation (required)
#' @param taxon_name string: the taxon name that the equation applies to (required)
#' @param taxon_aphia_id numeric: the AphiaID of the taxon that the equation applies to (recommended)
#' @param equation function: the equation. Must return a data.frame or tibble, with at least the column "allometric_value", and optionally also "allometric_value_lower" and "allometric_value_upper" (required)
#' @param inputs data.frame: the inputs needed by the equation. Must have columns "property" and "units", with entries that match those in \code{sol_properties}. Optionally also "sample_minimum" and "sample_maximum" if known (describing the range of the data used to generate the equation) (required)
#' @param return_property string: the name of the allometric property that the equation returns (required)
#' @param return_units string: the units of measurement of the allometric property that the equation returns. Will be parsed by units::as_units (required)
#' @param reliability data.frame: indicators of reliability of the equation. Must have columns "type" and "value"; see examples (recommended)
#' @param notes string: any notes that users should be aware of (optional)
#' @param reference bibentry: the source of the equation (recommended)
#' @param check_packaged_ids logical: if TRUE, check the equation_id against the package-bundled equations. A warning will be issued if there is a packaged equation with the same ID as equation_id
#' @param warn_recommended logical: issue a warning if "recommended" informations is not supplied?
#'
#' @return equation object
#'
#' @seealso \code{\link{sol_equation}} \code{\link{sol_equations}} \code{\link{sol_properties}}
#'
#' @examples
#' library(dplyr)
#' my_ref <- bibentry(bibtype="Article",key="Lake2003",
#'             author=c(person("S","Lake"),person("H","Burton"),
#'               person("J","van den Hoff")),
#'             year=2003,
#'             title="Regional, temporal and fine-scale spatial variation in
#'               Weddell seal diet at four coastal locations in east Antarctica",
#'             journal="Marine Ecology Progress Series",
#'             volume=254,pages="293-305",doi="10.3354/meps254293")
#'
#' eq <- sol_make_equation(equation_id="my_equation_id",
#'                         taxon_name="Chorismus antarcticus",
#'                         taxon_aphia_id=369214,
#'                         equation=function(L)
#'                            tibble(allometric_value=0.000943*(L^2.976)),
#'                         inputs=tibble(property="carapace length",units="mm",
#'                                       sample_minimum=6,sample_maximum=16),
#'                         return_property="wet weight",
#'                         return_units="g",
#'                         reliability=tribble(~type,~value,
#'                                              "N",35,
#'                                              "R^2",0.976),
#'                         reference=my_ref)
#'
#' @export
sol_make_equation <- function(equation_id,taxon_name,taxon_aphia_id,equation,inputs,return_property,return_units,reliability,notes,reference,check_packaged_ids=TRUE,warn_recommended=TRUE) {
    assert_that(is.flag(warn_recommended))
    assert_that(is.string(equation_id))
    if (check_packaged_ids) {
    if (equation_id %in% sol_equations()$equation_id)
        warning("the supplied equation_id (\"",equation_id,"\") is already used by an equation in the solong package: consider using a different equation_id")
    }
    assert_that(is.string(taxon_name))
    if (missing(taxon_aphia_id)) {
        if (warn_recommended) warning("no taxon_aphia_id provided: consider adding this")
        taxon_aphia_id <- NA_integer_
    } else {
        assert_that(is.numeric(taxon_aphia_id))
    }
    assert_that(is.function(equation))

    ## inputs
    assert_that(is.data.frame(inputs))
    if (!all(c("property","units") %in% names(inputs)))
        stop("the inputs data.frame should have the columns \"property\" and \"units\"")
    assert_that(is.character(inputs$property))
    is_sol_property <- function(z) z %in% sol_properties()$property
    chk <- vapply(inputs$property,is_sol_property,FUN.VALUE=TRUE)
    if (!all(chk))
        stop(sprintf("input property not recognized: \"%s\"",paste(inputs$property,collapse="\", \"")))
    assert_that(is.character(inputs$units))
    chk <- vapply(inputs$units,function(z)is.null(as_units(z)),FUN.VALUE=TRUE)
    if (any(chk))
        stop("input units (\"",paste(inputs$units[chk],collapse="\", \""),"\") cannot be parsed by units::as_units")
    ## check that each input property is compatible with its units
    for (z in seq_len(nrow(inputs))) {
        if (!check_property_units(inputs$property[z],inputs$units[z])) {
            stop("input units (\"",inputs$units[z],"\") are not compatible with the input property (\"",
             inputs$property[z],"\") which has default units \"",
             sol_properties()$units[sol_properties()$property==inputs$property[z]],"\"")
        }
    }
    if (!"sample_minimum" %in% names(inputs)) inputs$sample_minimum <- rep(NA, nrow(inputs) > 0)
    if (!"sample_maximum" %in% names(inputs)) inputs$sample_maximum <- rep(NA, nrow(inputs) > 0)

    ## return property and units
    assert_that(is.string(return_property))
    chk <- sol_get_property(return_property)
    if (!is_sol_property(return_property))
        stop(sprintf("return property (\"%s\") not recognized",return_property))
    assert_that(is.string(return_units))
    chk <- as_units(return_units)
    if (is.null(chk))
        stop("return_units (\"",return_units,"\") cannot be parsed by units::as_units")
    ## check that the return_property is compatible with the return_units
    if (!check_property_units(return_property,return_units)) {
        stop("return_units (\"",return_units,"\") are not compatible with the return_property (\"",
             return_property,"\") which has default units \"",
             sol_properties()$units[sol_properties()$property==return_property],"\"")
    }

    ## reliability
    if (missing(reliability)) {
        if (warn_recommended) warning("no reliability information provided: consider adding this to help guide users in the use of the equation")
        reliability <- tibble(type=character(),value=character())
    } else {
        assert_that(is.data.frame(reliability))
        if (!all(c("type","value") %in% names(reliability)))
            stop("the reliability data.frame should have the columns \"type\" and \"value\"")
        assert_that(is.character(reliability$type))
        ##assert_that(is.numeric(reliability$value))
    }

    ## notes
    if (missing(notes)) {
        notes <- ""
    } else {
        assert_that(is.string(notes))
    }

    ## reference
    if (missing(reference)) {
        if (warn_recommended) warning("no reference provided: consider adding this to help establish the provenance of the equation")
        reference <- NULL
    } else {
        assert_that(inherits(reference,"bibentry"))
    }

    ## test the equation
    chkout <- NULL
    invar <- rep(list(1),nrow(inputs))
    tryCatch(chkout <- do.call(equation,invar),
             error=function(e) {
                 warning("the supplied equation failed with unit inputs")
             })
    if (!is.null(chkout)) {
        if (!is.data.frame(chkout)) stop("the equation should return a data.frame or tibble object")
        if (!"allometric_value" %in% names(chkout)) stop("the returned data.frame should have a column named \"allometric_value\"")
    }
    eqn <- tribble(~equation_id,~taxon_name,~taxon_aphia_id,~equation,~inputs,~return_property,~return_units,~reliability,~notes,~reference,
                   equation_id,taxon_name,taxon_aphia_id,equation,inputs,return_property,return_units,reliability,notes,reference)
    add_eq_class(eqn)
}
