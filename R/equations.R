
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
    eqn <- allometric_equations %>%
        filter_(~equation_id==id)
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
## #' @method filter sol_equation
## #' @export
## filter.sol_equation <- function(.data,...) {
##     add_eq_class(NextMethod("filter"))
## }
## #' @method filter_ sol_equation
## #' @export
## filter_.sol_equation <- function(.data,...) {
##     add_eq_class(NextMethod("filter_"))
## }


#' @method print sol_equation
#' @export
print.sol_equation <- function(x,...) {
    jth <- function(j) switch(j,
                              "1"="1st",
                              "2"="2nd",
                              "3"="3rd",
                              paste0(j,"th"))
    rows_to_print <- round(getOption("max.print")/10)
    if (length(rows_to_print)<1 || is.null(rows_to_print) || is.na(rows_to_print)) rows_to_print <- 100
    rows_to_print <- min(rows_to_print,nrow(x))
    for (i in seq_len(rows_to_print)) {
        if ("equation_id" %in% names(x))
            cat("equation_id: ",x$equation_id[i],"\n",sep="")
        if ("taxon_name" %in% names(x))
            cat("  taxon_name: ",x$taxon_name[i],", taxon_aphia_id: ",x$taxon_aphia_id[i],"\n",sep="")

        if ("equation" %in% names(x))
            cat("  equation: ",paste(deparse(x$equation[i][[1]]),sep=" "),"\n",sep="")
        if ("inputs" %in% names(x)) {
            ips <- x$inputs[i][[1]]
            for (j in seq_len(nrow(ips)))
                cat("  It takes as ",jth(j)," input: ",ips$property[j]," (units: ",ips$units[j],")\n",sep="")
        }
        if ("return_property" %in% names(x)) {
            cat("  It estimates: ",x$return_property[i],sep="")
            if ("return_units" %in% names(x))
                cat(" (units: ",x$return_units[i],")",sep="")
            cat("\n")
        }
        if ("reliability" %in% names(x)) {
            rel <- x$reliability[i][[1]]
            if (nrow(rel)>0) {
                ind <- if (nrow(rel)>1) "Indicators" else "Indicator"
                cat("  ",ind," of reliability: ",paste(rel$type,rel$value,sep="=",collapse=", "),"\n",sep="")
            }
        }
        if ("notes" %in% names(x) && nzchar(x$notes[i]))
            cat("  Notes: ",x$notes[i],"\n",sep="")
        if ("reference" %in% names(x) && nzchar(x$reference[i]))
            cat("  Reference: ",x$reference[i],"\n",sep="")
        cat("\n")
    }
    nomit <- nrow(x)-rows_to_print
    if (nomit>0)
        cat("  [ reached getOption(\"max.print\") -- omitted ",nomit," row",if (nomit>1)"s"," ]\n",sep="")

}




#' Create an allometric equation object
#' 
#' @param equation_id string: a unique identifier for the equation (required)
#' @param taxon_name string: the taxon name that the equation applies to (required)
#' @param taxon_aphia_id numeric: the AphiaID of the taxon that the equation applies to (recommended)
#' @param equation function: the equation; see details (required)
#' @param inputs data.frame: the inputs needed by the equation; see details (required)
#' @param return_property string: the name of the allometric property that the equation returns (required)
#' @param return_units string: the units of measurement of the allometric property that the equation returns. Must be units that are recognized by units::ud_unit (required)
#' @param reliability data.frame: indicators of reliability of the equation; see details (recommended)
#' @param notes string: any notes that users should be aware of (optional)
#' @param reference string: the source of the equation (recommended)
#' @param warn_recommended logical: issue a warning if "recommended" informations is not supplied?
#'
#' @return equation object
#'
#' @seealso \code{\link{sol_equation}} \code{\link{sol_equations}}
#'
#' @examples
#' library(dplyr)
#' eq <- sol_make_equation(equation_id="my_equation_id",
#'                         taxon_name="Chorismus antarcticus",
#'                         taxon_aphia_id=369214,
#'                         equation=function(L)
#'                            tibble(allometric_value=0.000943*(L^2.976)),
#'                         inputs=tibble(property="carapace length",units="mm"),
#'                         return_property="mass",
#'                         return_units="g",
#'                         reliability=tribble(~type,~value,
#'                                              "N",35,
#'                                              "R^2",0.976),
#'                         reference="Lake S et al. (2003) doi:10.3354/meps254293")
#'
#' @export
sol_make_equation <- function(equation_id,taxon_name,taxon_aphia_id,equation,inputs,return_property,return_units,reliability,notes,reference,warn_recommended=TRUE) {
    assert_that(is.flag(warn_recommended))
    assert_that(is.string(equation_id))
    if (equation_id %in% sol_equations()$equation_id)
        warning("the supplied equation_id (\"",equation_id,"\") is already used by an equation in the solong package: consider using a different equation_id")
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
    chk <- vapply(inputs$units,function(z)is.null(ud_units[[z]]),FUN.VALUE=TRUE)
    if (any(chk))
        stop("input units (\"",paste(inputs$units[chk],collapse="\", \""),"\") are not recognized by units::ud_units")
    ## check that each input property is compatible with its units
    for (z in seq_len(nrow(inputs))) {
        if (!check_property_units(inputs$property[z],inputs$units[z])) {
            stop("input units (\"",inputs$units[z],"\") are not compatible with the input property (\"",
             inputs$property[z],"\") which has default units \"",
             sol_properties()$units[sol_properties()$property==inputs$property[z]],"\"")
        }
    }
    
    ## return property and units
    assert_that(is.string(return_property))
    chk <- sol_get_property(return_property)
    if (!is_sol_property(return_property))
        stop(sprintf("return property (\"%s\") not recognized",return_property))
    assert_that(is.string(return_units))
    chk <- ud_units[[return_units]]
    if (is.null(chk))
        stop("return_units (\"",return_units,"\") are not recognized by units::ud_units")
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
            stop("the inputs data.frame should have the columns \"type\" and \"value\"")
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
        reference <- ""
    } else {
        assert_that(is.string(reference))
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
