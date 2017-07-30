
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
