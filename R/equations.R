
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

`[.sol_equation` <- function(x,i,j,...) {
    add_eq_class(NextMethod("["))
}

filter.sol_equation <- function(.data,...) {
    add_eq_class(NextMethod("filter"))
}


#' @method print sol_equation
print.sol_equation <- function(x,...) {
    for (i in seq_len(nrow(x))) {
        if ("equation_id" %in% names(x))
            cat("equation_id: ",x$equation_id[i],"\n",sep="")
        if ("taxon_name" %in% names(x))
            cat("  taxon_name: ",x$taxon_name[i],", taxon_aphia_id: ",x$taxon_aphia_id[i],"\n",sep="")

        if ("equation" %in% names(x))
            cat("  equation: ",paste(deparse(x$equation[i][[1]]),sep=" "),"\n",sep="")
        if ("inputs" %in% names(x)) {
            ips <- x$inputs[i][[1]]
            for (j in seq_len(nrow(ips)))
                cat("  it takes as input ",j,": ",ips$property[j]," (units: ",ips$units[j],")\n",sep="")
        }
        if ("return_property" %in% names(x)) {
            cat("  it estimates: ",x$return_property[i],sep="")
            if ("return_units" %in% names(x))
                cat(" (units: ",x$return_units[i],")",sep="")
            cat("\n")
        }
        if ("reliability" %in% names(x)) {
            rel <- x$reliability[i][[1]]
            for (j in seq_len(nrow(rel)))
                cat("  indicator of reliability: ",rel$reliability_type[j],"=",rel$reliability[j],"\n",sep="")
        }
        if ("notes" %in% names(x) && nzchar(x$notes[i]))
            cat("  notes: ",x$notes[i],"\n",sep="")
        if ("reference" %in% names(x) && nzchar(x$reference[i]))
            cat("  reference: ",x$reference[i],"\n",sep="")
        cat("\n")
    }
}
