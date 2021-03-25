globalVariables("allometric_equations") # To make R CMD Check happy

#' Apply allometric equations
#'
#' @param data data.frame: input data
#' @param equation character or sol_equation object: either the identifier of the equation to apply, or the equation object itself. Can be a single element (this equation will be applied to all rows of the data) or with length matching the number of rows of the data
#'
#' @return the input data frame, augmented with columns "allometric_property", "allometric_value", "allometric_value_lower", and "allometric_value_upper"
#'
#' @seealso \code{\link{allometric_equations}}
#'
#' @examples
#' \dontrun{
#' x <- data.frame(LRL=c(11.3,13.9),species=c("Architeuthis dux"),
#'   stringsAsFactors=FALSE)
#' ## it doesn't matter what the column names are, but we
#' ## need to set the property types correctly
#' x$LRL <- sol_set_property(x$LRL,"lower rostral length")
#'
#' ## apply a single equation to all rows
#' sol_allometry(x,c("342218_ML_Roel2000"))
#'
#' ## apply a different equation to each row
#' sol_allometry(x,c("342218_ML_Roel2000","342218_ML_Clar1986"))
#' }
#' @export
sol_allometry <- function(data,equation) {
    assert_that(is.data.frame(data))
    assert_that(is.character(equation) || inherits(equation,"sol_equation"))
    if (is.character(equation)) {
        if (!all(nzchar(equation))) stop("equation entries must not be empty strings")
        assert_that(length(equation)==1 || length(equation)==nrow(data))
        ## get tibble of equations for these equation IDs
        equation <- do.call(rbind,lapply(equation,sol_equation))
    } else {
        assert_that(nrow(equation)==1 || nrow(equation)==nrow(data))
    }
    ## find unique equations, apply in groups
    ## could use equation_id, but for user-supplied equations these might not be unique identifiers
    equation_hash <- vapply(seq_len(nrow(equation)),function(z)digest(equation[z,]),FUN.VALUE="")

    ## can't do this, because the bind_rows that follows do() drops the attributes
##    suppressWarnings(
##        data %>%
##        mutate_(equation_id=~equation_id,SAVE_ROW_ID=~seq_len(n())) %>%
##        group_by_(~equation_id) %>%
##        do(apply_eq(.,eqn_id=equation_id[1])) %>%
##        ungroup %>%
##        arrange_(~SAVE_ROW_ID) %>%
##        select_(~-SAVE_ROW_ID)
##    )
    data <- mutate(data, SAVE_ROW_ID = seq_len(n()))
    grp_id <- group_indices(group_by(tibble(eqh = equation_hash), .data$eqh))
    ## if we have different return units for the equations being used,
    ## we'll convert the returned value's units to the default for the property in question
    ## otherwise when we combine the chunks with rbind, the values don't get converted for different units
    ## test that we have more than one unique equation, and their return units are different
    use_property_units <- length(unique(vapply(seq_len(nrow(equation)), function(z) equation$return_units[z], FUN.VALUE = ""))) > 1
    out <- lapply(unique(grp_id), function(gid) {
        idx <- grp_id == gid
        eidx <- if (nrow(equation)==1) 1 else which(idx)[1]
        apply_eq(data[idx, ], equation[eidx, ], use_property_units = use_property_units)
    })
    chk_prop <- vapply(out,function(z)sol_get_property(z$allometric_value),FUN.VALUE="",USE.NAMES=FALSE)
    out <- do.call(rbind,out) %>%
        arrange(.data$SAVE_ROW_ID) %>%
        select(-"SAVE_ROW_ID")
    if (length(unique(chk_prop))!=1) {
        warning("return values are not all of the same property")
        out$allometric_value <- strip_units(out$allometric_value)
        out$allometric_value <- sol_set_property(out$allometric_value,NULL)
    }
    out
}


#' Remove the units from an object
#'
#' A convenience function to remove the units assigned to an object.
#'
#' @param x object: with units
#'
#' @return x, with units removed
#'
#' @examples
#' x <- data.frame(LRL=c(11.3,13.9),species=c("Architeuthis dux"),
#'   stringsAsFactors=FALSE)
#' x$LRL <- sol_set_property(x$LRL,"lower rostral length")
#' ## apply an allometric equation
#' xa <- sol_allometry(x,c("342218_ML_Roel2000"))
#'
#' strip_units(xa$allometric_value)
#'
#'
#' @export
strip_units <- function(x) {
    class(x) <- setdiff(class(x),"units")
    attr(x,"units") <- NULL
    x
}

## augment data with output of equation
apply_eq <- function(data,eqn,use_property_units=FALSE) {
    assert_that(is.data.frame(data))
    assert_that(inherits(eqn,"sol_equation"))
    assert_that(nrow(eqn)==1)
    cidx <- resolve_cols(data,eqn) ## column indices into data of the required inputs. NA if required input property not found
    if (any(is.na(cidx)))
        stop(sprintf("could not find required input properties (%s) in data",paste(eqn$inputs[[1]]$property[is.na(cidx)],collapse=", ")))
    data2 <- as.data.frame(data)[,cidx,drop=FALSE]
    ## convert units, if necessary
    for (i in seq_len(ncol(data2)))
        units(data2[,i]) <- as_units(eqn$inputs[[1]]$units[i])
    ## now remove property and units, so that equation can just be applied without getting upset by e.g. ^2 operations
    ## and convert data2 to list, so can call equation with do.call
    data2 <- lapply(seq_len(ncol(data2)),function(z)sol_set_property(data2[,z],NULL))
    ## get the equation output and set its property and units, add it to data
    out <- do.call(eqn$equation[[1]],data2)
    if (!"allometric_value_lower" %in% names(out))
        out$allometric_value_lower <- NA
    if (!"allometric_value_upper" %in% names(out))
        out$allometric_value_upper <- NA
    ## set property
    out$allometric_value <- sol_set_property(out$allometric_value,eqn$return_property,with_units=eqn$return_units)
    attributes(out$allometric_value_lower) <- attributes(out$allometric_value)
    attributes(out$allometric_value_upper) <- attributes(out$allometric_value)
    ## we've set the units to whatever the equation's return units are
    if (use_property_units) {
        ## convert to the default units for the property
        ## this will help enforce consistency across equations
        ## note though that this will drop the sol_property class
        units(out$allometric_value) <- as_units(sol_properties(eqn$return_property)$units)
        units(out$allometric_value_lower) <- as_units(sol_properties(eqn$return_property)$units)
        units(out$allometric_value_upper) <- as_units(sol_properties(eqn$return_property)$units)
        ## reinstate sol_property
        out$allometric_value <- sol_set_property(out$allometric_value,eqn$return_property)
        attributes(out$allometric_value_lower) <- attributes(out$allometric_value)
        attributes(out$allometric_value_upper) <- attributes(out$allometric_value)
    }
    out$allometric_property <- eqn$return_property
    bind_cols(data,out)
}

which_or_na <- function(x) if (length(which(x))>0) which(x) else NA


# Figure out which columns of input data correspond to the input vars that we need for this equation
#
# @param data data.frame: input data
# @param eqn data.frame: equation, e.g. a row from allometric_equations
#
# @return numeric vector, where each entry is the index of the column of data that corresponds to the nth required input, or NA if the matching column was not present
resolve_cols <- function(data,eqn) {
    data <- as.data.frame(data)
    data_props <- vapply(seq_len(ncol(data)),function(j)sp_or_na(data[,j]),FUN.VALUE="",USE.NAMES=FALSE)
    vapply(eqn$inputs[[1]]$property,function(z) {
        tmp <- which_or_na(data_props==z)
        if (length(tmp)>1) stop("data has multiple columns of property ",z,", don't know which one to use")
        tmp
    },FUN.VALUE=1,USE.NAMES=FALSE)
}

sp_or_na <- function(z) {tmp <- sol_get_property(z); if (length(tmp)>0) tmp else NA_character_}
