globalVariables("allometric_equations") # To make R CMD Check happy

#' Apply allometric equations
#'
#' @param data data.frame: input data
#' @param equation_id string or character: the identifier of the equation to apply. Either a single string (that equation will be applied to all rows of the data) or a character vector with length matching the number of rows of the data
#'
#' @return something
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
sol_allometry <- function(data,equation_id) {
    assert_that(is.data.frame(data))
    assert_that(is.character(equation_id))
    if (!all(nzchar(equation_id))) stop("equation_id entries must not be empty strings")
    assert_that(length(equation_id)==1 || length(equation_id)==nrow(data))
    if (length(equation_id)==1) equation_id <- rep(equation_id,nrow(data))
    ## apply in groups by equation_id

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
    data <- data %>%
        mutate_(equation_id=~equation_id,
                SAVE_ROW_ID=~seq_len(n()))
    grp_id <- group_indices_(data,~equation_id)
    eqru <- function(eqid) sol_equation(eqid)$return_units
    use_property_units <- length(unique(grp_id))>1 && length(unique(vapply(unique(equation_id),eqru,FUN.VALUE="")))>1
    ## if we have different return units for the equations being used,
    ## we'll convert the returned value's units to the default for the property in question
    ## otherwise when we combine the chunks with rbind, the values don't get converted for different units
    out <- lapply(unique(grp_id),function(gid) {
        idx <- grp_id==gid
        apply_eq(data[idx,],equation_id[idx][1],use_property_units=use_property_units)
    })
    chk_prop <- vapply(out,function(z)sol_get_property(z$allometric_value),FUN.VALUE="",USE.NAMES=FALSE)
    out <- do.call(rbind,out) %>%
        arrange_(~SAVE_ROW_ID) %>%
        select_(~-SAVE_ROW_ID)
    if (length(unique(chk_prop))!=1) {
        warning("return values are not all of the same property")
        out$allometric_value <- strip_units(out$allometric_value)
        out$allometric_value <- sol_set_property(out$allometric_value,NULL)
    }
    out
}

strip_units <- function(x) {
    class(x) <- setdiff(class(x),"units")
    attr(x,"units") <- NULL
    x
}

## augment data with output of equation
apply_eq <- function(data,eqn_id,use_property_units=FALSE) {
    assert_that(is.data.frame(data))
    assert_that(is.string(eqn_id))
    eqn <- sol_equation(eqn_id)
    cidx <- resolve_cols(data,eqn) ## column indices into data of the required inputs. NA if required input property not found
    if (any(is.na(cidx)))
        stop(sprintf("could not find required input properties (%s) in data",paste(eqn$inputs[[1]]$property[is.na(cidx)],collapse=", ")))
    data2 <- as.data.frame(data)[,cidx,drop=FALSE]
    ## convert units, if necessary
    for (i in seq_len(ncol(data2)))
        units(data2[,i]) <- ud_units[[eqn$inputs[[1]]$units[i]]]
    ## now remove units, so that equation can just be applied without getting upset by e.g. ^2 operations
    data2 <- lapply(seq_len(ncol(data2)),function(z)strip_units(data2[,z]))
    ## get the equation output and set its property and units, add it to data
    out <- unclass(do.call(eqn$equation[[1]],data2)) %>%
        sol_set_property(eqn$return_property,with_units=eqn$return_units)
    ## we've set the units to whatever the equation's return units are
    if (use_property_units) {
        ## convert to the default units for the property
        ## this will help enforce consistency across equations
        ## note though that this will drop the sol_property class
        units(out) <- ud_units[[sol_properties(eqn$return_property)$units]]
        out <- out %>% sol_set_property(eqn$return_property)
    }
    data %>% mutate_(allometric_property=~eqn$return_property,allometric_value=~out)
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
