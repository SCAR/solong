check_property_units <- function(prop,un) {
    tmp <- sol_set_property(1,prop)
    chk <- NULL
    try({
        units(tmp) <- ud_units[[un]]
        chk <- tmp
    },silent=TRUE)
    !is.null(chk)
}
