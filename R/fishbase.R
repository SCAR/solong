## stuff for interacting with fishbase

#' Create allometric equation from Fishbase
#'
#' Experimental.
#'
#' @param ... : arguments passed to rfishbase::length_weight
#'
#' @return equation object
#'
#' @seealso \code{\link{sol_equation}} \code{\link{sol_equations}}
#'
#' @examples
#' \dontrun{
#'  library(dplyr)
#'  eq <- sol_fb_length_weight("Electrona antarctica")
#'  x <- tibble(SL=10) %>%
#'    mutate(SL=sol_set_property(SL,"standard length",with_units="cm"))
#'  sol_allometry(x,eq[2,])
#' }
#'
#' @export
sol_fb_length_weight <- function(...) {
    if (!requireNamespace("rfishbase",quietly=TRUE))
        stop("install the rfishbase package to use this function")
    x <- rfishbase::length_weight(...)
    do.call(rbind,lapply(seq_len(nrow(x)),function(z) {
        this <- x[z,]
        suppressWarnings(
        out <- sol_make_equation(equation_id=as.character(this$AutoCtr),
                          taxon_name=this$sciname,
                          equation=function(L){
                              a <- this$a
                              b <- this$b
                              tibble(allometric_value=a*(L^b))
                          },
                          inputs=tibble(property=fb_len_map(this$Type),units="cm",sample_minimum=this$LengthMin,sample_maximum=this$LengthMax),
                          return_property="wet weight",
                          return_units="g")
        )
        if (!is.na(this$CoeffDetermination))
            out$reliability[[1]] <- tribble(~type,~value,
                                            "R^2",this$CoeffDetermination)
        ##reference=???)
        out
    }
    ))
}


fb_len_map <- function(z) {
    switch(z,
           TL="total length",
           SL="standard length",
           FL="fork length",
           stop("unhandled fishbase length type: ",z)
           )
}
