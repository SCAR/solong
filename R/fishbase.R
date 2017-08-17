## stuff for interacting with fishbase

#' Create allometric equation from Fishbase
#'
#' Experimental. Requires the rfishbase package to be installed.
#'
#' @param ... : arguments passed to rfishbase::length_weight
#' @param worms logical: if TRUE, and if the worrms package is installed, try and find the AphiaID for the taxon in the World Register of Marine Species
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
sol_fb_length_weight <- function(...,worms=requireNamespace("worrms",quietly=TRUE)) {
    assert_that(is.flag(worms))
    if (!requireNamespace("rfishbase",quietly=TRUE))
        stop("install the rfishbase package to use this function")
    if (worms && !requireNamespace("worrms",quietly=TRUE)) {
        warning("worms=TRUE but the worrms package is not installed, ignoring")
        worms <- FALSE
    }
    x <- rfishbase::length_weight(...)
    if (nrow(x)<1) return(NULL)
    thisrefs <- NULL
    if (!all(is.na(x$DataRef))) {
        suppressWarnings(thisrefs <- rfishbase::references(na.omit(x$DataRef),
                                          fields=c("RefNo","Author","Year","Title","Source","ShortCitation")))
        if (nrow(thisrefs)>0) {
            thisrefs <- thisrefs %>% rowwise %>%
                summarize_(DataRef=~RefNo,
                           reference=~bibentry(bibtype="Misc",key=paste0("fishbase::",RefNo),
                                               author=person(Author),
                                               year=Year,title=Title,
                                               howpublished=paste("Fishbase reference",RefNo,":",ShortCitation,".",Source,sep=" ")))
            x <- x %>% left_join(thisrefs,by="DataRef")
        }
    }


    mk_eqfun <- function(a,b) {
        ## see also LCLa, UCLa, LCLb, UCLb
        eval(parse(text=sprintf("function(L) tibble(allometric_value=%g*(L^%g))",a,b)))
    }

    do.call(rbind,lapply(seq_len(nrow(x)),function(z) {
        this <- x[z,]
        suppressWarnings(
        out <- sol_make_equation(equation_id=paste0("fishbase::",this$AutoCtr),
                          taxon_name=this$sciname,
                          equation=mk_eqfun(this$a,this$b),
                          inputs=tibble(property=fb_len_map(this$Type),units="cm",sample_minimum=this$LengthMin,sample_maximum=this$LengthMax),
                          return_property="wet weight",
                          return_units="g")
        )
        if ("reference" %in% names(this)) out$reference[[1]] <- this$reference[[1]]
        if (worms) {
            wx <- worrms::wm_records_name(out$taxon_name) %>%
                filter_(~status=="accepted")
            if (nrow(wx)==1)
                out$taxon_aphia_id=wx$AphiaID
        }
        if (!is.na(this$CoeffDetermination))
            out$reliability[[1]] <- tribble(~type,~value,
                                            "R^2",this$CoeffDetermination)
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
