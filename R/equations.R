
#' Allometric equations
#'
#' @param id string: the identifier of the equation. If omitted, all equations will be returned
#'
#' @return data.frame
#'
#' @examples
#' sol_equation()
#' sol_equation("342218_ML_Roel2000")
#'
#' @export
sol_equation <- function(id) {
    if (!missing(id)) assert_that(is.string(id))
    eqn <- allometric_equations
    if (!missing(id)) {
        eqn <- eqn %>% filter_(~equation_id==id)
        if (nrow(eqn)<1) {
            stop("no equations matching id: ",id)
        } else if (nrow(eqn)>1) {
            stop("multiple equations matching id: ",id)
        }
    }
    eqn
}
