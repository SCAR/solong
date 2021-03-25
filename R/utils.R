check_property_units <- function(prop, un) {
    tmp <- sol_set_property(1, prop)
    chk <- NULL
    try({
        units(tmp) <- as_units(un)
        chk <- tmp
    }, silent=TRUE)
    !is.null(chk)
}

#' Von Bertalanffy growth equation with optional propagation of uncertainty
#'
#' Intended for use inside of \code{sol_equation} objects.
#'
#' @param t numeric: prediction times
#' @param Linf numeric: Linf parameter estimate
#' @param k numeric: k parameter estimate
#' @param t0 numeric: t0 parameter estimate
#' @param Linf_se numeric: Linf parameter standard error estimate
#' @param k_se numeric: k parameter standard error estimate
#' @param t0_se numeric: t0 parameter standard error estimate
#' @param reps integer: number of Monte-Carlo samples to draw
#' @param ci numeric: confidence level
#' @param method string: method to use for uncertainty propagation (only "monte carlo" supported at the moment)
#'
#' @return A tibble with columns \code{allometric_value}, \code{allometric_value_lower}, \code{allometric_value_upper}. If any of the standard error inputs are missing, \code{NULL}, or \code{NA}, the upper and lower estimates will be \code{NA_real_}
#'
#' @examples
#' sol_vonbert(0:7, Linf = 80.7, Linf_se = 0.82, k = 0.25, k_se = 0.01, t0 = -2.31, t0_se = 0.01)
#'
#' @export
sol_vonbert <- function(t, Linf, k, t0, Linf_se, k_se, t0_se, reps = 1000L, ci = 0.95, method = "monte carlo") {
    mn <- Linf * (1-exp(-k * (t-t0))) ## mean
    ul <- cbind(rep(NA_real_, length(mn)), rep(NA_real_, length(mn)))
    if (!missing(Linf_se) && !is.null(Linf_se) && !is.na(Linf_se) &&
        !missing(k_se) && !is.null(k_se) && !is.na(k_se) &&
        !missing(t0_se) && !is.null(t0_se) && !is.na(t0_se)) {
        assert_that(is.string(method))
        method <- match.arg(tolower(method), c("monte carlo"))
        if (method == "monte carlo") {
            Linf <- rnorm(reps, mean = Linf, sd = Linf_se)
            k <- rnorm(reps, mean = k, sd = k_se)
            t0 <- rnorm(reps, mean = t0, sd = t0_se)
            ul <- do.call(rbind, lapply(t, function(z) {
                this <- Linf * (1-exp(-k * (z-t0)))
                quantile(this, c((1-ci)/2, (1+ci)/2))
            }))
        }
    }
    colnames(ul) <- c("allometric_value_lower", "allometric_value_upper")
    dplyr::as_tibble(cbind(allometric_value = mn, ul))
}
