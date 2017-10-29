##oldrefs$Lake2003 <- "Lake S, Burton H, van den Hoff J (2003) Regional, temporal and fine-scale spatial variation in Weddell seal diet at four coastal locations in east Antarctica. Marine Ecology Progress Series 254:293-305. doi:10.3354/meps254293"
refs$Lake2003 <- bibentry(bibtype="Article",key="Lake2003",
                             author=c(person("S","Lake"),person("H","Burton"),person("J","van den Hoff")),
                             year=2003,
                             title="Regional, temporal and fine-scale spatial variation in Weddell seal diet at four coastal locations in east Antarctica",
                             journal="Marine Ecology Progress Series",
                             volume=254,pages="293-305",doi="10.3354/meps254293")

if (FALSE) {
    library(dplyr)
    xlake <- tribble(~CL,~BL,~mass,~sex,
                     8.05,32.92,0.37,"j",
                     8.61,33.65,0.55,"j",
                     8.89,37.4,0.75,"j",
                     10.81,44.03,1.26,"j",
                     9.71,38.85,0.88,"j",
                     8.66,34.96,0.62,"j",
                     9.84,38.32,0.82,"j",
                     10.97,40.72,1.02,"j",
                     10.1,41.23,1.07,"j",
                     11.43,43.34,1.11,"j",
                     9.3,39.37,0.83,"j",
                     9.08,38.31,0.7,"j",
                     11.25,46.39,1.09,"j",
                     8.41,34.23,0.51,"j",
                     14.79,55.62,2.65,"j",
                     6.19,24.69,0.21,"j",
                     7.95,30.25,0.42,"j",
                     11.9,45.47,1.53,"j",
                     12.46,44.56,1.56,"j",
                     10.28,42.04,1.05,"j",
                     8.05,32.31,0.52,"j",
                     8.55,35.46,0.47,"j",
                     14.53,56.25,2.68,"m",
                     14.44,55.94,2.57,"m",
                     13.87,54.07,2.43,"m",
                     14.34,57.14,3.16,"m",
                     16.24,60.6,3.38,"m",
                     13.86,53.7,2.48,"m",
                     15.97,54.11,2.96,"m",
                     14.44,58.28,2.85,"f",
                     15.01,58.78,3.07,"f",
                     12.88,53.94,2.33,"f",
                     14.19,58.33,2.65,"f",
                     13.32,50.29,2.11,"f")

    fit <- lm(log(mass)~log(CL),data=xlake)
    ## i.e. log(mass)~a+b*log(CL)
    a <- coefficients(fit)[1] ## -6.966094
    b <- coefficients(fit)[2] ## 2.97569
    df_residual <- fit$df.residual ## 32
    s2_residual <- sum(fit$residuals^2) ## 0.4449043
    V <- vcov(fit) ## matrix(c(0.03948, -0.01624, -0.01624, 0.00675),nrow=2,byrow=TRUE)

}

alleq_Lake2003 <- function(id) {
    switch(id,
           ## Chorismus antarcticus from Lake et al. 2003
           "369214_WW_Lake2003"=list(taxon_name="Chorismus antarcticus",
                                     taxon_aphia_id=369214,
                                     equation=function(CL){
                                         lw_alb <- function(l,a,b,V,df_residual,s2_residual,ci=0.95,interval="prediction") {
                                             interval <- match.arg(interval,c("confidence","prediction"))
                                             Xp <- model.matrix(~L,data.frame(L=log(l))) ## model matrix
                                             w_hat <- as.numeric(Xp %*% c(a,b)) ## predicted mean
                                             alpha <- qt((1-ci)/2,df=df_residual)
                                             se2 <- unname(rowSums((Xp %*% V) * Xp)) ## prediction variance
                                             ## for prediction interval, additional uncertainty due to noise
                                             if (interval=="prediction") se2 <- se2+s2_residual/df_residual
                                             int <- abs(alpha*sqrt(se2))
                                             tibble(mean=as.numeric(w_hat),lower=w_hat-int,upper=w_hat+int)
                                         }
                                         ## fitted coefs
                                         a <- -6.966094
                                         b <- 2.97569
                                         V <- matrix(c(0.03948, -0.01624, -0.01624, 0.00675),nrow=2,byrow=TRUE)
                                         df_residual <- 32
                                         s2_residual <- 0.4449043
                                         out <- lw_alb(CL,a,b,V,df_residual,s2_residual)
                                         tibble(allometric_value=exp(out$mean),
                                                allometric_value_lower=exp(out$lower),
                                                allometric_value_upper=exp(out$upper))
                                     },
                                     inputs=tibble(property="carapace length",units="mm",sample_minimum=6,sample_maximum=16),
                                     return_property="wet weight",
                                     return_units="g",
                                     reliability=tribble(~type,~value,
                                                         "N",35,
                                                         "R^2",0.976),
                                     notes="Upper and lower bounds on estimates are prediction intervals. Equation refitted using data from http://doi.org/10.4225/15/5982a2aeb56e1",
                                     reference=refs$Lake2003),
           stop("unrecognized equation ID: ",id))
}
