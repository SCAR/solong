context("solong equations")

test_that("equations from Dubi2012 behave as expected",{
    ## WW~OAL
    x <- tibble(OAL=c(60),species=c("Ihlea racovitzai"))
    x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    val <- sol_allometry(x,c("266542_WW~OAL_summer_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),2518)
    val <- sol_allometry(x,c("266542_WW~OAL_winter_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),1360)
    ##x$OAL <- sol_set_property(40,"oral-atrial length")
    ##val <- sol_allometry(x,c("266542_WW~OAL_autumn_Dubi2012"))$allometric_value
    ##expect_equal((round(as.numeric(val)/100)*100),700)
    x <- tibble(OAL=c(60),species=c("Salpa thomsoni"))
    x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    val <- sol_allometry(x,c("266545_WW~OAL_summer_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),2283)
    x$OAL <- sol_set_property(40,"oral-atrial length")
    val <- sol_allometry(x,c("266545_WW~OAL_winter_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),843)
    ##val <- sol_allometry(x,c("266545_WW~OAL_autumn_Dubi2012"))$allometric_value
    ##expect_equal((round(as.numeric(val)/100)*100),1400)

    ## DW~OAL
    x <- tibble(OAL=c(60),species=c("Ihlea racovitzai"))
    x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    val <- sol_allometry(x,c("266542_DW~OAL_summer_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),144)
    val <- sol_allometry(x,c("266542_DW~OAL_winter_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),76)
    x$OAL <- sol_set_property(40,"oral-atrial length")
    val <- sol_allometry(x,c("266542_DW~OAL_autumn_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),42)
    x <- tibble(OAL=c(60),species=c("Salpa thomsoni"))
    x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    val <- sol_allometry(x,c("266545_DW~OAL_summer_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),109)
    x$OAL <- sol_set_property(40,"oral-atrial length")
    val <- sol_allometry(x,c("266545_DW~OAL_winter_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),39)
    val <- sol_allometry(x,c("266545_DW~OAL_autumn_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),65)

    ## CW~OAL
    ## summer equations not used, curves in paper do not match equations given
    ##x <- tibble(OAL=c(60),species=c("Ihlea racovitzai"))
    ##x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    ##val <- sol_allometry(x,c("266542_CW~OAL_summer_Dubi2012"))$allometric_value
    ##expect_equal(round(as.numeric(val)),18)
    ##x <- tibble(OAL=c(40),species=c("Ihlea racovitzai"))
    ##x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    ##val <- sol_allometry(x,c("266542_CW~OAL_summer_Dubi2012"))$allometric_value
    ##expect_equal(round(as.numeric(val)),8)

    ## this equation not included, error in print
    ##val <- sol_allometry(x,c("266542_CW~OAL_winter_Dubi2012"))$allometric_value
    ##expect_equal(round(as.numeric(val)),??)
    ## summer equations not used, curves in paper do not match equations given
    ##x <- tibble(OAL=c(60),species=c("Salpa thomsoni"))
    ##x$OAL <- sol_set_property(x$OAL,"oral-atrial length")
    ##val <- sol_allometry(x,c("266545_CW~OAL_summer_Dubi2012"))$allometric_value
    ##expect_equal(round(as.numeric(val)),10)
    x$OAL <- sol_set_property(40,"oral-atrial length")
    ## winter salpa equation or curve wrong as well
    ##val <- sol_allometry(x,c("266545_CW~OAL_winter_Dubi2012"))$allometric_value
    ##expect_equal(round(as.numeric(val)),5)
    val <- sol_allometry(x,c("266545_CW~OAL_autumn_Dubi2012"))$allometric_value
    expect_equal(round(as.numeric(val)),7)

    ## printed curve does not match equation
    ##x <- tibble(OAL=sol_set_property(60,"oral-atrial length"))
    ##val <- sol_allometry(x,c("137217_LpW~OAL_Dubi2012"))$allometric_value
    ##expect_equal(round(as.numeric(val)),3.4) ## approx value from printed curve=3.4, equation gives 2.9



    if (FALSE) {
        ## visual checks of curves
        x <- tibble(OAL=sol_set_property(5:60,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_WW~OAL_summer_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_WW~OAL_summer_Dubi2012"))$allometric_value,lty=2)
        ## ok

        x <- tibble(OAL=sol_set_property(5:40,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_WW~OAL_autumn_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_WW~OAL_autumn_Dubi2012"))$allometric_value,lty=2)
        ## neither of these look right

        x <- tibble(OAL=sol_set_property(5:40,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_WW~OAL_winter_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_WW~OAL_winter_Dubi2012"))$allometric_value,lty=2)
        ## ok

        x <- tibble(OAL=sol_set_property(5:60,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_DW~OAL_summer_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_DW~OAL_summer_Dubi2012"))$allometric_value,lty=2)
        ## ok

        x <- tibble(OAL=sol_set_property(5:40,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_DW~OAL_autumn_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_DW~OAL_autumn_Dubi2012"))$allometric_value,lty=2)
        ## ok

        x <- tibble(OAL=sol_set_property(5:40,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_DW~OAL_winter_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_DW~OAL_winter_Dubi2012"))$allometric_value,lty=2)
        ## ok

        x <- tibble(OAL=sol_set_property(5:60,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266542_CW~OAL_summer_Dubi2012"))$allometric_value,type="l")
        lines(x$OAL,sol_allometry(x,c("266545_CW~OAL_summer_Dubi2012"))$allometric_value,lty=2)
        ## both curves in paper are elevated compared to the curves here

        x <- tibble(OAL=sol_set_property(5:40,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266545_CW~OAL_autumn_Dubi2012"))$allometric_value,type="l",lty=2)
        ## ok

        x <- tibble(OAL=sol_set_property(5:40,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("266545_CW~OAL_winter_Dubi2012"))$allometric_value,type="l",lty=2)
        ## definitely not

        x <- tibble(OAL=sol_set_property(5:60,"oral-atrial length"))
        plot(x$OAL,sol_allometry(x,c("137217_LpW~OAL_Dubi2012"))$allometric_value,type="l")
        ## nope
    }
})
