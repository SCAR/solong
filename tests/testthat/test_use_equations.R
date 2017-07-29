context("solong")

test_that("equation usage generally works", {
    x <- tibble(LRL=c(11.3,13.9),species=c("Architeuthis dux"),aaa=c(3,2))
    ## no properties set
    expect_error(sol_allometry(x,c("342218_ML_Roel2000")),
                 "could not find required input prop")
    x$LRL <- sol_set_property(x$LRL,"lower rostral length")
    ## set property, should work
    xa <- sol_allometry(x,c("342218_ML_Roel2000"))
    expect_true(is.data.frame(xa))
    expect_equal(nrow(x),nrow(xa))
    expect_s3_class(xa$allometric_value,"solprop_ML")
    ## multiple equations
    xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_ML_Clar1986"))
    ## multiple cols matching property
    x$aaa <- sol_set_property(x$LRL,"lower rostral length")
    expect_error(sol_allometry(x,c("342218_ML_Roel2000")),
                 "multiple columns of property")
})
