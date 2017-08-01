context("solong")

test_that("equation usage generally works", {
    x <- tibble(LRL=c(11.3,13.9),species=c("Architeuthis dux"),aaa=c(3,2))
    ## no properties set
    expect_error(sol_allometry(x,c("342218_ML_Roel2000")),
                 "could not find required input prop")
    x$LRL <- sol_set_property(x$LRL,"lower rostral length")

    #xdf <- as.data.frame(x)
    #data_props <- vapply(seq_len(ncol(xdf)),function(j)solong:::sp_or_na(xdf[,j]),FUN.VALUE="",USE.NAMES=FALSE)
    #expect_identical(data_props,c("lower rostral length",NA,NA))

    #tmp <- solong:::resolve_cols(x,sol_equation("342218_ML_Roel2000"))
    #expect_identical(tmp,1)

    #tmp <- solong:::apply_eq(x,sol_equation("342218_ML_Roel2000")[c(1,1),])

    ## have set property, so should now work
    ## currently passing locally on test() but failing check() and
    ##   on travis: to be investigated
    xa <- sol_allometry(x,"342218_ML_Roel2000")
    expect_true(is.data.frame(xa))
    expect_equal(nrow(x),nrow(xa))
    expect_s3_class(xa$allometric_value,"solprop_ML")
    ## should get same result if equation object itself is provided
    xa2 <- sol_allometry(x,sol_equation("342218_ML_Roel2000"))
    expect_identical(xa,xa2)
    ## multiple equations
    xa <- sol_allometry(x,c("342218_ML_Roel2000","342218_ML_Clar1986"))
    ## multiple cols matching property
    x$aaa <- sol_set_property(x$LRL,"lower rostral length")
    expect_error(sol_allometry(x,c("342218_ML_Roel2000")),
                 "multiple columns of property")
})

test_that("mixtures of things work sensibly",{
    x <- tibble(LRL=c(11.3,13.9,NA),species=c("Architeuthis dux","Architeuthis dux","Leptonychotes weddellii"),SL=c(NA,NA,175)) %>%
        mutate(LRL=sol_set_property(LRL,"lower rostral length"),
               SL=sol_set_property(SL,"standard length","cm"))
    xa <- sol_allometry(x,c("342218_mass_Clar1986","342218_mass_Clar1986","195932_mass_GaBu1988"))
    ## expect in this case that all values have been provided in g
    expect_true(all(xa$allometric_value>units::set_units(10e3,units::ud_units$g)))
    expect_true(all(as.numeric(xa$allometric_value)>10e3)) ## all >10kg
    expect_identical(units(xa$allometric_value)$numerator,"g")

    ## but if we only do the 3rd one it should come back in kg
    xa <- sol_allometry(x[3,],c("342218_mass_Clar1986","342218_mass_Clar1986","195932_mass_GaBu1988")[3])
    expect_identical(units(xa$allometric_value)$numerator,"kg")
    expect_true(all(xa$allometric_value>units::set_units(100e3,units::ud_units$g))) ## expect >100kg
    expect_true(all(as.numeric(xa$allometric_value)>100))
})


test_that("equation pretty print", {
    tmp <- capture.output(print(sol_equation("342218_ML_Roel2000")))
    expect_gt(length(tmp),5)
    tmp <- paste0(tmp,collapse="\n")
    expect_true(grepl("equation_id:",tmp))
    expect_true(grepl("Reference:",tmp))

    eq <- sol_equations()[1,]
    tmp <- capture.output(print(eq))
    expect_gt(length(tmp),5)
    tmp <- paste0(tmp,collapse="\n")
    expect_true(grepl("equation_id:",tmp))
    expect_true(grepl("Reference:",tmp))
})
