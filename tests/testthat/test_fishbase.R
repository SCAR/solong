context("solong")

test_that("fishbase stuff works", {
    skip_if_not_installed("rfishbase")
    eq <- sol_fb_length_weight("Electrona antarctica")
    expect_gt(nrow(eq),1)
    x <- tibble(SL=10) %>%
        mutate(SL=sol_set_property(SL,"standard length",with_units="cm"))

    xa <- sol_allometry(x,eq[2,])
    expect_true(as.numeric(xa$allometric_value)>10) ## >10g

    eq <- sol_fb_length_weight("Pleuragramma antarctica")
    expect_is(eq$reference[[1]],"bibentry")
})
