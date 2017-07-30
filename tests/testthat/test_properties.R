context("solong")

test_that("subsetting preserves properties", {
    x <- tibble(LRL=c(11.3,13.9,10.0),species=c("Architeuthis dux"),aaa=c(3,2,1))
    x$LRL <- sol_set_property(x$LRL,"lower rostral length")
    expect_equal(sol_get_property(x$LRL),"lower rostral length")
    x <- x[1:2,]
    expect_equal(sol_get_property(x$LRL),"lower rostral length")
})

