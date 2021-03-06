context("Testing Winer and Mauchly statistical tests and Shrout & Fleiss ICC functions")


test_that("Testing Winer test", {
    dta <- data.frame(cbind(
        col1 <- c(3., 6., 2., 2., 5.),
        col2 <- c(4., 5., 4., 4., 3.),
        col3 <- c(2., 7., 7., 8., 6.),
        col4 <- c(6., 8., 4., 6., 5.)
    ))

    expect_equal( WinerCompoundSymmetryTest(dta), 0.6733123 )
})


test_that("Testing Mauchly test", {
    dta <- data.frame(cbind(
        col1 <- c(3., 6., 2., 2., 5.),
        col2 <- c(4., 5., 4., 4., 3.),
        col3 <- c(2., 7., 7., 8., 6.),
        col4 <- c(6., 8., 4., 6., 5.)
    ))

    expect_equal( MauchlySphericityTest(dta), 0.5824426 )
})


test_that("Testing Shrout and Fleiss functions", {
    dta <- data.frame(cbind(
        clus <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
        col1 <- c(2, 4, 4, 6, 4, 5, 8, 8, 5, 8, 9, 9)
    ))

    expect_equal( ShroutFleissICC1(dta, 1, 2), 0.434343434 )
    expect_equal( ShroutFleissICC11(dta[, 1], dta[,2]), 0.434343434 )

    dta2 <- data.frame(cbind(
        clus <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
        col1 <- c(1, 3, 3, 5, 3, 4, 7, 7, 4, 7, 8, 8),
        col1 <- c(2, 4, 4, 6, 4, 5, 8, 8, 5, 8, 9, 9),
        col1 <- c(3, 5, 5, 7, 5, 6, 9, 9, 6, 9, 10, 10)
    ))

    expect_equal( ShroutFleissICC1(dta2, 1, 2:4), 0.7543859649 )
    expect_equal( ShroutFleissICC1k(dta2[, 1], dta2[,2:4]), 0.7543859649 )

})







