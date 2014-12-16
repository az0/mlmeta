test_that('earth2sas',  {

    require(earth)

    # regression
    trees.earth <- earth(Volume ~ ., data=trees)
    trees.sas <- earth2sas(trees.earth)
    expect_that(trees.sas, is_a('character'))

    # invalid input
    expect_that(earth2sas(1), throws_error())

})
