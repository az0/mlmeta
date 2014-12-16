test_that('earth2sas',  {

    require(earth)

    # regression
    trees.earth <- earth(Volume ~ ., data=trees)
    trees.sas <- earth2sas(trees.earth)
    expect_that(trees.sas, is_a('character'))

    # regression with caret
    require(caret)
    trees.earth.caret <- train(Volume ~ .,
        data = trees,
        method = "earth")
    trees.caret.sas <- earth2sas(trees.earth.caret)
    expect_that(trees.caret.sas, is_a('character'))

    # invalid input
    expect_that(earth2sas(1), throws_error())

})
