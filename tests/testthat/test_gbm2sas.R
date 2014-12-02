test_that('gbm2sas returns a character',  {

    require(gbm)
    data(mtcars)
    gbm.fit <- gbm(mpg ~ cyl + disp, data=mtcars, bag.fraction=1.1,distribution="gaussian")
    ret <- gbm2sas(gbm.fit)
    expect_that(ret, is_a('character'))
})
