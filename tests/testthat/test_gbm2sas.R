test_that('gbm2sas returns a character',  {

    require(gbm)

    # Gaussian distribution
    data(mtcars)
    gbm.fit.g <- gbm(mpg ~ cyl + disp, data = mtcars, bag.fraction = 1.1,distribution = "gaussian")
    ret <- gbm2sas(gbm.fit.g)
    expect_that(ret, is_a('character'))

    # Bernoulli distribution
    data(iris)
    iris2 <- iris[iris$Species %in% c('setosa', 'versicolor'),]
    iris2$Species <- factor(iris2$Species)
    gbm.fit.b <- gbm(Species ~ ., data = iris2, bag.fraction = 1.1, distribution = "gaussian")
    ret <- gbm2sas(gbm.fit.b)
    expect_that(ret, is_a('character'))



})
