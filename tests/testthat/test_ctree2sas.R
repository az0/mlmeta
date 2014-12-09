test_that('ctree2sas returns a character',  {

    require(party)

    # regression
    data(mtcars)
    mtcars.ctree <- ctree(mpg ~ cyl + disp, data = mtcars)
    mtcars.sas <- ctree2sas(mtcars.ctree)
    expect_that(mtcars.sas, is_a('character'))

    # binary classification
    data(iris)
    iris2 <- iris[iris$Species %in% c('setosa', 'versicolor'),]
    iris2$Species <- factor(iris2$Species)
    iris2.ctree <- ctree(Species ~ ., data = iris2)
    iris2.sas <- ctree2sas(iris2.ctree)
    expect_that(iris2.sas, is_a('character'))

    # invalid input
    expect_that(ctree2sas(1), throws_error())

})
