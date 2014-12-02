test_that('nnet2sas returns a character',  {

    require(nnet)

    data(iris)
    iris2 <- iris[iris$Species %in% c('setosa', 'versicolor'),]
    iris2$Species <- factor(iris2$Species)
    nnet.fit <- nnet(Species ~ ., data = iris2, size = 1, trace = FALSE)
    ret <- nnet2sas(nnet.fit)
    expect_that(ret, is_a('character'))

})
