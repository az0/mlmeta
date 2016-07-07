test_that('simulate_regression_data',  {

    s1 <- simulate_regression_data(n = 1000, unordered_factor = TRUE, ordered_factor = TRUE)
    expect_that(s1, is_a('data.frame'))
    expect_that(1000, equals(nrow(s1)))
    expect_that(4, equals(sum(sapply(s1, is.factor))))
    expect_that(1, equals(sum(sapply(s1, is.ordered))))

    s2 <- simulate_regression_data(n = 10, unordered_factor = FALSE, ordered_factor = FALSE)
    expect_that(s2, is_a('data.frame'))
    expect_that(10, equals(nrow(s2)))
    expect_that(0, equals(sum(sapply(s2, is.factor))))
    expect_that(0, equals(sum(sapply(s2, is.ordered))))
    expect_that(0, (equals(sum(sapply(s2, is.na)))))

    s3 <- simulate_regression_data(p_missing = 0)
    expect_that(s3, is_a('data.frame'))
    expect_that(0, equals(sum(sapply(s3, is.na))))
})
