#' Generate a Data Set for Testing Regression Models
#' @param n number of rows
#' @param unordered_factor whether to include an unordered factor
#' @param ordered_factor whether to include an ordered factor
#' @return a data frame
#' @author Greg Ridgeway
#' @author Andrew Ziem
simulate_regression_data <- function(n = 1000, unordered_factor = TRUE, ordered_factor = TRUE)
{
    stopifnot(n > 1)

    X1 <- runif(n)
    X2 <- runif(n)
    if (unordered_factor) {
        X3 <- factor(sample(letters[1:3], n, replace=TRUE))
        X3.a <- as.numeric('a' == X2)
        X3.b <- as.numeric('b' == X2)
    } else {
        X3 <- 2*runif(n)
        X3.a <- as.numeric(X3 < 0.5)
        X3.b <- as.numeric(X3 >= 0.5)
    }
    if (ordered_factor) {
        X4 <- ordered(sample(letters[1:4], n,replace=TRUE),levels=letters[4:1])
        X4.numeric <- c(-1,0,1,2)[as.numeric(X4)]
    } else {
        X4.numeric <- X4 <- runif(n)
    }

    # these variables are just noise
    if (unordered_factor) {
        X5 <- factor(sample(letters[1:6], n,replace=TRUE))
        X6 <- factor(sample(letters[1:3], n,replace=TRUE))
    } else {
        X5 <- rnorm(n)
        X6 <- rlnorm(n)
    }
    X7 <- 3*runif(n) 

    # true regression equation
    Y.nonoise <- 2 + X1 + X2**1.5 + X3.a + X3.b + X4.numeric
    
    # add noise
    SNR <- 10 # signal-to-noise ratio
    sigma <- sqrt(var(Y.nonoise)/SNR)
    Y <- Y.nonoise + rnorm(n,0,sigma)

    # introduce some missing values
    n.missing <- round(n*.03)
    X1[sample(1:n, size=n.missing)] <- NA
    X2[sample(1:n, size=n.missing)] <- NA
    X3[sample(1:n, size=n.missing)] <- NA
    X4[sample(1:n, size=n.missing)] <- NA
    X5[sample(1:n, size=n.missing)] <- NA
    X6[sample(1:n, size=n.missing)] <- NA
    X7[sample(1:n, size=n.missing)] <- NA

    # collect and return
    data <- data.frame(Y,X1,X2,X3,X4,X5,X6, X7)
    data
}


