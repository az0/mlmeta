# Machine Learning Metaprogramming for R
# by Andrew Ziem
# Copyright (c) 2014 Compassion International
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


# Create a model and data to check that gbm gives the same
# values in R and SAS.

library(mlmeta)
library(testthat)

#' Indicate whether tests are running on CRAN
#' @return boolean
on_cran <- function() {
    '' == Sys.getenv('NOT_CRAN')
}

#' Set the working directory for testing purposes
#'
#' On CRAN, do nothing. On Linux, switch to \code{/tmp}.  On Windows,
#' switch to \code{\%TMP\%}.
test_setwd <- function() {
    # On CRAN do not use a temporary directory because (1) CRAN
    # policies forbid writing to /tmp and (2) CRAN does not have SAS.
    if (on_cran()) return(invisible())
    if (''==(tmpdir <- Sys.getenv('TMP')))
        tmpdir <- '/tmp'
    setwd(tmpdir)
    writeLines(paste('changing working directory to: ', getwd()))
}

#' A generic function for testing functions that generate SAS code
test_foo2sas <- function(name, pkg, data_func, model_func, ml_func, predict_func) {
    expect_is(name, 'character')
    expect_is(pkg, 'character')
    expect_is(data_func, 'function')
    expect_is(model_func, 'function')
    expect_is(ml_func, 'function')
    expect_is(predict_func, 'function')
    writeLines(paste('testing',name,'from package', pkg))
    if (on_cran()) {
        writeLines(paste('skipping test on CRAN: ', name))
        return(invisible())
    }
    test_setwd()
    library(pkg, character.only=TRUE)
    data <- data_func()
    expect_is(data, 'data.frame')

    # fit the model
    fit <- model_func(data)

    # metaprogram
    sas_code <- ml_func(fit)
    expect_is(sas_code, 'character')
    expect_more_than(nchar(sas_code), 10)
    cat(sas_code, file = paste('mlmeta_', name, '.sas', sep = ''))

    # predict in R
    pred <- predict_func(fit, newdata=data)
    expect_true(class(pred) %in% c('data.frame', 'numeric'))

    # export
    export <- data.frame(data, pred)
    expect_more_than(nrow(export), 10)
    write.csv(export, paste('mlmeta_',name,'.csv',sep = ''), row.names = FALSE, na = "")
}

#' Simulate classification data
simulate_classification_data <- function(n=1000) {
    library(caret)
    dat <- twoClassSim(n=n, linearVars=3)
    names(dat)[names(dat)=="Class"] <- "Y"
    dat
}

###
### test bagEarth
###

B <- 10

test_bagEarth_predict <- function (fit, newdata) {
    # bagged earth prediction
    r_pred_all <- predict(fit, newdata=newdata)

    # individual earth predictions
    if ('factor' == class(newdata$Y))
        type = 'response'
    else if ('numeric' == class(newdata$Y))
        type = 'link'
    r_pred_individual <- as.data.frame(sapply(1:B,
        function(n) predict(fit$fit[[n]], newdata=newdata, type=type)))
    names(r_pred_individual) <- paste0('r_pred_',1:B)

    data.frame(r_pred_all, r_pred_individual)
}

# regression with no factors and no missing values
test_foo2sas('bagEarth_reg', 'caret',
    function() { simulate_regression_data(unordered_factor = FALSE, ordered_factor = FALSE, p_missing = 0) },
    function(data) { caret::bagEarth(Y ~ ., data=data, B=B) },
    function(fit) { bagEarth2sas(fit, drop = FALSE) },
    test_bagEarth_predict)

# classification
library(class)
test_foo2sas('bagEarth_class', 'caret',
    simulate_classification_data,
    function(data) { caret::bagEarth(Y ~ ., data=data, B=B, glm=list(family=binomial)) },
    function(fit) { bagEarth2sas(fit, drop = FALSE) },
    test_bagEarth_predict)



###
### test cforest
###

test_foo2sas('cforest_reg', 'party',
    function() { simulate_regression_data(unordered_factor = FALSE, ordered_factor = FALSE, p_missing = 0) },
    function(data) { party::cforest(Y ~ ., data = data, controls = cforest_unbiased(ntree=5)) },
    function(fit) { cforest2sas(fit, drop = FALSE) },
    function(fit, newdata) { as.numeric(predict(fit)) })

###
### test ctree
###


test_foo2sas('ctree_reg', 'party',
    function() { simulate_regression_data(unordered_factor = TRUE, ordered_factor = FALSE, p_missing = 0) },
    function(data) { party::ctree(Y ~ ., data = data, controls = ctree_control(mincriterion = 0)) },
    ctree2sas,
    function(fit, newdata) { as.numeric(predict(fit)) })



###
### test earth
###

test_foo2sas('earth_reg', 'earth',
    function() { simulate_regression_data(unordered_factor = FALSE, ordered_factor = FALSE, p_missing = 0) },
    function(data) { earth(Y ~ ., data=data) },
    earth2sas,
    function(fit, newdata) { as.numeric(predict(fit)) })

test_foo2sas('earth_reg_interaction', 'earth',
    function() { simulate_regression_data(unordered_factor = FALSE, ordered_factor = FALSE, p_missing = 0) },
    function(data) { earth(Y ~ ., data = data, degree = 2) },
    earth2sas,
    function(fit, newdata) { as.numeric(predict(fit)) })
    
test_foo2sas('earth_class_link', 'earth',
    simulate_classification_data,
    function(data) { earth(Y ~ ., data=data, glm=list(family=binomial)) },
    earth2sas,
    function(fit, newdata) { as.numeric(predict(fit)) })

test_foo2sas('earth_class_response', 'earth',
    simulate_classification_data,
    function(data) { earth(Y ~ ., data=data, glm=list(family=binomial)) },
    function(fit) { earth2sas(fit, type="response") },
    function(fit, newdata) { as.numeric(predict(fit, type="response")) })


###
### test GBM
###

gbm.n.trees = 10

test_gbm_predict <- function(fit, newdata, n.trees = gbm.n.trees) {
    # the probability for all trees
    r_pred_all <- predict(fit, n.trees = n.trees)
    summary(r_pred_all)

    # the outputs of the individual trees
    r_pred_single <- as.data.frame(sapply(1:n.trees,
        function(n) predict(fit, n.trees=n, single.tree=TRUE)))

    # rename the columns
    names(r_pred_single) <- paste0('r_pred_',1:n.trees)

    # combine
    data.frame(r_pred_all, r_pred_single)
}

# regression with factors, ordinal factors, and missing values
test_foo2sas('gbm_reg', 'gbm',
    simulate_regression_data,
    function(data) { gbm(Y ~ ., data = data, distribution = "gaussian", n.trees = gbm.n.trees) },
    function(fit) { gbm2sas(fit, drop = FALSE) },
    test_gbm_predict)


###
### CRAN does not allow writing "anywhere else on the file system apart
### from the R session’s temporary directory"
###

if (on_cran()) {
    expect_equal(length(Sys.glob('~/mlmeta*csv')), 0)
    expect_equal(length(Sys.glob('~/mlmeta*sas')), 0)
    expect_equal(length(Sys.glob('/tmp/mlmeta*csv')), 0)
    expect_equal(length(Sys.glob('/tmp/mlmeta*sas')), 0)
} else {
    expect_more_than(length(Sys.glob('mlmeta*csv')), 0)
    expect_more_than(length(Sys.glob('mlmeta*sas')), 0)
}
