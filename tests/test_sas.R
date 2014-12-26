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

require(mlmeta)

#' Indicate whether tests are running on CRAN
#' @return boolean
on_cran <- function() {
    Sys.getenv('NOT_CRAN') != ''
}

#' Set the working directory for testing purposes
#'
#' On CRAN, do nothing. On Linux, switch to \code{/tmp}.  On Windows,
#' switch to \code{\%TMP\%}.
test_setwd <- function() {
    # On CRAN do not use a temporary directory because (1) CRAN
    # policies forbid writing to /tmp and (2) CRAN does not have SAS.
    if (on_cran()) return()
    if (''==(tmpdir <- Sys.getenv('TMP')))
        tmpdir <- '/tmp'
    setwd(tmpdir)
    writeLines(paste('changing working directory to: ', getwd()))
}

#' A generic function for testing functions that generate SAS code
test_foo2sas <- function(name, pkg, data_func, model_func, ml_func, predict_funct) {
    writeLines(paste('testing',name,'from package', pkg))
    if (on_cran()) {
        writeLines(paste('skipping test on CRAN: ', pkg))
        return
    }
    test_setwd()
    library(pkg, character.only=TRUE)
    data <- data_func()
    fit <- model_func(data)
    sas_code = ml_func(fit)
    cat(sas_code, file = paste('mlmeta_', name, '.sas', sep = ''))
    pred <- predict_funct(fit, newdata=data)
    export <- data.frame(data, pred)
    write.csv(export, paste('mlmeta_',name,'.csv',sep = ''), row.names = FALSE, na = "")
}

###
### test bagEarth
###

B <- 10

test_bagEarth_predict <- function (fit, newdata) {
    # bagged earth prediction
    r_pred_all <- predict(fit, newdata=newdata)

    # individual earth predictions
    r_pred_individual <- as.data.frame(sapply(1:B,
        function(n) predict(fit$fit[[n]], newdata=newdata)))
    names(r_pred_individual) <- paste0('r_pred_',1:B)

    data.frame(r_pred_all, r_pred_individual)
}

# regression with no factors and no missing values
test_foo2sas('bagEarth_reg', 'caret',
    function() { simulate_regression_data(unordered_factor = FALSE, ordered_factor = FALSE, p_missing = 0) },
    function(data) { caret::bagEarth(Y ~ ., data = data, B = B) },
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



