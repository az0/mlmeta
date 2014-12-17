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



#' Export a bagged MARS model to SAS
#'
#' Generate SAS DATA step code to predict the values of a bagged Multivariate
#' Adaptive Regression Splines (MARS) model from the \pkg{caret} package.
#'
#' @param fit a bagged MARS model trained by \code{\link[caret]{bagEarth}}.
#' @param name the name of the variable in which to store the prediction
#' @param drop whether to drop the variables for the individual trees

#' @export
#' @examples
#' require(caret)
#' trees.bagEarth <- bagEarth(Volume ~ ., data=trees, B=2)
#' trees.sas <- bagEarth2sas(trees.bagEarth)
#' cat(trees.sas, file="trees.sas")
bagEarth2sas <- function(fit, name = 'prediction', drop = TRUE)
{
    if (inherits(fit, 'train'))
        # tuned using the caret package
        fit <- fit$finalModel
    if (!inherits(fit, 'bagEarth'))
        stop('fit must be of type bagEarth')

    B <- length(fit$fit)

    ret <- character(0)
    for (b in 1:B) {
        name_b <- paste(name, '_', b, sep='')
        ret <- paste(ret, earth2sas(fit$fit[[b]], name_b), sep='\n')
    }

    ret <- paste(ret, name, ' = mean(of ',name,'_1-',name,'_',B,');\n', sep='')

    if (drop)
        ret <- paste(ret, '\ndrop ',name,'_1-',name,'_',B,';\n',sep='')

    ret
}
