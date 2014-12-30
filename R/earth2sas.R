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



#' Export a MARS model to SAS
#'
#' Generate SAS DATA step code to predict the values of a Multivariate
#' Adaptive Regression Splines (MARS) model from the \pkg{earth} package.
#'
#' This function supports regression and binary classification (when
#' \code{\link[earth]{earth}} is called with \code{glm=list(family=binomial)}.
#' 
#' This function supports only numeric variables, so any factors must first be
#' converted to numeric variables (as \code{\link[caret]{train}} normally does).
#'
#' Interactions (\code{degree > 1)} are supported.
#'
#' @param fit a MARS model trained by \code{\link[earth]{earth}}.  It may
#' be tuned using \code{\link[caret]{train}}.
#' @param name the name of the variable in which to store the prediction
#' @param type For classification models \code{type=link} gives log-odds, while
#'  \code{type=response} gives probabilities.
#' @export
#' @examples
#' require(earth)
#' trees.earth <- earth(Volume ~ ., data=trees)
#' trees.sas <- earth2sas(trees.earth)
#' cat(trees.sas, file="trees.sas")
earth2sas <- function(fit, name='prediction', type='link')
{
    if (inherits(fit, 'train'))
        # tuned using the caret package
        fit <- fit$finalModel
    if (!inherits(fit, 'earth'))
        stop('fit must be of type earth')

    require(earth)
    ret <- paste(name, ' = ', format(fit, style="max", digits=15), ';\n')
    if ('response' == type)
        ret <- paste(ret, name, '= 1/(1+exp(min(max(-(',name,'),-500),500)));\n', sep='')

    ret
}
