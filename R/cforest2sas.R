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

#' Export a random forest to SAS
#'
#' Generate SAS DATA step code to predict the values of a random forest from
#' the \pkg{party} package.
#' 
#' Factors and missing values are not supported.
#'
#' \code{cforest2sas} averages the predictions of the trees like
#' \code{\link[randomForest]{randomForest}}, while \code{\link[party]{cforest}}
#' averages observation weights.
#'
#' @param fit a random forest trained using \code{\link[party]{cforest}}
#' @param name the name of the variable in which to store the prediction
#' @param drop whether to drop the variables for the individual trees
#' @export
#' @examples
#' require(party)
#' iris.ct <- cforest(Species ~ .,data = iris,
#'    controls = cforest_unbiased(ntree=5, mtry=2))
#' iris.sas <- cforest2sas(iris.ct)
#' cat(iris.sas, file="iris.sas")
cforest2sas <- function(fit, name = 'prediction', drop = TRUE)
{
    require(party)
    if (!isS4(fit)) stop('fit must be an S4 object')
    if (!is(fit, 'RandomForest')) stop('fit must be a RandomForest')
    n.trees <- length(fit@ensemble)
    stopifnot(n.trees>0)
    ret <- paste("/* cforest2sas(), ", R.Version()$version.string, ", party version ", installed.packages()["party",
            "Version"], " */\n", sep = "")
    for (n.tree in 1:n.trees) {
        one_tree <- new("BinaryTree")
        one_tree@tree <- party:::prettytree(fit@ensemble[[n.tree]], names(fit@data@get("input")))
        tree_name <- paste(name, '_', n.tree, sep='')
        one_tree.sas <- ctree2sas(one_tree, name = tree_name)
        ret <- paste(ret, paste('/* tree', n.tree,'*/', sep=''), one_tree.sas, sep='\n')
    }
    ret <- paste(ret, '\n\nif cmiss(of ',name,'_1-',name,'_',n.trees,') then put "WARNING: the predictions of one or more individual trees is missing" _N_;',sep='')
    ret <- paste(ret, '\n', name, ' = mean(of ',name,'_1-',name,'_',n.trees,');\n',sep='')
    if (drop)
        ret <- paste(ret, '\ndrop ',name,'_1-',name,'_',n.trees,';\n',sep='')
    ret
}
