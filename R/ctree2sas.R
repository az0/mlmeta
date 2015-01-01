# Machine Learning Metaprogramming for R
# by Andrew Ziem
# Copyright (c) 2011 Compassion International
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


# get node ID for left child
btree_left <- function(mytree, parent_id)
{
    party::nodes(mytree, parent_id)[[1]]$left$nodeID
}

# get right child
btree_right <- function(mytree, parent_id)
{
    party::nodes(mytree, parent_id)[[1]]$right$nodeID
}

# get prediction for this node
btree_prediction <- function(mytree, node_id)
{
    p <- party::nodes(mytree, node_id)[[1]]$prediction
    if (2 == length(p)) {
        return(p[2])
    }
    return (p)

}

# criteria for this node as a string
btree_criteria <- function(mytree, node_id, left)
{
    if (party::nodes(mytree, node_id)[[1]]$terminal)
    {
        return("(error: terminal node)");
    }
    if (party::nodes(mytree, node_id)[[1]]$psplit$ordered)
    {
        sp <- party::nodes(mytree, node_id)[[1]]$psplit$splitpoint
        vn <- party::nodes(mytree, node_id)[[1]]$psplit$variableName
        if (left) {
            op <- '<='
        } else {
            op <- '>'
        }
        return(paste(vn, op, sp))
    } else {
        psplit <- party::nodes(mytree, node_id)[[1]]$psplit
        if (left){
            l <- as.logical(psplit$splitpoint)
        } else {
            l <- as.logical(!psplit$splitpoint)
        }

        r <- paste(attr(psplit$splitpoint, 'levels')[l], sep='', collapse="','")
        return(paste(psplit$variableName, " in ('", r,"')", sep=''))
    }
}

#' Export a decision tree to SAS
#'
#' Generate SAS DATA step code to predict the values of a conditional inference tree
#' from the \pkg{party} package.
#'
#' Unordered factors are supported, while ordered factors and missing values are
#' not supported.
#'
#' @param mytree a decision tree model trained by \code{\link[party]{ctree}}
#' @param name the name of the variable in which to store the prediction
#' @param node_id the initial node (used internally)
#' @param parent_criteria used internally
#' @export
#' @examples
#' require(party)
#' iris.ct <- ctree(Species ~ . , data = iris)
#' iris.sas <- ctree2sas(iris.ct)
#' cat(iris.sas, file="iris.sas")
ctree2sas <- function(mytree, name = 'prediction', node_id = 1, parent_criteria = character(0))
{
    require(party)
    if (party::nodes(mytree, node_id)[[1]]$terminal) {
        ret <- btree_prediction(mytree, node_id)
        ret <- paste('else if', parent_criteria, 'then', name,'=',ret,'; /* node',node_id,'*/')
        return (ret)
    }

    left_node_id <- btree_left(mytree, node_id)
    right_node_id <- btree_right(mytree, node_id)

    if (is.null(left_node_id) != is.null(right_node_id)) {
        print('left node ID != right node id')
    }
    if (!is.null(left_node_id)) {
        new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, T), sep=' and ')
        if (1 == node_id)
            new_criteria <- btree_criteria(mytree, node_id, T)
        ret <- ctree2sas(mytree, name=name, node_id=left_node_id, parent_criteria=new_criteria)
    }
    if (!is.null(right_node_id)) {
        new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, F), sep=' and ')
        if (1 == node_id)
            new_criteria <- btree_criteria(mytree, node_id, F)
        ret <- paste(ret, ctree2sas(mytree, name=name, node_id=right_node_id, parent_criteria=new_criteria), sep='\n')
    }

    if (1 == node_id) {
        # remove the very first 'else '
        ret <- substr(ret, 6, nchar(ret))

        # add general information
        ret <- paste("/* ctree2sas(), ", R.Version()$version.string, ", party version ", installed.packages()["party",
            "Version"], " */\n", ret, sep = "")
    }

    # return
    return(ret)
}
