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
    nodes(mytree, parent_id)[[1]]$left$nodeID
}

# get right child
btree_right <- function(mytree, parent_id)
{
    nodes(mytree, parent_id)[[1]]$right$nodeID
}

# get prediction for this node
btree_prediction <- function(mytree, node_id)
{
    p <- nodes(mytree, node_id)[[1]]$prediction
    if (2 == length(p)) {
        return(p[2])
    }
    return (p)

}

# criteria for this node as a string
btree_criteria <- function(mytree, node_id, left)
{
    if (nodes(mytree, node_id)[[1]]$terminal)
    {
        return("(error: terminal node)");
    }
    if (nodes(mytree, node_id)[[1]]$psplit$ordered)
    {
        sp <- nodes(mytree, node_id)[[1]]$psplit$splitpoint
        vn <- nodes(mytree, node_id)[[1]]$psplit$variableName
        if (left) {
            op <- '<='
        } else {
            op <- '>'
        }
        return(paste(vn, op, sp))
    } else {
        psplit <- nodes(mytree, node_id)[[1]]$psplit
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
#' @param mytree a decision tree model trained by party::ctree()
#' @param node_id the initial node (used internally)
#' @param parent_criteria used internally
#' @export
#' @examples
#' require(party)
#' iris.ct <- ctree(Species ~ .,data = iris)
#' iris.sas <- ctree2sas(iris.ct)
#' cat(iris.sas)
ctree2sas <- function(mytree, node_id = 1, parent_criteria = character(0))
{
    if (nodes(mytree, node_id)[[1]]$terminal) {
        prediction <- btree_prediction(mytree, node_id)
        sprediction <- paste('else if', parent_criteria, 'then prediction =',prediction,';')
        return (sprediction)
    }

    left_node_id <- btree_left(mytree, node_id)
    right_node_id <- btree_right(mytree, node_id)

    if (is.null(left_node_id) != is.null(right_node_id)) {
        print('left node ID != right node id')
    }
    sprediction <- character(0)
    if (!is.null(left_node_id)) {
        new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, T), sep=' and ')
        if (1 == node_id)
            new_criteria <- btree_criteria(mytree, node_id, T)
        sprediction <- ctree2sas(mytree, left_node_id, new_criteria)
    }
    if (!is.null(right_node_id)) {
        new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, F), sep=' and ')
        if (1 == node_id)
            new_criteria <- btree_criteria(mytree, node_id, F)
        sprediction <- paste(sprediction, ctree2sas(mytree, right_node_id, new_criteria), sep='\n')
    }
    return(sprediction)
}
