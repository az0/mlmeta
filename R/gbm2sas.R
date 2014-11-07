# Machine Learning Metaprogramming for R
# by Andrew Ziem
# Copyright (c) 2012 Compassion International
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

# Recursively descend the tree.  For internal use.
gbm2sas.descend <- function(fit, tree.n, this.node_id = 0, parent.criteria='')
{
	#cat(paste('debug: gbm2sas.descend(fit,', tree.n, ', ', this.node_id, ',',parent.criteria,')\n'))

	# sanity checks
	stopifnot('gbm' == class(fit))
	stopifnot(tree.n > 0)
	stopifnot(tree.n <= fit$n.trees)
	stopifnot(this.node_id >= 0)
	stopifnot(is.character(parent.criteria))

	# the splitting rule
	split.rule <- pretty.gbm.tree(fit, i.tree = tree.n)[this.node_id+1,]
	stopifnot(is.data.frame(split.rule))
	stopifnot(dim(split.rule) ==c(1,8))

	# child nodes
	left.node.id <- split.rule[,'LeftNode']
	right.node.id <- split.rule[,'RightNode']
	missing.node.id <- split.rule[,'MissingNode']

	# more sanity checks
	if (-1 == split.rule[,'SplitVar'] && -1 != left.node.id) stop('SplitVar -1 but LeftNode is not')
	if (-1 == left.node.id && -1 != right.node.id) stop('LeftNode -1 but RightNode is not')
	if (-1 != left.node.id && -1 == right.node.id) stop('LeftNode not -1 but RightNode is')

	# Is this a terminal node?
	if (-1 == split.rule[,'SplitVar']) {
		# This a terminal node, so make a prediction
		return(paste('else if', parent.criteria, ' then ',paste('gbm',tree.n,sep=''), ' = ', split.rule[,'Prediction'], '; /* terminal node ',this.node_id,' */\n',sep=''))
	} else {
		# This is not a terminal node

		# find the name of the splitting variable
		# SplitVar is zero based, so add 1
		# split.var <- attr(fit$Terms,'term.labels')[split.rule[,'SplitVar']+1]
		split.var <- fit$var.names[split.rule[,'SplitVar']+1]

		# the data type for the splitting variable
		# This vector starts with the dependent variable.
		if (is.null(fit$Terms))
		{
			# Without GBM formula interface, there are no factors.
			data.class <- 'numeric'
		}
		else
		{
			# With GBM formula interface, there are factors.
			data.class <- attr(fit$Terms,'dataClasses')[split.rule[,'SplitVar']+2]
		}


		# splitting conditions for this level
		split.code.pred <- split.rule[,'SplitCodePred']
		if ('numeric' == data.class) {
			# SAS evaluates missing value "x" in "x < y" as true
			left.condition <- paste('not missing(',split.var,') and ',split.var, '<', split.code.pred)
			right.condition <- paste(split.var, '>=', split.code.pred)
		} else if ('factor' == data.class) {
			var.levels <- fit$var.levels[[split.rule[,'SplitVar'] + 1]]
			c.splits <- fit$c.splits[[split.code.pred + 1]]
			left.levels <- var.levels[c.splits == -1]
			right.levels <- var.levels[c.splits == 1]
			left.condition <- paste(split.var, ' in (\'', paste(left.levels, collapse="', '"), '\')', sep='')
			right.condition <- paste(split.var, ' in (\'', paste(right.levels, collapse="','"), '\')', sep='')
		} else if ('ordered' == data.class) {
			var.levels <- fit$var.levels[[split.rule[,'SplitVar'] + 1]] # all the variable levels
			var.levels.left <- paste(var.levels[1:length(var.levels) < (split.code.pred +1)], collapse="','") # levels included in the left split
			var.levels.right <- paste(var.levels[1:length(var.levels) < (split.code.pred + 1)], collapse="','") # levels included in the right split
			left.condition <- paste(split.var, ' in (\'', var.levels.left, '\')', sep='')
			right.condition <- paste(split.var, ' in (\'', var.levels.right, '\')', sep='')
		} else {
			# FIXME: support logical
			stop(paste('unsupported data class:', data.class))
		}
		missing.condition <- paste('missing(',split.var,')')

		# separator differs for first level
		my.sep <- ifelse(0 == this.node_id, ' ', ' and ')

		# descend to left
		left.criteria <- paste(parent.criteria, left.condition, sep=my.sep)
		left.expanded <- gbm2sas.descend(fit, tree.n, left.node.id, left.criteria)

		# descend missing
		missing.criteria <- paste(parent.criteria, missing.condition, sep=my.sep)
		missing.expanded <-  gbm2sas.descend(fit, tree.n, missing.node.id, missing.criteria)

		# descend to right node
		right.criteria <- paste(parent.criteria, right.condition, sep=my.sep)
		right.expanded <-  gbm2sas.descend(fit, tree.n, right.node.id, right.criteria)

		# combine
		return (paste(left.expanded, missing.expanded, right.expanded, sep=''))
	}
}


#` Export a Gradient Boosted Regression Model (GBM) to SAS
#`
#` @param fit the model fitted by gbm::gbm()
#` @param n.trees the number of trees to export
#` @export
gbm2sas <- function(fit, n.trees = fit$n.trees) {
    # Sanity checks
    stopifnot("gbm" == class(fit))
    data.classes <- attr(fit$Terms, "dataClasses")
    if (is.null(fit$initF))
        stop("Missing intercept term. Use gbm() instead of gbm.fit().")
    stopifnot(fit$n.trees >= n.trees)
    stopifnot(1 == length(fit$distribution))
    if (!fit$distribution[[1]] %in% c("guassian", "bernoulli"))
        warning("only Bernoulli and Gaussian distributions have been tested")

    # if ('ordered' %in% unlist(types)) stop('ordered factor not supported')

    # General information
    ret <- "/* gbm2sas() */\n"
    ret <- paste(ret, "/* n.trees=", n.trees, ", interaction depth=", fit$interaction.depth,
        ", shrinkage=", fit$shrinkage, ", bag fraction=", fit$bag.fraction, ", n.minobsinnode=",
        fit$n.minobsinnode, " */\n", sep = "")
    ret <- paste(ret, "/* ", R.Version()$version.string, ", gbm version ", installed.packages()["gbm",
        "Version"], " */\n", sep = "")

    # Check that factor levels in SAS match those we have now in R.  This applies
    # only to the gbm formula interface.
    if (!is.null(fit$Terms)) {
        data.classes <- attr(fit$Terms, "dataClasses")
        data.classes <- data.classes[2:length(data.classes)]  # remove independent variable
        for (i in 1:length(data.classes)) {
            if (data.classes[i] %in% c("factor", "ordered")) {
                var.name <- names(data.classes[i])
                ret <- paste(ret, "if ", var.name, " not in ('", paste(fit$var.levels[[i]],
                  collapse = "','"), "', ' ') then do; put 'ERROR: ", var.name, " has unexpected value ' ",
                  var.name, "=; abort; end;\n", sep = "")
            }
        }
    }

    # Separate sanity check from first tree
    ret <- paste(ret, "\n")

    # Loop through trees
    for (tree.n in c(1:n.trees)) {
        this.tree <- gbm2sas.descend(fit, tree.n)
        this.tree <- substr(this.tree, 6, nchar(this.tree))  # remove the very first 'else'
        ret <- paste(ret, this.tree, "else do; put \"ERROR: no rules match for tree ",
            tree.n, " \" _all_; abort; end;\n\n", sep = "")
    }

    # Sum over the trees ret <- paste(ret, 'gbm = ', fit$initF, ' + ',
    # paste('gbm',1:n.trees, sep='', collapse=' + '), ';\n', sep='') New versions of
    # SAS have a very compact way of writing the summation
    ret <- paste(ret, "gbm = ", fit$initF, " + sum(of gbm1-gbm", n.trees, ");\n",
        sep = "")


    # Clean up temporary variables ret <- paste(ret, 'drop ', paste('gbm',1:n.trees,
    # sep='', collapse=' '), ';\n',sep='')
    ret <- paste(ret, "drop gbm1-gbm", n.trees, ";\n", sep = "")

    # Done
    ret <- paste(ret, "\n/* End of gbm2sas() */\n", sep = "")

    ret
}
