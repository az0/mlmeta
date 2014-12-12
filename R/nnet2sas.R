# Machine Learning Metaprogramming for R
# by Andrew Ziem
# Copyright (c) 2011, 2012 Compassion International
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

#' Export an neural network to SAS
#'
#' Generates SAS DATA step code to predict the values of an artificial
#' neural network.
#'
#' @param fit a neural network fitted by \code{\link[nnet]{nnet}} or 
#'   \code{\link[caret]{train}} with \code{method="nnet"}
#' @export
#' @examples
#' require(nnet)
#' # binary classification
#' iris2 <- iris[iris$Species %in% c('setosa', 'versicolor'),]
#' iris2$Species <- factor(iris2$Species)
#' iris2.nnet <- nnet(Species ~ ., data = iris2, size = 1, trace = FALSE)
#' iris2.sas <- nnet2sas(iris2.nnet)
#' cat(iris2.sas, file='iris.sas')

nnet2sas <- function(fit)
{
    if(!inherits(fit, c('train','nnet')))
        stop('nnet2sas requires an object returned by train() or nnet()')
    if(inherits(fit, 'train'))
    {
        if(!inherits(fit$finalModel, 'nnet'))
            stop('expecting train() was used with method="nnet"')
        if (any(!(fit$preProcess$methods %in% c('center','scale'))))
            stop('preprocessing support is limited to center and scale')
        nn <- fit$finalModel
        preprocess_methods <- fit$preProcess$methods
    } else {
        nn <- fit
        fit$preProcess$methods <- numeric(0)
    }

    sas <- paste("/* neural network size", paste(nn$n, collapse='-'), "*/\n")
    sas <- paste(sas, '/* inputs: ', paste(nn$coefnames, collapse=' '), ' */\n', sep='')
    sas <- paste(sas, '/* this macro handles extreme values */\n')
    sas <- paste(sas, '%macro logistic(z);\n')
    sas <- paste(sas, '1/(1+exp(min(max(-(&z),-500),500)))\n')
    sas <- paste(sas, '%mend;\n')

    # Define the input layer.
    # If there are factors, then in SAS you will have to manually change
    # something like 'pclass2nd' to 'pclass eq "2nd"'.
    # Also, this is the place to apply a range transformation (if any).

    for (input in 1:nn$n[1]){
        inputvar <- inputvar.org <- nn$coefnames[input]
        if ('center' %in% fit$preProcess$method)
        {
            inputvar <- paste('(',inputvar,'-',fit$preProcess$mean[[inputvar.org]],')')
        }
        if ('scale' %in% fit$preProcess$method)
        {
            inputvar <- paste(inputvar,'/',fit$preProcess$std[[inputvar.org]])
        }
        sas <- paste(sas, 'i', input,' = ', inputvar,';\n',sep='')
    }

    # notation:
    # zji refers to z^j_i where j is the layer and i is the unit
    # aji refers to a^j_i is the activation of g(z^j_i) where g is the sigmoid function

    # compute the hidden layer from the input layer
    for (h in 1:nn$n[2]) {
        unit.offset <- (nn$n[1]+1)*(h-1)+1
        z2 <- c()
        # bias unit (intercept)
        z2[1] <- paste('z2',h,' = ',nn$wts[unit.offset],sep='')
        # loop through input layer
        for (input in 1:nn$n[1]){
            z2[input+1] <- paste('(',nn$wts[unit.offset+input],' * i', input, ')', sep='')
        }
        sas <- paste(sas, paste(z2, collapse='+'),';\n',sep='')
        sas <- paste(sas, 'a2',h," = %logistic(z2",h,");\n", sep='')
    }

    # compute the output layer from the hidden layer
    output.offset <- (nn$n[1]+1)*(nn$n[2])
    z3<-c()
    # bias unit
    z3[1] <- paste('z31 = ',nn$wts[output.offset+1],sep='')
    # loop through the hidden layer
    for (h in 1:nn$n[2]) {
        z3[h+1] <- paste('(',nn$wts[output.offset + h + 1],' * a2', h, ')', sep='')
    }

    sas <- paste(sas, paste(z3, collapse='+'),';\n',sep='')
    sas <- paste(sas, "o = %logistic(z31);\n", sep='')

    # clean up temporary SAS variables
    sas <- paste(sas,
        paste('drop ',paste('i', 1:nn$n[1], collapse=' ', sep=''), ' ',
        paste('z2', 1:nn$n[2], collapse=' ', sep=''),' ',
        paste('a2', 1:nn$n[2], collapse=' ', sep=''),
        ' z31 ;\n', sep=''))


    return(sas)
}
