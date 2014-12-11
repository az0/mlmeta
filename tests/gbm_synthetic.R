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
require(gbm)

# store .sas and .csv files in temporary folder
if (''==(tmpdir <- Sys.getenv('TMP')))
    tmpdir <- '/tmp'
setwd(tmpdir)
writeLines(paste('writing to directory: ', getwd()))

data <- simulate_regression_data(n = 1000, unordered_factor = TRUE, ordered_factor = TRUE)

n.trees <- 100

fit <- gbm(Y ~ . , data=data, n.trees=n.trees, distribution="gaussian")
fit
code <- gbm2sas(fit, drop = FALSE)
cat(code, file='synthetic.sas')

# the probablity for all trees
r_pred_all <- predict(fit, n.trees=n.trees)
summary(r_pred_all)

# the outputs of the individual trees
r_pred_single <- as.data.frame(sapply(1:n.trees, 
    function(n) predict(fit, n.trees=n, single.tree=TRUE)))

# rename the columns
names(r_pred_single) <- paste0('r_pred_',1:n.trees)

# export the data set and predictions
export <- data.frame(data, r_pred_all, r_pred_single)
write.csv(export, 'synthetic.csv', row.names=FALSE, na="")
