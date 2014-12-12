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


# Create a model and data to check that party::cforest gives the same
# values in R and SAS.

require(mlmeta)
require(party)

# store .sas and .csv files in temporary folder
if (''==(tmpdir <- Sys.getenv('TMP')))
    tmpdir <- '/tmp'
setwd(tmpdir)
writeLines(paste('writing to directory: ', getwd()))

data <- simulate_regression_data(n = 1000,
    unordered_factor = FALSE,
    ordered_factor = FALSE,
    p_missing = 0)
fit <- party::cforest(Y ~ . , data=data,
    controls = cforest_unbiased(ntree=5))

code <- cforest2sas(fit, drop=FALSE)
cat(code, file='cforest.sas')
r_pred <- as.numeric(predict(fit))
export <- data.frame(data, r_pred=r_pred)
write.csv(export, 'cforest.csv', row.names=FALSE, na="")
