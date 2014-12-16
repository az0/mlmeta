mlmeta
======

Machine Learning Metaprogramming for R

These functions export several kinds of machine learning models trained in R to other programming languages, so the models can score data in a separate environment without R.  Currently Base SAS is supported, and additional SAS licenses for SAS/IML, SAS/STAT, and Enterprise Miner are *not* required.  In some cases the models in SAS run faster and use less memory than in R.

Supported models:
* Conditional inference trees from `party`
* Random forests from `party`
* Multivariate Adaptive Regression Splines from `earth`
* Generalized Boosted Regression Models from `gbm`
* Artificial neural networks from `nnet`

Later I will post this package on CRAN.  Until then, use:
````r
require(devtools)
install_github("az0/mlmeta")
````

For an example usage, please see:
* [Train neural network in R, predict in SAS](http://heuristically.wordpress.com/2011/11/11/train-neural-network-in-r-predict-in-sas/)
* [Model decision tree in R, score in Base SAS](http://heuristically.wordpress.com/2011/10/11/model-decision-tree-in-r-score-in-sas/)

If you find these useful, please consider a donation to [Compassion International](http://www.compassion.com/donate-to-charity.htm?referer=131808).
