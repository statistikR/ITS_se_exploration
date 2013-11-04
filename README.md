ITS_se_exploration
==================

This program simulates data and runs interrupted time-series models on that data. The main focus of the program is to evaluate if the ITS models are correctly specified and if the standard errors are correctly estimated. Additionally, the program should help you to evaluate how the model estimation behaves if key factors such as # of baseline years, sample size, etc. change.

The program runs OLS and multi-level regressions and evaluates if the standard errors of the multi-level model are calculated correctly as well as other features of interrupted time-series analysis.

to do:

## next things to do ====
# make function more flexible by allowing to pass all arguments as variables
# experiment with models with multiple comparison schools and treatment schools with nested student effects and cohort effects ####


## function needed to calculate p-values with lmer package
# http://blog.lib.umn.edu/moor0554/canoemoore/2010/09/lmer_p-values_lrt.html
# currently we calculatea an approximate p-value based on a t-distribution
