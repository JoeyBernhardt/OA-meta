# OA-meta
This repository contains data and code for a factorial meta-analysis of food and ocean acidification impacts on marine organisms (Brown et al., in revision).

Methods for calculating weighted effect sizes are from [Borenstein et al.](https://www.meta-analysis.com/downloads/Meta%20Analysis%20Fixed%20vs%20Random%20effects.pdf), and standard errors are calculated using corrections for small sample sizes in [Hedges et al. 1999, Ecology](http://onlinelibrary.wiley.com/doi/10.1890/0012-9658(1999)080%5B1150:TMAORR%5D2.0.CO;2/abstract)

There are 4 versions of all the scripts, figures, datasets.

1. 'main' refers to the dataset used in the main analysis
2. 'repeat' refers to the dataset in Ramajo, but corrected for proper set up of the lnRR
3. 'exact' refers to the dataset in Ramajo exactly
4. 'reduced' refers to the dataset used in the main analysis, reduced by choosing one CO2 level and one unit for each response variable


* [raw data](https://github.com/JoeyBernhardt/OA-meta/tree/master/data-raw) as extracted from papers
* [processed data](https://github.com/JoeyBernhardt/OA-meta/tree/master/data-processed) from data cleaning and wrangling
* [scripts](https://github.com/JoeyBernhardt/OA-meta/tree/master/R) for analysis, numbered in sequential order of analysis
* [figures](https://github.com/JoeyBernhardt/OA-meta/tree/master/figures)

