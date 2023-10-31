# apparent-reproduction-number
This repository contains the code to recreate the analysis in the pre-print [Differences between the true reproduction number and the apparent reproduction number of an epidemic time series](https://arxiv.org/abs/2307.03415).

## Code
The 'code' sub-directory contains the code required to run the analysis.
As the names suggest:
1. `code/figure1.R` produces Figure 1
2. `code/figure2.R` produces Figure 2
3. `code/figure3.R` produces Figure 3
4. `code/figure4.R` produces Figure 4
5. `code/figure5.R` produces Figure 5

All figure scripts depend on the functions in the script 'code/functions.R'

The script 'code/get_ct_dist.R' produces the data in the format of the file 'dat/ct_array.rds' if given as an input a stan model fit from [Hay et al. 2022](https://elifesciences.org/articles/81849) with the supporting github repository [gradlab/SC2-kinetics-immune-history](https://github.com/gradlab/SC2-kinetics-immune-history). More details are given below.

## Data
The 'dat' sub-directory contains a few sources of data required for some of the functions in 'code/functions.R'.

1. 'dat/pcr_pos_dat.csv' describes the probability of testing positive (y-column) as a function of days post-infection. The data was obtained from [Hellewell et al. 2021](https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-021-01982-x) 
2. 'dat/ct_array.rds'
