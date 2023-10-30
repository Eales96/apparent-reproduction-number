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

## Data
The 'dat' sub-directory contains a fewer sources of data required for some of the functions in 'code/functions.R'.

1. 'dat/pcr_pos_dat.csv'
2. 'dat/ct_array.rds'
