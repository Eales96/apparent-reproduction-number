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

The script 'code/get_ct_dist.R' produces the data in the format of the file 'dat/ct_array.rds' if given as an input a stan model fit from [Hay et al. 2022](https://elifesciences.org/articles/81849) with the supporting github repository [gradlab/SC2-kinetics-immune-history](https://github.com/gradlab/SC2-kinetics-immune-history).

## Dependent data
The file 'dat/ct_array.rds' contains an array of 1000 simulated Ct trajectories. The Ct trajectories were obtained by following the work of [Hay et al. 2022](https://elifesciences.org/articles/81849) using the code available at [gradlab/SC2-kinetics-immune-history](https://github.com/gradlab/SC2-kinetics-immune-history). The code used to produce the model fit is found at 'https://github.com/gradlab/SC2-kinetics-immune-history/blob/main/code/run_analysis.R'. The code by default only runs the 10th parameter set. This parameter set fits the Ct model (Hay et al. 2022) to the individual-level data from the paper subset by antibody titre (low:<250AU, or high:>250AU) and vaccine status (boosted or not boosted). The Ct trajectory model fits population level parameters (mean and standard deviation) for clearance duration, proliferation duration, and peak viral load (minimum Ct value). The mean of the population parameter is also adjusted between sub-groups (antibody titre and vaccine status). When simulating the Ct trajectories we use the population parameters for the low antibody titre, and non-boosted group. We simulate the trajectories using the code in 'code/get_ct_dist.R'. The Ct model fit used in simulating these trajectories is too large to host on github and so will need to be recreated using the code at the github repository [gradlab/SC2-kinetics-immune-history](https://github.com/gradlab/SC2-kinetics-immune-history).

