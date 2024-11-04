# Paper:   Nickel mining reduced forest cover in Indonesia, but had mixed outcomes for well-being  

- Author: Michaela Guo Ying Lo (2024, One Earth) 
- Queries: m.lo@kent.ac.uk OR michaelalo39@gmail.com
- All code was tested using R version 4.4.1 (September, 2024)
- Code and Data availability can be found using the following link: 10.5281/zenodo.13884414 

## R scripts 
 This project provides the prepared data and code for the analysis and output. The R project contains three R script files which should be run as ordered:
- 1_gm_matching.R :  runs genetic (statistical) matching process and matches non-mining villages with mining villages (nickel and other mines)
                    Note!! takes two days with Intel i7-10700, RAM 64GB, option to run pre-generated data in "prepared_data" folder, see below 
                    Outputs will be stored in gm_output, including the covariate balance results and matched samples 

- 2_matched_outcome_combine.R :  combines the matched data with the outcome variables

- 3_regression_FigTab.R :  runs the regressions and produces results for figures and tables in both the manuscript and SI

## Data folders 
 We have two data folders: 
- gm_data:  stores unmatched data, containing information of villages
           that overlap mining concessions, and baseline characteristics
           needed for the statistical matching procedure in 1_gm_matching.R"

- prepared_data:  pre-generated matched data in folder for ease of
                 running regression, user can therefore skip "1_gm_matching.R" 
                 and move to "2_matched_outcome_combine.R"

 ### The matched data are saved with the following names:
 - Nickel_ : non-mining villages matched with villages overlapping nickel
           concessions
 - Other_ :  non-mining villages matched with villages overlapping other mine
           concessions

- _whole : over the 1-7 year period (2011-2018)
- _short1 : over the 1-3 year period (2011-2014)
- _short2: over the 1-3 year period (2014-2017)
- _long: over the 4-7 year period (2011-2018)

- _Low :  villages matched to mining villages with similar low poverty baseline
        conditions
- _High : villages matched to mining villages with similar high poverty baseline
        conditions

#END
