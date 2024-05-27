# A Shock to the Commons: Nonprofits and Philanthropy in the Wake of Climate Disaster

Contributors: Brenna Jungers, Evan Perry, Drew Westberg


This repository contains the code and data to replicate the empirical work in the book chapter "A Shock to the Commons: Nonprofits and Philanthropy in the Wake of Climate Disaster".



Input files:

nccs.csv : Consolidated data from NCCS. Raw data from NCCS is too large to
    upload to GitHub, so this is the consolidated data.

2020_Planning_Data.csv : County-level census data from the 2010 decennial
    Census and the 2014 American Community Survey. Use selected variables for
    perspective on demographic makeup of counties.

co-est2009-alldata.csv : County-level annual population estimates from 2000 -
    2009.

co-est2019-alldata.csv : County-level annual population estimates from 2010 -
    2019.

sf12010countydistance500miles.csv : This file contains the distances of every
    pair of counties that are within 500 miles of each other.

FEMA_Disasters.csv : All disaster declaration records at the county level from
    FEMA.

hurricanes.csv : A hand-cleaned file that lists the disaster numbers for all
    relevant hurricanes.


Output files:

cleaned_data.csv : Cleaned data incorporating the data listed above. This
    is panel data with each observation representing a county-year.

summary_stats.tex : Descriptive statistics table.

All the LaTeX tables in the Results/Regression Tables subdirectory.
All the figures in the Results/Figures subdirectory.
All the figures in the Results/Robustness Checks subdirectory.




