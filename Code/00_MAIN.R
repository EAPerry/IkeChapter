#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Natural Disasters and Giving: Book Chapter -----------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02
# 
# Purpose: The purpose of this script is to clean, organize, and summarize key 
# data related to the impact of natural disasters on local charitable giving. 
# Primary goals include: (1) basic data cleaning on nonprofit data, (2) 
# summarizing and visualizing the data, and (3) running the econometric analysis
# on the data.
# 
# 
# Input files: 
# 
# nccs.csv : Consolidated data from NCCS. Raw data from NCCS is too large to 
#     upload to GitHub, so this is the consolidated data. 
# 
# 2020_Planning_Data.csv : County-level census data from the 2010 decennial
#     Census and the 2014 American Community Survey. Use selected variables for
#     perspective on demographic makeup of counties.
#
# co-est2009-alldata.csv : County-level annual population estimates from 2000 - 
#     2009. 
#
# co-est2019-alldata.csv : County-level annual population estimates from 2010 - 
#     2019. 
#     
# sf12010countydistance500miles.csv : This file contains the distances of every 
#     pair of counties that are within 500 miles of each other.
# 
# FEMA_Disasters.csv : All disaster declaration records at the county level from
#     FEMA. 
# 
# hurricanes.csv : A hand-cleaned file that lists the disaster numbers for all
#     relevant hurricanes.
# 
# 
# Output files: 
# 
# cleaned_data.csv : Cleaned data incorporating the data listed above. This 
#     is panel data with each observation representing a county-year.
#
# summary_stats.tex : Descriptive statistics table.
#
# All the LaTeX tables in the Results/Regression Tables subdirectory.
# All the figures in the Results/Figures subdirectory.
# All the figures in the Results/Robustness Checks subdirectory.
# 
# Outline: (Crtl + Shift + O in RStudio) 
#   1. Code to Change When Replicating
#   2. Importing Packages and Setup
#   3. Reading and Processing Data
#   4. Data Summary & Visualization
#   5. Analysis
#   6. Placebo Tests
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Code to Change When Replicating ---------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Set your working directory
#setwd("C:/Users/eaper/CoeRA/IkeChapter") #not needed - tied to Rproj.

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Importing Packages and Setup ------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Basic data manipulation and importing
library(tidyverse)
library(lubridate)
library(arrow)
library(fastDummies)
library(callr)
# Spatial
library(sf)
library(tidycensus)
library(tmap)
library(tmaptools)
# Estimation
library(estimatr)
library(plm)
# Aesthetics
library(stargazer)
library(ggpubr)
library(NatParksPalettes)

options(scipen = 9)    # Get rid of scientific notation

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Section 1: Data Cleaning -----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Data prep
source("Code/01_clean_nccs.R")
source("Code/02_clean_census.R")
source("Code/03_clean_hurricane.R")
source("Code/04_merge_all.R")

# Summary Statistics & Visualizations
source("Code/05_summary_figs.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Section 2: Analysis ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("Code/06_new_regressions.R")
source("Code/07_panel_balance.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
