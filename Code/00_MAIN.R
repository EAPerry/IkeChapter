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
# not needed - tied to Rproj.
# setwd("C:/Users/eaper/CoeRA/IkeChapter") 

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
library(fixest)
library(marginaleffects)
# Aesthetics
library(stargazer)
library(ggpubr)
library(NatParksPalettes)

options(scipen = 99)    # Get rid of scientific notation

# Set some color palettes
theme_set(theme_bw())
options(ggplot2.continuous.colour= natparks.pals("Yellowstone", 6, type = "continuous"))
options(ggplot2.continuous.fill = natparks.pals("Yellowstone", 6, type = "continuous"))
options(ggplot2.discrete.colour= natparks.pals("Yellowstone", 6, type = "discrete"))
options(ggplot2.discrete.fill = natparks.pals("Yellowstone", 6, type = "discrete"))


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
