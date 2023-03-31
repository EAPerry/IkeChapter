#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Natural Disasters and Giving: Book Chapter -----------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-03-30
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
setwd("C:/Users/eaper/CoeRA/IkeChapter")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Importing Packages and Setup ------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Basic data manipulation and importing
library(tidyverse)
library(lubridate)
library(arrow)
library(fastDummies)
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
## Reading and Processing Data -------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### NCCS Data ------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# The original files with all the NCCS data are too large to upload to GitHub. 
# I've commented out the code needed to process the NCCS data---let me know if 
# you want to use the raw NCCS data and I'll send it.
# 
# #### Consolidate the NCCS Trend Files ------------------------------------------
# 
# my_vars1 <- c(
#   "CONT",
#   "CoreSrc",
#   "EIN",
#   "EXPS",
#   "FIPS",
#   "FISYR",
#   "FNDNCD",
#   "GRPROF",
#   "INVINC",
#   "ASS_EOY",
#   "LEVEL1",
#   "LEVEL2",
#   "LEVEL3",
#   "LEVEL4",
#   "LIAB_EOY",
#   "MAJGRPB",
#   "NAICS",
#   "NAME",
#   "NETINC",
#   "nteeFinal",
#   "ntmaj10",
#   "ntmaj12",
#   "ntmaj5",
#   "OTHINC",
#   "OUTNCCS",
#   "RULEDATE",
#   "STATE",
#   "SUBSECCD",
#   "TAXPER",
#   "TOTREV",
#   "ZIP5"
# )
# 
# my_vars2 <- c(
#   "P1TCONT",
#   "CoreSrc",
#   "EIN",
#   "P1TOTEXP",
#   "FIPS",
#   "FISYR",
#   "FNDNCD",
#   "P1GINVPF",
#   "P1NETINV",
#   "P2TOTAST",
#   "LEVEL1",
#   "LEVEL2",
#   "LEVEL3",
#   "LEVEL4",
#   "P2TLIABL",
#   "MAJGRPB",
#   "NAICS",
#   "NAME",
#   "P1NADINC",
#   "nteeFinal",
#   "ntmaj10",
#   "ntmaj12",
#   "ntmaj5",
#   "P1OTHINC",
#   "OUTNCCS",
#   "RULEDATE",
#   "STATE",
#   "SUBSECCD",
#   "TAXPER",
#   "P1TOTREV",
#   "ZIP5"
# )
# 
# 
# df1 <- read_parquet(
#   "Data/NCCS/co_fy_trends.parquet",
#   col_select = all_of(my_vars1)
# )
# 
# df2 <- read_parquet(
#   "Data/NCCS/pc_fy_trends.parquet",
#   col_select = all_of(my_vars1)
# )
# 
# df3 <- read_parquet(
#   "Data/NCCS/pf_fy_trends.parquet",
#   col_select = all_of(my_vars2)
# )
# 
# # The private foundation variable names do not match the other files
# df3 <- df3 %>% 
#   rename(
#     GRPROF = P1GINVPF,
#     NETINC = P1NADINC,
#     INVINC = P1NETINV,
#     OTHINC = P1OTHINC,
#     CONT = P1TCONT,
#     EXPS = P1TOTEXP,
#     TOTREV = P1TOTREV,
#     LIAB_EOY = P2TLIABL,
#     ASS_EOY = P2TOTAST
#   )
# 
# # Combine
# nccs <- bind_rows(df1, df2, df3)
# rm(df1, df2, df3, my_vars1, my_vars2)
# gc()
# 
# 
# #### Clean the NCCS Data -------------------------------------------------------
# 
# # We can only look at the period from 2000 to 2014
# nccs <- nccs[nccs$FISYR >= 2000, ]
# nccs <- nccs[nccs$FISYR <= 2014, ]
# 
# # Remove any observations with a missing county
# nccs <- nccs[!is.na(nccs$FIPS),]
# nccs <- nccs[!(nccs$FIPS %in% c("AA","AE","AP")),]
# 
# # Also remove some useless variables
# nccs <- nccs %>% 
#   select(-c(          
#     "CoreSrc",
#     "FNDNCD",
#     "OUTNCCS",
#     "TAXPER",
#     "RULEDATE"
#   ))
# 
# # Group and summarize variables for each county and fiscal year
# nccs <- nccs %>% 
#   group_by(FIPS, FISYR) %>% 
#   summarize(
#     # Concentration Indexes
#     # For now, do not group by industry
#     HHI_CONT = sum((CONT/sum(CONT, nar.rm=T))^2, na.rm = T),
#     HHI_EXPS = sum((EXPS/sum(EXPS, nar.rm=T))^2, na.rm = T),
#     HHI_GRPROF = sum((GRPROF/sum(GRPROF, nar.rm=T))^2, na.rm = T),
#     HHI_INVINC = sum((INVINC/sum(INVINC, nar.rm=T))^2, na.rm = T),
#     HHI_REV = sum((TOTREV/sum(TOTREV, nar.rm=T))^2, na.rm = T),
#     HHI_ASS = sum((ASS_EOY/sum(ASS_EOY, nar.rm=T))^2, na.rm = T),
#     # Base sums
#     NONPROFITS = n(),
#     CONT = sum(CONT, na.rm = T),
#     EXPS = sum(EXPS, na.rm = T),
#     GRPROF = sum(GRPROF, na.rm = T),
#     INVINC = sum(INVINC, na.rm = T),
#     OTHINC = sum(OTHINC, na.rm = T),
#     TOTREV = sum(TOTREV, na.rm = T),
#     TOTASS = sum(ASS_EOY, na.rm = T)
#   ) %>% 
#   ungroup()
# 
# # Remove unused data from memory
# gc()
# 
# # Just in case: Be sure to remove counties with a missing FIPS code
# nccs <- nccs[!is.na(nccs$FIPS),]
# 
# # Create the unique ID variable
# nccs$co_year <- paste(nccs$FIPS, nccs$FISYR, sep="-")
# 
# # Write to another file that will be small enough for GitHub
# write_csv(nccs, "Data/nccs.csv", na="")

# Read in the cleaned NCCS data 
nccs <- read_csv("Data/nccs.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Census Data ----------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Read in data from the planning data -- but only certain variables
census <- read_csv(
  "Data/2020_Planning_Data.csv",
  col_select = c(
    "GIDSTCO",
    "County_name",
    "LAND_AREA"
  )
)

# Remove any problematic non-distinct observations
census <- census %>% 
  distinct(GIDSTCO, .keep_all = T)

# This data is not year specific, so the merging variable here will be FIPS
census <- census %>% 
  rename(FIPS = GIDSTCO)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Population Data ------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

filename <- c("Data/co-est2009-alldata.csv", "Data/co-est2019-alldata.csv")
df_list <- as.list(rep(NA, 2))

for (i in 1:2){
  
  my_file <- filename[i]
  
  df <- read_csv(my_file)
  df <- df[df$COUNTY != "000",]
  
  df$FIPS <- paste(df$STATE, df$COUNTY, sep="")
  
  keep <- grepl("FIPS", names(df)) | grepl("POPESTIMATE", names(df))
  
  df <- df[,keep]
  
  temp_vec <- names(df)
  temp_vec <- temp_vec[temp_vec != "FIPS"]
  
  df <- df %>% 
    pivot_longer(cols = all_of(temp_vec), 
                 names_to = "year", 
                 values_to = "pop") %>% 
    mutate(
      year = as.integer(str_remove(year, "POPESTIMATE"))
    )
  
  df_list[[i]] <- df
  
}

pop <- bind_rows(df_list)

pop$co_year <- paste(pop$FIPS, pop$year, sep="-") 
pop <- pop %>% select(co_year, pop)

rm(df, df_list, filename, i, keep, my_file, temp_vec)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Hurricane Data -------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Read in the FEMA data
fema <- read_csv("Data/FEMA_Disasters.csv")

# This is a hand-cleaned data file that lists the qualifying disaster numbers
hurricanes <- read_csv("Data/hurricanes.csv")

# Get the full FIPS
fema$FIPS <- paste(fema$fipsStateCode, fema$fipsCountyCode, sep="")
fema <- fema %>% 
  rename(year = fyDeclared)

fema <- select(fema, fipsStateCode, FIPS, disasterNumber, year)

# Get all the combinations of the hurricanes/disaster numbers and counties
# because some counties are hit by multiple storms this will be a "long" 
# dataframe
hurricanes <- merge(fema, hurricanes, by="disasterNumber")

# Get a list of the storms
storms <- unique(hurricanes$storm)

# Bring in this data on the distance between counties
dist <- read_csv("Data/sf12010countydistance500miles.csv")

# Set a distance---say, 250 miles
dist <- dist[dist$mi_to_county <= 250, ]

# We need to break this up by storm for a minute---we are going to identify 
# the control counties for each storm
split_df <- split(hurricanes, hurricanes$storm)

for (i in 1:length(split_df)){
  
  my_storm <- storms[i]
  
  # Get the FIPS for all counties with the relevant disaster declaration
  temp_df1 <- hurricanes %>% 
    filter(storm == my_storm)
  
  # Get the FIPS codes for any affected states
  my_states <- unique(temp_df1$fipsStateCode)
  
  # Get the FIPS for all counties in the listed states
  temp_df2 <- fips_codes %>% 
    filter(state_code %in% my_states) %>% 
    mutate(FIPS = paste(state_code, county_code, sep="")) %>% 
    select(FIPS)
  
  # Get the FIPS codes for all counties within 250 miles of an affected county
  temp_df3 <- dist[dist$county1 %in% temp_df1$FIPS,]
  temp_df3 <- data.frame(FIPS = unique(temp_df3$county2))
  
  # Put these together to determine our treatment and control groups
  temp_df1 <- merge(temp_df1, temp_df2, by="FIPS", all.x=T, all.y=T)
  temp_df1 <- merge(temp_df3, temp_df1, by="FIPS", all.x=T, all.y=T)
  
  # Clean up this microchasm of a dataframe
  temp_df1 <- temp_df1 %>% 
    select(FIPS, disasterNumber, storm, year)
  
  temp_df1$storm <- rep(my_storm, nrow(temp_df1))
  temp_df1$hit_year <- rep(min(temp_df1$year, na.rm = T), nrow(temp_df1))
  temp_df1$treatment <- ifelse(is.na(temp_df1$disasterNumber), 0, 1)
  
  temp_df1 <- temp_df1 %>% 
    select(-c("year","disasterNumber"))
  
  split_df[[i]] <- temp_df1
  rm(temp_df1, temp_df2, temp_df3, my_storm, my_states)
  
}

hurricanes <- bind_rows(split_df)
rm(i, split_df, fema, dist)

# long_df contains what we want our geographic sample to look like for all 
# potential hurricanes. For this chapter though, we only want to look at 
# Hurricane Ike.

hurricanes <- hurricanes[hurricanes$storm == "IKE",]

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Merging the Data -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Start forming the main dataframe by merging the NCCS and Census data
main_df <- merge(nccs, census, by="FIPS", all.x = T)

# Merge in the Population Data
main_df <- merge(main_df, pop, by="co_year", all.x=T)

# Merge in the hurricane data
main_df <- merge(main_df, hurricanes, by="FIPS", all.y=T)

# Remove the extra dataframes
rm(census, hurricanes, nccs, pop, storms)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Cleaning on the full dataset -----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# There are a collection of outcome variables that we should be sure to add to
# the data 

main_df <- main_df %>% 
  mutate(
    # # Nonprofits
    # npo_capita = NONPROFITS/pop,
    # npo_area = NONPROFITS/LAND_AREA,
    # # Contributions
    # cont_capita = CONT/pop,
    # cont_area = CONT/LAND_AREA,
    # cont_npo = CONT/NONPROFITS,
    # # Expenses
    # exps_capita = EXPS/pop,
    # exps_area = EXPS/LAND_AREA,
    # exps_npo = EXPS/NONPROFITS,
    # # Assets
    # ass_capita = TOTASS/pop,
    # ass_area = TOTASS/LAND_AREA,
    # ass_npo = TOTASS/NONPROFITS,
    # # Revenues
    # revs_capita = TOTREV/pop,
    # revs_area = TOTREV/LAND_AREA,
    # rev_npo = TOTREV/NONPROFITS,
    # Outcome Variables
    density = NONPROFITS/pop,
    concentration = concentration,
    wealth = TOTASS/NONPROFITS,
    distribution = DIST,
    output = EXPS/pop,
    use = CONT/NONPROFITS,
    dependence = CONT/TOTREV * 100
  ) %>% 
  rename(
    year = FISYR
  )

# Last thing to do before we output the full dataset is order the columns
my_order <- c(
  "FIPS",
  "year",
  "co_year",
  "County_name",
  "storm",
  "hit_year",
  "treatment",
  "NONPROFITS",
  "CONT",
  "EXPS",
  "GRPROF",
  "INVINC",
  "OTHINC",
  "TOTREV",
  "TOTASS",
  "LAND_AREA",
  "pop",
  "density",
  "concentration",
  "wealth",
  "distribution",
  "output",
  "use",
  "dependence"
)

# Order up
main_df <- main_df[,my_order]
main_df <- main_df[with(main_df, order(FIPS, year)),]

# Finally, output the cleaned data file
write_csv(main_df, "Results/cleaned_data.csv", na = "")

# Remove unnecessary variables and dataframes for ~~aesthetic~~ purposes
rm(my_order)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Data Summary & Visualization ------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df <- read_csv("Results/cleaned_data.csv")

# Quick Tidying
main_df <- na.omit(main_df)

# Remove 2014 -- it's bad
main_df <- main_df %>% 
  filter(
    year < 2014
  )

main_df <- main_df %>% 
  filter(
    CONT > 0,
    EXPS > 0,
    TOTREV > 0,
    TOTASS > 0, 
    distribution < 5000,
    distribution > 0,
    wealth > 0,
    dependence < 100
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### The Lolipop Plot -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

temp <- main_df %>% 
  filter(treatment == 1) %>% 
  filter(year == 2007 | year == 2009) %>% 
  group_by(year) %>% 
  summarise(
    density = mean(density),
    concentration = mean(concentration),
    wealth = mean(wealth),
    distribution = mean(distribution),
    output = mean(output),
    use = mean(use),
    dependence = mean(dependence)
  )

temp <- as.data.frame(t(temp))
temp$variable <- rownames(temp)
colnames(temp) <- c("pre","post","variable")
temp <- temp[temp$variable != "year",]

# Rescale the variables
temp <- temp %>% 
  mutate(
    post = (post - pre)/pre,
    pre = 0
  )

l <- c("dependence", "use", "output", "distribution", "wealth", 
       "concentration", "density")

temp$variable <- factor(temp$variable, levels= l)

png("Results/Figures/lineplot.png", width=6, height = 4, units="in",
    res=600)
ggplot(temp, aes(x = post, y =  variable)) +
  geom_segment( aes(x=pre ,xend=post, y=variable, yend=variable), color="grey", 
                lwd = 1) +
  geom_vline(xintercept = 0) +
  geom_point(size=5, color= natparks.pals("Yellowstone", 1, type = "discrete")) +
  scale_y_discrete(labels = str_to_title(l)) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face="bold"),
    axis.title.y = element_blank()
  ) + 
  xlab("\n% Change 2007 to 2009, Impacted Counties")
dev.off()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Mapping the Geographic Sample ----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# County shapefile
county_shp <- st_as_sf(county_laea)

# Get the proper geographic sample
dist <- read_csv("Data/sf12010countydistance500miles.csv")
dist <- dist[dist$mi_to_county < 250, ]

# The disaster data again---but just Ike
hurricanes <- read_csv("Data/hurricanes.csv")
hurricanes <- hurricanes[hurricanes$storm == "IKE",]
fema <- read_csv("Data/FEMA_Disasters.csv")
fema <- fema[fema$disasterNumber %in% hurricanes$disasterNumber,]
fema$FIPS <- paste(fema$fipsStateCode, fema$fipsCountyCode, sep="")

# The states with any place with a disaster declaration
my_states <- unique(fema$fipsStateCode)

# The counties within 250 miles of a disaster county
dist <- merge(dist, fema, by.x = "county1", by.y = "FIPS")
counties_250 <- unique(dist$county2)

# The potential geographic sample
county_shp <- county_shp %>% 
  mutate(
    state_fips = str_sub(GEOID, 1, 2)
  ) %>% 
  filter(
    state_fips %in% my_states | GEOID %in% counties_250
  )

# Temporary availability file
temp <- main_df %>% 
  group_by(FIPS) %>% 
  summarise(
    treatment = max(treatment),
    years_in_sample = n(),
    population = mean(pop),
    density = mean(density),
    concentration = mean(concentration),
    wealth = mean(wealth),
    distribution = mean(distribution),
    output = mean(output),
    use = mean(use),
    dependence = mean(dependence),
    CONT = mean(CONT),
    EXPS = mean(EXPS),
    TOTREV = mean(TOTREV),
    TOTASS = mean(TOTASS)
  ) %>% 
  ungroup()

county_shp <- merge(county_shp, temp, by.x="GEOID", by.y="FIPS", all.x=T)
county_shp$treatment <- ifelse(county_shp$treatment == 1, "Treatment", "Control")
county_shp$years_in_sample <- replace_na(county_shp$years_in_sample, 0)

# Make the map

png("Results/Figures/geographic_sample.png", width=7, height = 4, units="in",
    res=600)
tm_shape(county_shp) +
  tm_polygons(
    "treatment",
    border.col = "white",
    title = "Treatment Status",
    palette = rev(natparks.pals("Yellowstone", 2, type="discrete"))) + 
  tm_layout(frame = FALSE)
dev.off()


png("Results/Figures/geographic_balance.png", width=7, height = 4, units="in",
    res=600)
tm_shape(county_shp) +
  tm_polygons(
    "years_in_sample",
    border.col = "white",
    title = "# of Years in Sample",
    style = "pretty",
    as.count=T,
    palette = natparks.pals("Denali", 6, type="discrete")) +
  tm_layout(frame = FALSE)
dev.off()

rm(county_shp, dist, fema, hurricanes, temp, counties_250, my_states, l)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Summary Statistics Table ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

summ_df <- main_df %>% 
  mutate(
    CONT = CONT/1000000,
    EXPS = EXPS/1000000,
    TOTREV = TOTREV/1000000,
    TOTASS = TOTASS/1000000,
    density = density * 1000,
    concentration = concentration * 100,
    wealth = wealth / 1000000,
    use = use/1000
  )

summ_df <- data.frame(
  summ_df[
    c("pop", "LAND_AREA", 
      "NONPROFITS", "TOTASS", "CONT", "EXPS", "TOTREV",  
      "density", "concentration", "wealth", "distribution", "output", "use", 
      "dependence"
    )
  ]
)

stargazer(summ_df, 
          summary = TRUE, 
          summary.stat = c(
            "mean", "median", "sd", "min", "max"
          ),
          digits = 2,
          notes = "Unbalanced panel; N = 7,745; T = 14; C = 566",
          style = "aer",
          covariate.labels = c(
            "\\quad Population",
            "\\quad Land Area (sq miles)",
            "\\quad \\# of Nonprofits",
            "\\quad Assets (millions USD)",
            "\\quad Contributions (millions USD)",
            "\\quad Expenses (millions USD)",
            "\\quad Revenues (millions USD)",
            "\\quad Density",
            "\\quad Concentration",
            "\\quad Wealth (millions USD)",
            "\\quad Distribution",
            "\\quad Output (USD)",
            "\\quad Resource Use (thousands USD)",
            "\\quad Resource Dependence"
          ),
          out = "Results/summary_stats.tex"
)

rm(summ_df)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Contribution Scales --------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

conts <- main_df %>% 
  group_by(FIPS) %>% 
  summarise(
    mean_pop = mean(pop),
    mean_cont = mean(CONT)
  )

coef_of_interest <- lm("log(mean_cont, 10) ~ log(mean_pop, 10)", conts)
coef_of_interest <- summary(coef_of_interest)
coef_of_interest <- round(coef_of_interest$coefficients[2], 3)

png("Results/Figures/cont_scale.png", width=6, height=4, units="in",
    res = 600)
ggplot(conts, aes(x=mean_pop, y=mean_cont/1000000)) +
  geom_point(size = 3.5, shape = 21,
             color = "white",
             fill = natparks.pals("Yellowstone", 2, type = "discrete")[2]) +
  scale_x_continuous(trans = scales::log10_trans()) + 
  scale_y_continuous(trans = scales::log10_trans()) + 
  geom_smooth(method='lm', formula= y~x, 
              color = natparks.pals("Yellowstone", 1, type = "discrete"), 
              lwd = 2) +
  annotate(geom = "text", x = 10000, y=1000, 
           label = paste("Slope =", coef_of_interest)) +
  labs(x="\nMean Population (Log Scale)", 
       y="Mean Contributions, Millions USD (Log Scale)\n") +
  theme_bw()
dev.off()

rm(conts, coef_of_interest)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Natural Disasters Figure ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

disasters <- read_csv("Data/FEMA_Disasters.csv")

disasters_ts <- disasters %>% 
  filter(
    declarationType == "DR",
    fyDeclared < 2022,
  ) %>% 
  distinct(disasterNumber, .keep_all = T) %>% 
  count(fyDeclared)

png("Results/Figures/major_disasters.png", width=7, height=4, units="in",res=600)
ggplot(disasters_ts, aes(x = fyDeclared, y = n)) +
  geom_line(lwd=1.25, color = natparks.pals("Yellowstone", 1)) +
  geom_point(color="white", size=4) +
  geom_point(color = natparks.pals("Yellowstone", 1), size = 3) + 
  labs(x="\nFiscal Year of Declaration", y="# of Major Disaster Declarations\n") + 
  theme_bw()
dev.off()

rm(disasters, disasters_ts)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### DiD Figures ----------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Population Weighted

ike_did <- main_df %>% 
  group_by(year, treatment) %>% 
  mutate(
    id_year_weight = pop/sum(pop)
  )

# DiD Figures
ike_did <- ike_did %>% 
  group_by(treatment, year) %>% 
  summarize(
    density = sum(id_year_weight * density),
    concentration = sum(concentration * id_year_weight),
    wealth = sum(wealth * id_year_weight),
    distribution = sum(distribution * id_year_weight),
    output = sum(output * id_year_weight),
    use = sum(use * id_year_weight),
    dependence = sum(dependence * id_year_weight)
  )

# Get the year Ike hit
my_hityear <- max(main_df$hit_year)

# Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Density\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p2 <- ggplot(ike_did, aes(x=year, y=log(concentration), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Concentration\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))


p3 <- ggplot(ike_did, aes(x=year, y=log(wealth), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Wealth\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))


p4 <- ggplot(ike_did, aes(x=year, y=log(distribution), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Distribution\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y=log(output), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Output\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p6 <- ggplot(ike_did, aes(x=year, y=log(use), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Resource Use\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y=dependence, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Resource Dependence\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

png("Results/Figures/DiD_pop_weighted.png", width=8, height=8, 
    units="in", res=450)
print({
  ggarrange(
    p1, p2, p3, p4, p5, p6, p7,
    ncol = 3, nrow = 3,
    labels = c("a","b","c","d","e","f", "g"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()

ike_did <- ike_did %>% 
  pivot_wider(names_from = treatment, values_from = 3:9)

# Difference Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density_1) - log(density_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Density\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p2 <- ggplot(ike_did, aes(x=year, y=log(concentration_1) - log(concentration_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Concentration\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))


p3 <- ggplot(ike_did, aes(x=year, y=log(wealth_1) - log(wealth_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Wealth\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))


p4 <- ggplot(ike_did, aes(x=year, y=log(distribution_1) - log(distribution_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Distribution\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y = log(output_1) - log(output_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Output\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p6 <- ggplot(ike_did, aes(x=year, y = log(use_1) - log(use_0) )) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Resource Use\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y =  dependence_1 - dependence_0 )) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Resource Dependence\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

png("Results/Figures/differenced_pop_weighted.png", width=8, height=8, 
    units="in", res=450)
print({
  ggarrange(
    p1, p2, p3, p4, p5, p6, p7, 
    ncol = 3, nrow = 3,
    labels = c("a","b","c","d","e","f"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()

# Non-population weight trend figure

ike_did <- main_df %>% 
  group_by(year, treatment) %>% 
  summarize(
    density = mean(density),
    concentration = mean(concentration),
    wealth = mean(wealth),
    distribution = mean(distribution),
    output = mean(output),
    use = mean(use),
    dependence = mean(dependence)
  )

# Get the year Ike hit
my_hityear <- max(main_df$hit_year)

# Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Density\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p2 <- ggplot(ike_did, aes(x=year, y=log(concentration), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Concentration\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))


p3 <- ggplot(ike_did, aes(x=year, y=log(wealth), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Wealth\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))


p4 <- ggplot(ike_did, aes(x=year, y=log(distribution), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Distribution\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y=log(output), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Output\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p6 <- ggplot(ike_did, aes(x=year, y=log(use), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Resource Use\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y=dependence, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Resource Dependence\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

png("Results/Figures/DiD.png", width=8, height=8, 
    units="in", res=450)
print({
  ggarrange(
    p1, p2, p3, p4, p5, p6, p7,
    ncol = 3, nrow = 3,
    labels = c("a","b","c","d","e","f", "g"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()


ike_did <- ike_did %>% 
  pivot_wider(names_from = treatment, values_from = 3:9)

# Difference Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density_1) - log(density_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Density\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p2 <- ggplot(ike_did, aes(x=year, y=log(concentration_1) - log(concentration_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Concentration\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p3 <- ggplot(ike_did, aes(x=year, y=log(wealth_1) - log(wealth_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Wealth\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p4 <- ggplot(ike_did, aes(x=year, y=log(distribution_1) - log(distribution_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Distribution\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y = log(output_1) - log(output_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Output\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p6 <- ggplot(ike_did, aes(x=year, y = log(use_1) - log(use_0) )) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Resource Use\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y =  dependence_1 - dependence_0 )) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Resource Dependence\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

png("Results/Figures/differenced.png", width=8, height=8, 
    units="in", res=450)
print({
  ggarrange(
    p1, p2, p3, p4, p5, p6, p7, 
    ncol = 3, nrow = 3,
    labels = c("a","b","c","d","e","f"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()


rm(ike_did, p1, p2, p3, p4, p5, p6, p7, my_hityear)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Analysis --------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df$tpost_D <- ifelse(main_df$year >= main_df$hit_year, 1, 0)

# Make panel data frames
# df_storm <- pdata.frame(main_df, index = c("FIPS", "year"))
df_storm <- main_df
b_df_storm <- make.pbalanced(main_df, index = c("FIPS", "year"), 
                             balance.type = "shared.individuals")

# Write a function to make the standard errors
do_the_SE <- function(model){
  
  # Cluster the SEs
  se <- data.frame(summary(model, cluster = "FIPS")$coefficients)$`Std..Error`
  
  # Fix the position problem
  temp <- which(summary(model)[["aliased"]])
  
  for (i in 1:length(temp)){se <- append(se, 1, after=temp[i]-1)}
  
  return(se)
  
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### First Set of Regressions ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_1 <- function (my_df, table_description, table_output){
  
  mod1 <- lm(log(density) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(log(dependence) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7,
            title = paste("Hurricane Ike, Canonical DiD---", table_description),
            omit = c("factor","Constant"),
            se = my_SE,
            type = "latex",
            order = c(
              "treatment:tpost_D",
              "treatment",
              "tpost_D"
            ),
            font.size = 'footnotesize',
            covariate.labels = c(
              "Disaster * Post Storm",
              "Disaster",
              "Post Storm"
            ),
            dep.var.labels = c(
              "Density",
              "Concentration",
              "Wealth",
              "Distribution",
              "Output",
              "Use",
              "Dependence"
            ),
            omit.stat = c("rsq", "adj.rsq"), 
            out = paste("Results/Regression Tables/", table_output, sep=""))
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 1", 7)
  panel_type <- rep(table_description, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D"],
    mod2$coefficients["treatment:tpost_D"],
    mod3$coefficients["treatment:tpost_D"],
    mod4$coefficients["treatment:tpost_D"],
    mod5$coefficients["treatment:tpost_D"],
    mod6$coefficients["treatment:tpost_D"],
    mod7$coefficients["treatment:tpost_D"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

mod1a_coefs <- regressions_1(df_storm, "Unbalanced", "reg1_unbalanced.tex")
mod1b_coefs <- regressions_1(b_df_storm, "Balanced", "reg1_balanced.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Second Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_2 <- function (my_df, table_description, table_output){
  
  mod1 <- lm(log(density) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  
  stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, 
            title = paste("Hurricane Ike, Scaled with Log Contributions---", 
                          table_description, sep=""),
            omit = c("factor","Constant"),
            se = my_SE,
            type = "latex",
            order = c(
              "treatment:tpost_D:log(CONT)",
              "treatment:tpost_D",
              "treatment:log(CONT)",
              "tpost_D:log(CONT)",
              "treatment",
              "tpost_D",
              "log(CONT)"
            ),
            font.size = 'footnotesize',
            covariate.labels = c(
              "Disaster * Post Storm * Log Cont.",
              "Disaster * Post Storm",
              "Disaster * Log Cont.",
              "Post Storm *  Log Cont.",
              "Disaster",
              "Post Storm",
              "Log Cont."
            ),
            dep.var.labels = c(
              "Density",
              "Concentration",
              "Wealth",
              "Distribution",
              "Output",
              "Use",
              "Dependence"
            ),
            omit.stat = c("rsq", "adj.rsq"), 
            out = paste("Results/Regression Tables/", table_output, sep = ""))
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 2", 7)
  panel_type <- rep(table_description, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D:log(CONT)"],
    mod2$coefficients["treatment:tpost_D:log(CONT)"],
    mod3$coefficients["treatment:tpost_D:log(CONT)"],
    mod4$coefficients["treatment:tpost_D:log(CONT)"],
    mod5$coefficients["treatment:tpost_D:log(CONT)"],
    mod6$coefficients["treatment:tpost_D:log(CONT)"],
    mod7$coefficients["treatment:tpost_D:log(CONT)"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D:log(CONT)")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

mod2a_coefs <- regressions_2(df_storm, "Unbalanced", "reg2_unbalanced.tex")
mod2b_coefs <-regressions_2(b_df_storm, "Balanced", "reg2_balanced.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Third Set of Regressions ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_3 <- function (my_df, table_description, table_output, my_cutoff){
  
  # Dichotomize the contributions
  my_df <- my_df %>% 
    group_by(year, treatment) %>% 
    mutate(
      cont_dich = ifelse(CONT > quantile(CONT, probs = 1 - my_cutoff), 1, 0)
    ) %>% 
    ungroup
  
  mod1 <- lm(log(density) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  
  stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7,
            title = paste("Hurricane Ike, Dichotomized Contributions---",
                          table_description, sep=""),
            omit = c("factor","Constant"),
            se = my_SE,
            type = "latex",
            order = c(
              "treatment:tpost_D:cont_dich",
              "treatment:tpost_D",
              "treatment:cont_dich",
              "tpost_D:cont_dich",
              "treatment",
              "tpost_D",
              "cont_dich"
            ),
            font.size = 'footnotesize',
            covariate.labels = c(
              "Disaster * Post Storm * Top Cont.",
              "Disaster * Post Storm",
              "Disaster * Top Cont.",
              "Post Storm *  Top Cont.",
              "Disaster",
              "Post Storm",
              "Top Cont."
            ),
            dep.var.labels = c(
              "Density",
              "Concentration",
              "Wealth",
              "Distribution",
              "Output",
              "Use",
              "Dependence"
            ),
            omit.stat = c("rsq", "adj.rsq"), 
            out = paste("Results/Regression Tables/", table_output, sep=""))
  
  # Main Coefficient Reporting dataframe
  model_num <- rep(paste("Model 3", my_cutoff, sep=""), 7)
  panel_type <- rep(table_description, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D:cont_dich"],
    mod2$coefficients["treatment:tpost_D:cont_dich"],
    mod3$coefficients["treatment:tpost_D:cont_dich"],
    mod4$coefficients["treatment:tpost_D:cont_dich"],
    mod5$coefficients["treatment:tpost_D:cont_dich"],
    mod6$coefficients["treatment:tpost_D:cont_dich"],
    mod7$coefficients["treatment:tpost_D:cont_dich"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D:cont_dich")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

mod3a_coefs <- regressions_3(df_storm, "Unbalanced", "reg3_50_unbalanced.tex", 0.50)
mod3b_coefs <- regressions_3(b_df_storm, "Balanced", "reg3-50_balanced.tex", 0.50)
mod3c_coefs <- regressions_3(df_storm, "Unbalanced", "reg3-25_unbalanced.tex", 0.25)
mod3d_coefs <- regressions_3(b_df_storm, "Balanced", "reg3-25_balanced.tex", 0.25)
mod3e_coefs <- regressions_3(df_storm, "Unbalanced", "reg3-10_unbalanced.tex", 0.10)
mod3f_coefs <- regressions_3(b_df_storm, "Balanced", "reg3-10_balanced.tex", 0.10)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Fourth Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_4 <- function (my_df, table_description, table_output){

  my_df <- my_df %>%
    rowwise %>%
    mutate(
      year = as.numeric(year),
      tpost = max(year - hit_year, 0)
    )
  
  mod1 <- lm(log(density) ~  treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7,
            title = paste("Hurricane Ike, Time Scaled---", 
                          table_description, sep=""),
            omit = c("factor","Constant"),
            se = my_SE,
            type = "latex",
            order = c(
              "treatment:tpost_D:tpost",
              "treatment:tpost_D",
              "treatment",
              "tpost_D",
              "tpost"
            ),
            font.size = 'footnotesize',
            covariate.labels = c(
              "Disaster * Post Storm * Years After",
              "Disaster * Post Storm",
              "Disaster",
              "Post Storm",
              "Years After"
            ),
            dep.var.labels = c(
              "Density",
              "Concentration",
              "Wealth",
              "Distribution",
              "Output",
              "Use",
              "Dependence"
            ),
            omit.stat = c("rsq", "adj.rsq"), 
            out = paste("Results/Regression Tables/", table_output, sep = ""))
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 4", 7)
  panel_type <- rep(table_description, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D:tpost"],
    mod2$coefficients["treatment:tpost_D:tpost"],
    mod3$coefficients["treatment:tpost_D:tpost"],
    mod4$coefficients["treatment:tpost_D:tpost"],
    mod5$coefficients["treatment:tpost_D:tpost"],
    mod6$coefficients["treatment:tpost_D:tpost"],
    mod7$coefficients["treatment:tpost_D:tpost"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D:tpost")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)

}

mod4a_coefs <- regressions_4(df_storm, "Unbalanced", "reg4_unbalanced.tex")
mod4b_coefs <-regressions_4(b_df_storm, "Balanced", "reg4_balanced.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Unbalanced vs. Balanced Panel ----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

all_mods <- bind_rows(list(mod1a_coefs, mod1b_coefs, 
                           mod2a_coefs, mod2b_coefs, 
                           mod3a_coefs, mod3b_coefs, mod3c_coefs, mod3d_coefs, 
                           mod3e_coefs, mod3f_coefs,
                           mod4a_coefs, mod4b_coefs))

png("Results/Robustness Checks/Panel Balance/Model1.png", 
    width=6, height=4, units="in", res=600)
ggplot(all_mods %>% filter(model == "Model 1"), x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x=NULL, y=NULL) +
  theme_bw()
dev.off()


png("Results/Robustness Checks/Panel Balance/Model2.png", 
    width=6, height=4, units="in", res=600)
ggplot(all_mods %>% filter(model == "Model 2"), x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x=NULL, y=NULL) +
  theme_bw()
dev.off()


temp <- all_mods %>% 
  filter(grepl("Model 30", model)) %>% 
  mutate(
    Cutoff = paste(as.numeric(gsub("Model 30", "", model)) * 100, "%", sep="")
  )

png("Results/Robustness Checks/Panel Balance/Model3.png", 
    width=8, height=4, units="in", res=600)
ggplot(temp, x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI, color=Cutoff),
                size=0.5, width=0.5, position=position_dodge(width = 1)) +
  geom_point(aes(x=panel, y=coefs, color=Cutoff), size=2, position=position_dodge(width = 1)) +
  geom_hline(color="black", yintercept = 0) +
  scale_color_manual(values = natparks.pals("Yellowstone", 3)) + 
  labs(x=NULL, y=NULL) +
  theme_bw()
dev.off()

png("Results/Robustness Checks/Panel Balance/Model4.png", 
    width=6, height=4, units="in", res=600)
ggplot(all_mods %>% filter(model == "Model 4"), x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x=NULL, y=NULL) +
  theme_bw()
dev.off()


rm(all_mods, mod1a_coefs, mod1b_coefs, mod2a_coefs, mod2b_coefs, 
   mod3a_coefs, mod3b_coefs, mod3c_coefs, mod3d_coefs, mod3e_coefs, mod3f_coefs,
   mod4a_coefs, mod4b_coefs, temp
)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Placebo Tests --------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### First Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_1 <- function (my_df, placebo_year){
  
  my_df <- my_df %>% mutate(
    year = as.numeric(year),
    tpost_D = ifelse(year >= placebo_year, 1, 0)
  )
  
  mod1 <- lm(log(density) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment*tpost_D + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 1", 7)
  panel_type <- rep(placebo_year, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D"],
    mod2$coefficients["treatment:tpost_D"],
    mod3$coefficients["treatment:tpost_D"],
    mod4$coefficients["treatment:tpost_D"],
    mod5$coefficients["treatment:tpost_D"],
    mod6$coefficients["treatment:tpost_D"],
    mod7$coefficients["treatment:tpost_D"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Second Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_2 <- function (my_df, placebo_year){
  
  my_df <- my_df %>% mutate(
    year = as.numeric(year),
    tpost_D = ifelse(year >= placebo_year, 1, 0)
  )
  
  mod1 <- lm(log(density) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment*tpost_D*log(CONT) + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 2", 7)
  panel_type <- rep(placebo_year, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D:log(CONT)"],
    mod2$coefficients["treatment:tpost_D:log(CONT)"],
    mod3$coefficients["treatment:tpost_D:log(CONT)"],
    mod4$coefficients["treatment:tpost_D:log(CONT)"],
    mod5$coefficients["treatment:tpost_D:log(CONT)"],
    mod6$coefficients["treatment:tpost_D:log(CONT)"],
    mod7$coefficients["treatment:tpost_D:log(CONT)"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D:log(CONT)")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Third Set of Regressions ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_3 <- function (my_df, placebo_year){
  
  my_df <- my_df %>% mutate(
    year = as.numeric(year),
    tpost_D = ifelse(year >= placebo_year, 1, 0)
  )
  
  # Dichotomize the contributions
  my_df <- my_df %>% 
    group_by(year, treatment) %>% 
    mutate(
      cont_dich = ifelse(CONT > quantile(CONT, probs = 0.5), 1, 0)
    ) %>% 
    ungroup
  
  mod1 <- lm(log(density) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment*tpost_D*cont_dich + 
               as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  
  # Main Coefficient Reporting dataframe
  model_num <- rep(paste("Model 3", sep=""), 7)
  panel_type <- rep(placebo_year, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D:cont_dich"],
    mod2$coefficients["treatment:tpost_D:cont_dich"],
    mod3$coefficients["treatment:tpost_D:cont_dich"],
    mod4$coefficients["treatment:tpost_D:cont_dich"],
    mod5$coefficients["treatment:tpost_D:cont_dich"],
    mod6$coefficients["treatment:tpost_D:cont_dich"],
    mod7$coefficients["treatment:tpost_D:cont_dich"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D:cont_dich")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Fourth Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_4 <- function (my_df, placebo_year){
  
  my_df <- my_df %>% 
    rowwise %>% 
    mutate(
      year = as.numeric(year),
      tpost_D = ifelse(year >= placebo_year, 1, 0),
      tpost = max(year - placebo_year, 0)
    )
  
  mod1 <- lm(log(density) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(concentration) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(log(wealth) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod4 <- lm(log(distribution) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod5 <- lm(log(output) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod6 <- lm(log(use) ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod7 <- lm(dependence ~ treatment:tpost_D:tpost + treatment*tpost_D + 
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  my_SE <- lapply(my_models, do_the_SE)
  
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 4", 7)
  panel_type <- rep(placebo_year, 7)
  outcome <- c(
    "Density",
    "Concentration",
    "Wealth",
    "Distribution",
    "Output",
    "Use",
    "Dependence"
  )
  
  main_coefs <- c(
    mod1$coefficients["treatment:tpost_D:tpost"],
    mod2$coefficients["treatment:tpost_D:tpost"],
    mod3$coefficients["treatment:tpost_D:tpost"],
    mod4$coefficients["treatment:tpost_D:tpost"],
    mod5$coefficients["treatment:tpost_D:tpost"],
    mod6$coefficients["treatment:tpost_D:tpost"],
    mod7$coefficients["treatment:tpost_D:tpost"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:tpost_D:tpost")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Actually run the placebo tests

placebo_years = list(2006, 2007, 2008, 2009, 2010)

placebos1 <- lapply(placebo_years, function (x) regressions_1(df_storm, x))
placebos2 <- lapply(placebo_years, function (x) regressions_2(df_storm, x))
placebos3 <- lapply(placebo_years, function (x) regressions_3(df_storm, x))
placebos4 <- lapply(placebo_years, function (x) regressions_4(df_storm, x))

placebos1 <- bind_rows(placebos1)
placebos2 <- bind_rows(placebos2)
placebos3 <- bind_rows(placebos3)
placebos4 <- bind_rows(placebos4)

# Visualize the placebo tests

png("Results/Robustness Checks/Placebo Tests/set1.png", width=8, height = 4, 
    units="in", res=600)
ggplot(placebos1 , x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x="\nPlacebo Year (2008 = Actual Treatment Year)", y="Coefficient Estimate\n") +
  theme_bw()
dev.off()


png("Results/Robustness Checks/Placebo Tests/set2.png", width=7, height = 4, 
    units="in", res=600)
ggplot(placebos2, x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x="\nPlacebo Year (2008 = Actual Treatment Year)", y="Coefficient Estimate\n") +
  theme_bw()
dev.off()


png("Results/Robustness Checks/Placebo Tests/set3.png", width=7, height = 4, 
    units="in", res=600)
ggplot(placebos3, x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x="\nPlacebo Year (2008 = Actual Treatment Year)", y="Coefficient Estimate\n") +
  theme_bw()
dev.off()


png("Results/Robustness Checks/Placebo Tests/set4.png", width=7, height = 4, 
    units="in", res=600)
ggplot(placebos4, x=panel, y=coefs) +
  facet_wrap(vars(outcome), scales="free", nrow = 3) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x="\nPlacebo Year (2008 = Actual Treatment Year)", y="Coefficient Estimate\n") +
  theme_bw()
dev.off()

gc()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Event Studies --------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Event studies are---to me---not currently a useful addition. I had previously
# implemented some event study work with the old metrics. I do not anticipate 
# re-implementing these event studies, but if there was a need, I would begin by
# building on this commented out code.
#
# df <- main_df
# 
# df$center_year <- df$year - 2007
# 
# df <- df %>% mutate(center_year = ifelse(center_year==0, -20, center_year))
# 
# df <- df %>% 
#   mutate(
#     log_exps_npo = log(EXPS/NONPROFITS),
#     log_pop_npo = log(pop/NONPROFITS),
#     log_rev_npo = log(TOTREV/NONPROFITS),
#     log_ass_npo = log(TOTASS/NONPROFITS),
#     log_cont_npo = log(cont_npo),
#     log_cont_rev = log(cont_rev)
#   )
# 
# 
# # Regular Event Study
# 
# run_event_study <- function(out_var, variable_lab, file_name){
#   
#   my_cols <- c("FIPS", "center_year", "treatment", "pop", out_var)
#   my_df <- df[my_cols]
#   colnames(my_df) <- c("FIPS", "center_year", "treatment", "pop", "outcome")
#   
#   temp <- lm(outcome ~ treatment*as.factor(center_year) + FIPS, 
#              data = my_df,
#              weights = pop)
#   
#   temp <- summary(temp)
#   
#   temp <- data.frame(temp$coefficients)
#   temp$var <- rownames(temp)
#   rownames(temp) <- NULL
#   
#   temp <- temp[grepl(":", temp$var),]
#   temp[nrow(temp) +1,] <- c(0, 0, 0, 0, "0") 
#   temp <- temp %>% 
#     rowwise %>% 
#     mutate(
#       estimate = as.numeric(Estimate),
#       std_err = as.numeric(`Std..Error`),
#       period = as.integer(gsub(".*)", "", var)),
#       ci_lower = estimate - 1.96 * std_err,
#       ci_upper = estimate + 1.96 * std_err
#     )
#   
#   
#   print({
#     ggplot(temp, aes(x = period + 2007, y = estimate)) + 
#       geom_vline(xintercept = 2007, color="grey") +
#       geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), size=0.5, width=0.5, 
#                     color = natparks.pals("Yellowstone", 1)) +
#       geom_point(aes(x=period + 2007, y=estimate), size=2, 
#                  color = natparks.pals("Yellowstone", 1)) + 
#       geom_line(color = natparks.pals("Yellowstone", 1)) +
#       labs(x="\nYear (2007 = Pre-Storm Benchmark)", 
#            y = "Estimated Effect\n") +
#       geom_hline(yintercept=0) +
#       ggtitle(label = variable_lab) +
#       theme_bw()
#     ggsave(paste("Results/Figures/Ike Event Studies/Population Weighted/", 
#                  file_name, ".png", sep=""), width = 6, height = 4, units='in')
#   })
#   
# }
# 
# run_event_study("log_exps_npo", "log(Expenses/Nonprofit)", "Outcome1")
# run_event_study("log_pop_npo", "log(Population/Nonprofit)", "Outcome2")
# run_event_study("log_rev_npo", "log(Revenues/Nonprofit)", "Outcome3")
# run_event_study("log_ass_npo", "log(Assets/Nonprofit)", "Outcome4")
# run_event_study("log_cont_npo", "log(Contributions/Nonprofit)", "Outcome5")
# run_event_study("log_cont_rev", "log(Contributions/Revenue)", "Outcome6")
# 
# # Contribution Scaled -- Interpretation Challenged -- Event Study
# 
# run_event_study_2 <- function(out_var, variable_lab, file_name){
#   
#   my_cols <- c("FIPS", "center_year", "treatment", "CONT", out_var)
#   my_df <- df[my_cols]
#   colnames(my_df) <- c("FIPS", "center_year", "treatment", "cont", "outcome")
#   
#   temp <- lm(outcome ~ treatment*as.factor(center_year)*cont + FIPS, 
#              data = my_df)
#   
#   temp <- summary(temp)
#   
#   temp <- data.frame(temp$coefficients)
#   temp$var <- rownames(temp)
#   rownames(temp) <- NULL
#   
#   temp <- temp[(grepl("treatment", temp$var)) & 
#                  (grepl("cont", temp$var)) &
#                  (grepl("center", temp$var)),]
#   temp[nrow(temp) +1,] <- c(0, 0, 0, 0, "0") 
#   temp <- temp %>% 
#     rowwise %>% 
#     mutate(
#       estimate = as.numeric(Estimate) * 1000000000,
#       std_err = as.numeric(`Std..Error`) * 1000000000,
#       period = gsub(".*)", "", var),
#       period = as.integer(gsub(":.*", "", period)),
#       ci_lower = estimate - 1.96 * std_err,
#       ci_upper = estimate + 1.96 * std_err
#     )
#   
#   print({
#     ggplot(temp, aes(x = period + 2007, y = estimate)) + 
#       geom_vline(xintercept = 2007, color="grey") +
#       geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), size=0.5, width=0.5,
#                     color = natparks.pals("Yellowstone", 1)) +
#       geom_point(aes(x=period + 2007, y=estimate), size=2, 
#                  color = natparks.pals("Yellowstone", 1)) + 
#       geom_line(color = natparks.pals("Yellowstone", 1)) +
#       labs(x="\nYear (2007 = Pre-Storm Benchmark)", 
#            y = "Estimated Effect\n") +
#       geom_hline(yintercept=0) +
#       ggtitle(label = variable_lab) +
#       theme_bw()
#     ggsave(paste("Results/Figures/Ike Event Studies/Contribution Scaled/", 
#                  file_name, ".png", sep=""), 
#            width = 6, height = 4, units='in')
#   })
#   
#   
# }
# 
# run_event_study_2("log_exps_npo", "log(Expenses/Nonprofit)", "Outcome1")
# run_event_study_2("log_pop_npo", "log(Population/Nonprofit)", "Outcome2")
# run_event_study_2("log_rev_npo", "log(Revenues/Nonprofit)", "Outcome3")
# run_event_study_2("log_ass_npo", "log(Assets/Nonprofit)", "Outcome4")
# run_event_study_2("log_cont_npo", "log(Contributions/Nonprofit)", "Outcome5")
# run_event_study_2("log_cont_rev", "log(Contributions/Revenue)", "Outcome6")
# 
# rm(df, run_event_study, run_event_study_2)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
