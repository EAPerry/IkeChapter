#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Natural Disasters and Giving: Book Chapter -----------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2022-12-18
# 
# Purpose: The purpose of this script is to clean, organize, and summarize key 
# data related to the impact of natural disasters on local charitable giving. 
# Primary goals include: (1) basic data cleaning on nonprofit data, and (2) 
# supplementing nonprofit data with other county-level data.
# 
# Input files: 
# 
# co_fy_trends.parquet : NCCS trend data on individual nonprofits; contains
#     records for the nonprofits in the "other" category.
# 
# pc_fy_trends.parquet : NCCS trend data on individual nonprofits; contains
#     records for public charities.
# 
# pf_fy_trends.parquet : NCCS trend data on individual nonprofits; contains
#     records for private foundations.
# 
# 2020_Planning_Data.csv : County-level census data from the 2010 decennial
#     Census and the 2014 American Community Survey. Use selected variables for
#     perspective on demographic makeup of counties.
# 
# annual_population.csv : 1-year population estimates from the ACS, 2000-2014.
# 
# County_SoVI.csv : The Social Vulnerability Index, county-level. This is based
#     in the 2010-2014 ACS.
# 
# CAINC35_ALL_AREAS_1969_2020.csv : County-level transfers data over the 1969 to
#     2020 time period. Contains plenty of data that is entirely out unnecessary
#     to our analysis. The only thing we want is a variable looking at
#     transfers to nonprofits.
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
# Output files: 
# 
# cleaned_data.csv : Cleaned data incorporating the data listed above. This 
#     is panel data with each observation representing a county-year. A dummy 
#     variable is included indicating that the county-year experienced a 
#     disaster previously in the period. Note: The census data is not in panel 
#     form and is identical across time periods, with the exception of the 
#     population data.
#
# descriptive.html : Descriptive statistics table.
#
# reg1.html : Regression results for the first regression table.
#
# reg2.html : Regression results for the second regression table.
# 
# 
# Outline: (Crtl + Shift + O) 
#   1. Importing packages and Setup
#   2. Reading and Processing Data
#   3. Composing the Clean Data
#   4. Analysis
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Code to Change When Replicating ---------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Reset your working directory
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

#### Consolidate the NCCS Trend Files ------------------------------------------

my_vars1 <- c(
  "CONT",
  "CoreSrc",
  "EIN",
  "EXPS",
  "FIPS",
  "FISYR",
  "FNDNCD",
  "GRPROF",
  "INVINC",
  "ASS_EOY",
  "LEVEL1",
  "LEVEL2",
  "LEVEL3",
  "LEVEL4",
  "LIAB_EOY",
  "MAJGRPB",
  "NAICS",
  "NAME",
  "NETINC",
  "nteeFinal",
  "ntmaj10",
  "ntmaj12",
  "ntmaj5",
  "OTHINC",
  "OUTNCCS",
  "RULEDATE",
  "STATE",
  "SUBSECCD",
  "TAXPER",
  "TOTREV",
  "ZIP5"
)

my_vars2 <- c(
  "P1TCONT",
  "CoreSrc",
  "EIN",
  "P1TOTEXP",
  "FIPS",
  "FISYR",
  "FNDNCD",
  "P1GINVPF",
  "P1NETINV",
  "P2TOTAST",
  "LEVEL1",
  "LEVEL2",
  "LEVEL3",
  "LEVEL4",
  "P2TLIABL",
  "MAJGRPB",
  "NAICS",
  "NAME",
  "P1NADINC",
  "nteeFinal",
  "ntmaj10",
  "ntmaj12",
  "ntmaj5",
  "P1OTHINC",
  "OUTNCCS",
  "RULEDATE",
  "STATE",
  "SUBSECCD",
  "TAXPER",
  "P1TOTREV",
  "ZIP5"
)


df1 <- read_parquet(
  "Data/NCCS/co_fy_trends.parquet",
  col_select = all_of(my_vars1)
)

df2 <- read_parquet(
  "Data/NCCS/pc_fy_trends.parquet",
  col_select = all_of(my_vars1)
)

df3 <- read_parquet(
  "Data/NCCS/pf_fy_trends.parquet",
  col_select = all_of(my_vars2)
)

# The private foundation variable names do not match the other files
df3 <- df3 %>% 
  rename(
    GRPROF = P1GINVPF,
    NETINC = P1NADINC,
    INVINC = P1NETINV,
    OTHINC = P1OTHINC,
    CONT = P1TCONT,
    EXPS = P1TOTEXP,
    TOTREV = P1TOTREV,
    LIAB_EOY = P2TLIABL,
    ASS_EOY = P2TOTAST
  )

# Combine
nccs <- bind_rows(df1, df2, df3)
rm(df1, df2, df3, my_vars1, my_vars2)
gc()


#### Clean the NCCS Data -------------------------------------------------------

# We can only look at the period from 2000 to 2014
nccs <- nccs[nccs$FISYR >= 2000, ]
nccs <- nccs[nccs$FISYR <= 2014, ]

# Remove any observations with a missing county
nccs <- nccs[!is.na(nccs$FIPS),]
nccs <- nccs[!(nccs$FIPS %in% c("AA","AE","AP")),]

# Also remove some useless variables
nccs <- nccs %>% 
  select(-c(          
    "CoreSrc",
    "FNDNCD",
    "OUTNCCS",
    "TAXPER",
    "RULEDATE"
  ))

# Group and summarize variables for each county and fiscal year
nccs <- nccs %>% 
  group_by(FIPS, FISYR) %>% 
  summarize(
    # Concentration Indexes
    # For now, do not group by industry
    HHI_CONT = sum((CONT/sum(CONT, nar.rm=T))^2, na.rm = T),
    HHI_EXPS = sum((EXPS/sum(EXPS, nar.rm=T))^2, na.rm = T),
    HHI_GRPROF = sum((GRPROF/sum(GRPROF, nar.rm=T))^2, na.rm = T),
    HHI_INVINC = sum((INVINC/sum(INVINC, nar.rm=T))^2, na.rm = T),
    HHI_REV = sum((TOTREV/sum(TOTREV, nar.rm=T))^2, na.rm = T),
    HHI_ASS = sum((ASS_EOY/sum(ASS_EOY, nar.rm=T))^2, na.rm = T),
    # Base sums
    NONPROFITS = n(),
    CONT = sum(CONT, na.rm = T),
    EXPS = sum(EXPS, na.rm = T),
    GRPROF = sum(GRPROF, na.rm = T),
    INVINC = sum(INVINC, na.rm = T),
    OTHINC = sum(OTHINC, na.rm = T),
    TOTREV = sum(TOTREV, na.rm = T),
    TOTASS = sum(ASS_EOY, na.rm = T)
  ) %>% 
  ungroup()

# Remove unused data from memory
gc()

# Just in case: Be sure to remove counties with a missing FIPS code
nccs <- nccs[!is.na(nccs$FIPS),]

# Create the unique ID variable
nccs$co_year <- paste(nccs$FIPS, nccs$FISYR, sep="-")

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

# Bring in the annual population data from the Census -- this is actually annual
pop <- read_csv("Data/annual_population.csv")

# Because this is annual, the merging variable will be co_year
pop$co_year <- paste(pop$FIPS, pop$year, sep="-")
pop <- pop %>% 
  select("co_year","pop")

# Also, store the 2-digit FIPS codes for later
state_fips <- unique(substr(pop$co_year, 1,2))

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
rm(census, hurricanes, nccs, pop, state_fips, storms)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Cleaning on the full dataset -----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# There are a collection of outcome variables that we should be sure to add to
# the data 

main_df <- main_df %>% 
  mutate(
    # Nonprofits
    npo_capita = NONPROFITS/pop,
    npo_area = NONPROFITS/LAND_AREA,
    # Contributions
    cont_capita = CONT/pop,
    cont_area = CONT/LAND_AREA,
    cont_npo = CONT/NONPROFITS,
    # Expenses
    exps_capita = EXPS/pop,
    exps_area = EXPS/LAND_AREA,
    exps_npo = EXPS/NONPROFITS,
    # Assets
    ass_capita = TOTASS/pop,
    ass_area = TOTASS/LAND_AREA,
    ass_npo = TOTASS/NONPROFITS,
    # Revenues
    revs_capita = TOTREV/pop,
    revs_area = TOTREV/LAND_AREA,
    rev_npo = TOTREV/NONPROFITS
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
  "npo_capita",
  "npo_area",
  "cont_capita",
  "cont_area",
  "cont_npo", 
  "exps_capita",
  "exps_area",
  "exps_npo",
  "ass_capita",
  "ass_area",
  "ass_npo",
  "revs_capita",
  "revs_area",
  "rev_npo", 
  "HHI_CONT",
  "HHI_EXPS",    
  "HHI_GRPROF",  
  "HHI_INVINC", 
  "HHI_REV",     
  "HHI_ASS" 
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
  mutate(cont_rev = CONT/TOTREV *100) %>% 
  filter(
    CONT > 0,
    EXPS > 0,
    TOTREV > 0,
    TOTASS > 0,
    cont_rev < 100
  )


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Mapping the Geographic Sample ----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# County shapefile
county_shp <- st_as_sf(county_laea)

# Temporary availability file
temp <- main_df %>% 
  group_by(FIPS) %>% 
  summarise(
    treatment = max(treatment),
    years_in_sample = n()
  ) %>% 
  ungroup()

county_shp <- merge(county_shp, temp, by.x="GEOID", by.y="FIPS")
county_shp$treatment <- as.factor(county_shp$treatment)

# Make the map

png("Results/Figures/geographic_sample.png", width=6, height = 4, units="in",
    res=600)
tm_shape(county_shp) +
  tm_polygons(
    "treatment",
    border.col = "white",
    title = "Treatment Status",
    palette = rev(natparks.pals("Yellowstone", 2, type="discrete")))
dev.off()


png("Results/Figures/geographic_balance.png", width=6, height = 4, units="in",
    res=600)
tm_shape(county_shp) +
  tm_polygons(
    "years_in_sample",
    border.col = "white",
    title = "# of Years in Sample",
    style = "pretty",
    as.count=T,
    palette = natparks.pals("Denali", 4, type="discrete"))
dev.off()

rm(temp, county_shp)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Summary Statistics Table ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

summ_df <- data.frame(
  main_df[
    c("NONPROFITS", "CONT", "EXPS", "TOTREV", "TOTASS", "LAND_AREA", "pop", 
      "npo_capita", "npo_area", "cont_capita", "cont_area", "cont_npo")
  ]
)

summ_df <- summ_df %>% 
  mutate(
    CONT = CONT/1000000,
    EXPS = EXPS/1000000,
    TOTREV = TOTREV/1000000,
    TOTASS = TOTASS/1000000,
    npo_capita = npo_capita * 1000,
    npo_area = npo_area * 10
  )

stargazer(summ_df, 
          summary = TRUE, 
          type = "html",
          summary.stat = c(
            "mean", "median", "sd", "min", "max"
          ),
          digits = 2,
          notes = "Unbalanced panel; N = 7,600; T = 14; C = 555",
          style = "aer",
          covariate.labels = c(
            "# of Nonprofits",
            "Contributions (millions USD)",
            "Expenses (millions USD)",
            "Total Revenues (millions USD)",
            "Total Assets (millions USD)",
            "Land Area (sq miles)",
            "Population",
            "Nonprofits/1000 people",
            "Nonprofits/10 sq miles",
            "Contributions per Capita",
            "Contributions per sq mile",
            "Contributions per Nonprofit"
          ),
          out = "Results/summary_stats.html"
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

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### DiD Figures ----------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

ike_did <- main_df %>% 
  group_by(year, treatment) %>% 
  mutate(
    id_year_weight = pop/sum(pop)
  )

# DiD Figures
ike_did <- ike_did %>% 
  group_by(treatment, year) %>% 
  summarize(
    total_weights = sum(id_year_weight),
    productivity = sum(id_year_weight *log(exps_npo)),
    concentration = sum(id_year_weight *log(pop/NONPROFITS)),
    incomes = sum(id_year_weight *log(TOTREV/NONPROFITS)),
    wealth = sum(id_year_weight *log(TOTASS/NONPROFITS)),
    use = sum(id_year_weight *log(CONT/NONPROFITS)),
    dependence = sum(id_year_weight *log(CONT/TOTREV))
  )

# Get the year Ike hit
my_hityear <- max(main_df$hit_year)

# Plots
p1 <- ggplot(ike_did, aes(x=year, y=productivity, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Productivity\n") +
  scale_color_manual(values = natparks.pals("Yellowstone", 2),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p2 <- ggplot(ike_did, aes(x=year, y=concentration, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Concentration\n") +
  scale_color_manual(values = natparks.pals("Yellowstone", 2),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))


p3 <- ggplot(ike_did, aes(x=year, y=incomes, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Incomes\n") +
  scale_color_manual(values = natparks.pals("Yellowstone", 2),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))


p4 <- ggplot(ike_did, aes(x=year, y=wealth, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Wealth\n") +
  scale_color_manual(values = natparks.pals("Yellowstone", 2),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y=use, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Resource Use\n") +
  scale_color_manual(values = natparks.pals("Yellowstone", 2),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p6 <- ggplot(ike_did, aes(x=year, y=dependence, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Resource Dependence\n") +
  scale_color_manual(values = natparks.pals("Yellowstone", 2),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

png("Results/Figures/DiD_pop_weighted.png", width=8, height=6, 
    units="in", res=450)
print({
  ggarrange(
    p1, p2, p3, p4, p5, p6,
    ncol = 3, nrow = 2,
    labels = c("A","B","C","D","E","F"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()

ike_did <- ike_did %>% 
  pivot_wider(names_from = treatment, values_from = 4:9)

# Plots
p1 <- ggplot(ike_did, aes(x=year, y=productivity_1 - productivity_0)) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Productivity\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p2 <- ggplot(ike_did, aes(x=year, y=concentration_1 - concentration_0)) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Concentration\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))


p3 <- ggplot(ike_did, aes(x=year, y=incomes_1 - incomes_0)) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Incomes\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))


p4 <- ggplot(ike_did, aes(x=year, y=wealth_1 - wealth_0)) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Wealth\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y = use_1 - use_0)) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Resource Use\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p6 <- ggplot(ike_did, aes(x=year, y = dependence_1 -dependence_0)) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Resource Dependence\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

png("Results/Figures/differenced_pop_weighted.png", width=8, height=6, 
    units="in", res=450)
print({
  ggarrange(
    p1, p2, p3, p4, p5, p6,
    ncol = 3, nrow = 2,
    labels = c("A","B","C","D","E","F"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()

rm(ike_did, p1, p2, p3, p4, p5, p6, my_hityear)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Analysis --------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Make panel data frames
main_df$tpost_D <- ifelse(main_df$year >= main_df$hit_year, 1, 0)
df_storm <- pdata.frame(main_df, index = c("FIPS", "year"))
b_df_storm <- make.pbalanced(main_df, index = c("FIPS", "year"), 
                             balance.type = "shared.individuals")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### First Set of Regressions ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Regression Table 1

mod1 <- plm(log(exps_npo) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod2 <- plm(log(pop/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod3 <- plm(log(TOTREV/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod4 <- plm(log(TOTASS/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod5 <- plm(log(CONT/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod6 <- plm(log(CONT/TOTREV) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = df_storm,
            index = c("FIPS", "year"),
            model = "within")

stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
          title = "Hurricane Ike, Unbalanced Panel",
          omit = "factor",
          type = "html",
          order = c(6,3,5,4,2,1),
          font.size = 'footnotesize',
          covariate.labels = c(
            "Treatment * Post Storm * Log Cont",
            "Treatment * Post Storm",
            "Post Storm * Log Cont",
            "Treatment * Log Cont",
            "Log Cont",
            "Post Storm"
          ),
          dep.var.labels = c(
            "Productivity",
            "Concentration",
            "Incomes",
            "Wealth",
            "Resource Use",
            "Resource Dependence"
          ),
          column.sep.width = "20pt",
          out = 'Results/regression_results1.html')

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Second Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Regression Table 2

mod1 <- plm(log(exps_npo) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = b_df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod2 <- plm(log(pop/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = b_df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod3 <- plm(log(TOTREV/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = b_df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod4 <- plm(log(TOTASS/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = b_df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod5 <- plm(log(CONT/NONPROFITS) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = b_df_storm,
            index = c("FIPS", "year"),
            model = "within")

mod6 <- plm(log(CONT/TOTREV) ~ treatment*tpost_D*log(CONT) +
              as.factor(year) + as.factor(FIPS),
            data = b_df_storm,
            index = c("FIPS", "year"),
            model = "within")

stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
          title = "Hurricane Ike, Balanced Panel",
          omit = "factor",
          type = "html",
          order = c(6,3,5,4,2,1),
          font.size = 'footnotesize',
          covariate.labels = c(
            "Treatment * Post Storm * Log Cont",
            "Treatment * Post Storm",
            "Post Storm * Log Cont",
            "Treatment * Log Cont",
            "Log Cont",
            "Post Storm"
          ),
          dep.var.labels = c(
            "Productivity",
            "Concentration",
            "Incomes",
            "Wealth",
            "Resource Use",
            "Resource Dependence"
          ),
          column.sep.width = "20pt",
          out = 'Results/regression_results2.html')

rm(b_df_storm, df_storm, mod1, mod2, mod3, mod4, mod5, mod6)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Event Studies --------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

df <- main_df

df$center_year <- df$year - 2007

df <- df %>% mutate(center_year = ifelse(center_year==0, -20, center_year))

df <- df %>% 
  mutate(
    log_exps_npo = log(EXPS/NONPROFITS),
    log_pop_npo = log(pop/NONPROFITS),
    log_rev_npo = log(TOTREV/NONPROFITS),
    log_ass_npo = log(TOTASS/NONPROFITS),
    log_cont_npo = log(cont_npo),
    log_cont_rev = log(cont_rev)
  )


# Regular Event Study

run_event_study <- function(out_var, variable_lab, file_name){
  
  my_cols <- c("FIPS", "center_year", "treatment", "pop", out_var)
  my_df <- df[my_cols]
  colnames(my_df) <- c("FIPS", "center_year", "treatment", "pop", "outcome")
  
  temp <- lm(outcome ~ treatment*as.factor(center_year) + FIPS, 
             data = my_df,
             weights = pop)
  
  temp <- summary(temp)
  
  temp <- data.frame(temp$coefficients)
  temp$var <- rownames(temp)
  rownames(temp) <- NULL
  
  temp <- temp[grepl(":", temp$var),]
  temp[nrow(temp) +1,] <- c(0, 0, 0, 0, "0") 
  temp <- temp %>% 
    rowwise %>% 
    mutate(
      estimate = as.numeric(Estimate),
      std_err = as.numeric(`Std..Error`),
      period = as.integer(gsub(".*)", "", var)),
      ci_lower = estimate - 1.96 * std_err,
      ci_upper = estimate + 1.96 * std_err
    )
  
  
  print({
    ggplot(temp, aes(x = period + 2007, y = estimate)) + 
      geom_vline(xintercept = 2007, color="grey") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), size=0.5, width=0.5, 
                    color = natparks.pals("Yellowstone", 1)) +
      geom_point(aes(x=period + 2007, y=estimate), size=2, 
                 color = natparks.pals("Yellowstone", 1)) + 
      geom_line(color = natparks.pals("Yellowstone", 1)) +
      labs(x="\nYear (2007 = Pre-Storm Benchmark)", 
           y = "Estimated Effect\n") +
      geom_hline(yintercept=0) +
      ggtitle(label = variable_lab) +
      theme_bw()
    ggsave(paste("Results/Figures/Ike Event Studies/Population Weighted/", 
                 file_name, ".png", sep=""), width = 6, height = 4, units='in')
  })
  
}

run_event_study("log_exps_npo", "log(Expenses/Nonprofit)", "Outcome1")
run_event_study("log_pop_npo", "log(Population/Nonprofit)", "Outcome2")
run_event_study("log_rev_npo", "log(Revenues/Nonprofit)", "Outcome3")
run_event_study("log_ass_npo", "log(Assets/Nonprofit)", "Outcome4")
run_event_study("log_cont_npo", "log(Contributions/Nonprofit)", "Outcome5")
run_event_study("log_cont_rev", "log(Contributions/Revenue)", "Outcome6")

# Contribution Scaled -- Interpretation Challenged -- Event Study

run_event_study_2 <- function(out_var, variable_lab, file_name){
  
  my_cols <- c("FIPS", "center_year", "treatment", "CONT", out_var)
  my_df <- df[my_cols]
  colnames(my_df) <- c("FIPS", "center_year", "treatment", "cont", "outcome")
  
  temp <- lm(outcome ~ treatment*as.factor(center_year)*cont + FIPS, 
             data = my_df)
  
  temp <- summary(temp)
  
  temp <- data.frame(temp$coefficients)
  temp$var <- rownames(temp)
  rownames(temp) <- NULL
  
  temp <- temp[(grepl("treatment", temp$var)) & 
                 (grepl("cont", temp$var)) &
                 (grepl("center", temp$var)),]
  temp[nrow(temp) +1,] <- c(0, 0, 0, 0, "0") 
  temp <- temp %>% 
    rowwise %>% 
    mutate(
      estimate = as.numeric(Estimate) * 1000000000,
      std_err = as.numeric(`Std..Error`) * 1000000000,
      period = gsub(".*)", "", var),
      period = as.integer(gsub(":.*", "", period)),
      ci_lower = estimate - 1.96 * std_err,
      ci_upper = estimate + 1.96 * std_err
    )
  
  print({
    ggplot(temp, aes(x = period + 2007, y = estimate)) + 
      geom_vline(xintercept = 2007, color="grey") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), size=0.5, width=0.5,
                    color = natparks.pals("Yellowstone", 1)) +
      geom_point(aes(x=period + 2007, y=estimate), size=2, 
                 color = natparks.pals("Yellowstone", 1)) + 
      geom_line(color = natparks.pals("Yellowstone", 1)) +
      labs(x="\nYear (2007 = Pre-Storm Benchmark)", 
           y = "Estimated Effect\n") +
      geom_hline(yintercept=0) +
      ggtitle(label = variable_lab) +
      theme_bw()
    ggsave(paste("Results/Figures/Ike Event Studies/Contribution Scaled/", 
                 file_name, ".png", sep=""), 
           width = 6, height = 4, units='in')
  })
  
  
}

run_event_study_2("log_exps_npo", "log(Expenses/Nonprofit)", "Outcome1")
run_event_study_2("log_pop_npo", "log(Population/Nonprofit)", "Outcome2")
run_event_study_2("log_rev_npo", "log(Revenues/Nonprofit)", "Outcome3")
run_event_study_2("log_ass_npo", "log(Assets/Nonprofit)", "Outcome4")
run_event_study_2("log_cont_npo", "log(Contributions/Nonprofit)", "Outcome5")
run_event_study_2("log_cont_rev", "log(Contributions/Revenue)", "Outcome6")

rm(df, run_event_study, run_event_study_2)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


