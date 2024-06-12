# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Merge All Data

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

# Purpose: This script merges the cleaned census, NCCS, and FEMA data.


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
    # concentration = concentration,
    # wealth = TOTASS/NONPROFITS,
    # distribution = DIST,
    output = EXPS/pop,
    # use = CONT/NONPROFITS,
    CONT2 = CONT - GOVGT - GOVSVC,
    dependence = CONT / TOTREV * 100,
    dependence2 = CONT2 / TOTREV * 100,
    dependence3 = CONT2 / CONT * 100
  ) %>% 
  rename(year = FISYR)

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
  "EXPS",
  "TOTREV",
  "pop",
  "density",
  "output",
  "CONT",
  "CONT2",
  "dependence",
  "dependence2",
  "dependence3",
  "GOVGT",
  "GOVSVC"
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
# Final Filtering (create the regression dataset) ------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Quick Tidying
df <- na.omit(main_df)

# Just until 2012
df <- df %>% 
  filter(year <= 2012)

df <- df %>% 
  filter(
    CONT > 0,
    CONT2 >= 0,
    EXPS > 0,
    TOTREV > 0,
    dependence < 100
  ) %>% 
  mutate(
    post = case_when(year>=hit_year ~ 1, TRUE ~ 0),              # create post dummy
    tpost = case_when(post==1 ~ as.numeric(year)-2008, TRUE ~ 0) # time since disaster
  )

# List of outcome variables
# leave out "dependence" because it is treated differently in regressions
# yvars <- c("density", "wealth", "output")

# Created a county-specific variable of contributions as of 2007
df <- df %>% 
  group_by(FIPS) %>% 
  mutate(fixedcont = sum(CONT * (year == 2007)),
         fixedcont2 = sum(CONT2 * (year == 2007))) %>% 
  ungroup() %>% 
  filter(year!=2008) #partially treated


write_csv(df, "Results/cleaned_filtered_data.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Alternate Version 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# fema <- read.csv("Data/FEMA_Disasters.csv")
# 
# fema <- fema %>% 
#   unite(FIPS, starts_with("fips"), sep = "")
# 
# #vector of FIPS to omit in estimation (Katrina counties)
# katrina <- fema %>%
#   filter(declarationTitle %in% c("HURRICANE IKE", "HURRICANE KATRINA")) %>% 
#   filter((disasterNumber >= 1 & disasterNumber <= 1999) | 
#            (disasterNumber >= 4000 & disasterNumber <= 4999)) %>% 
#   select(FIPS, declarationTitle) %>%
#   group_by(FIPS, declarationTitle) %>% 
#   summarize(value = n()) %>% 
#   pivot_wider(names_from = declarationTitle, values_fill = 0) %>%
#   filter(`HURRICANE KATRINA`==1) %>% 
#   distinct(FIPS)
# 
# ##Code for counting lost counties (29) by omitting Katrina
# # mutate(across(starts_with("HU"), ~as.numeric(.))) %>% 
# # mutate(code = case_when(
# #   `HURRICANE KATRINA` > 0 & `HURRICANE IKE` == 0 ~ "Katrina only",
# #   `HURRICANE KATRINA` == 0 & `HURRICANE IKE` > 0 ~ "Ike only",
# #   TRUE ~ "Both")
# #   ) %>% 
# # ungroup() %>% 
# # filter(code=="Ike only") %>% 
# # select(FIPS)
# 
# ##store unfiltered df
# dat <- df
# 
# df <- dat %>% 
#   filter(!FIPS %in% katrina$FIPS)
# 
# ###Shrink the control group perimeter
# ike <- fema %>%
#   filter(declarationTitle=="HURRICANE IKE")
# 
# dist <- read.csv("Data/sf12010countydistance500miles.csv")
# 
# distance <- dist %>% 
#   filter(mi_to_county<50) %>% 
#   mutate(FIPS = as.character(county1)) %>% 
#   right_join(ike) %>% 
#   distinct(county2)
# 
# df <- dat %>% 
#   filter(FIPS %in% distance$county2|FIPS %in% ike$FIPS)
# 
# rm(list = ls())

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
