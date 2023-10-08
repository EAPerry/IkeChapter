# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Clean Hurricane Data

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

# Purpose: This script reads and prepares the FEMA data to merge at the 
# county-year level with the other data.


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