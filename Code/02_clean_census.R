# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Clean Census Population Data

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

# Purpose: This script reads and prepares the census data to merge at 
# the county-year level with the other data.


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
df_list <- list()

# Process the two population files one at a time
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
