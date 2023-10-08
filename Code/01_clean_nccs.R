# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Clean NCCS Data

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

# Purpose: This script reads and prepares the NCCS data to merge at the 
# county-year level with the other data.


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

