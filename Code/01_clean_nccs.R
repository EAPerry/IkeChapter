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

#### Consolidate the NCCS Core Files ------------------------------------------
# 
# my_vars1 <- c(
#   "cont",
#   "ein",
#   "exps",
#   "fips",
#   "fisyr",
#   "fndncd",
#   "grprof",
#   "invinc",
#   "ass_eoy",
#   "liab_eoy",
#   "majgrpb",
#   "name",
#   "netinc",
#   "nteefinal",
#   "othinc",
#   "outnccs",
#   "ruledate",
#   "state",
#   "subseccd",
#   "taxper",
#   "totrev",
#   "govgtestimate",
#   "govgtcode",
#   "govgtyr"
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
#   "ZIP5",
#   "GovGtEstimate", 
#   "GovGtCode", 
#   "GovGtYr"
# )
# 
# my_vars3 <- c(
#   "p1tcont",
#   "ein",
#   "p1totexp",
#   "fips",
#   "fisyr",
#   "fndncd",
#   "p1ginvpf",
#   "p1netinv",
#   "p2totast",
#   "level1",
#   "level2",
#   "level3",
#   "level4",
#   "p2tliabl",
#   "majgrpb",
#   "name",
#   "p1nadinc",
#   "nteefinal",
#   "ntmaj10",
#   "ntmaj12",
#   "ntmaj5",
#   "p1othinc",
#   "outnccs",
#   "ruledate",
#   "state",
#   "subseccd",
#   "taxper",
#   "p1totrev",
#   "zip5",
#   "govgtestimate",
#   "govgtcode",
#   "govgtyr"
# )
# 
# my_vars1 <- c(
#   "CONT",
#   "CoreSrc",
#   "EIN",
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
# my_vars1_core <- c(
#     "cont",
#     "ein",
#     "exps",
#     "fips",
#     "fisyr",
#     "fndncd",
#     "grprof",
#     "invinc",
#     "ass_eoy",
#     "liab_eoy",
#     "majgrpb",
#     "name",
#     "netinc",
#     "nteefinal",
#     "othinc",
#     "outnccs",
#     "ruledate",
#     "state",
#     "subseccd",
#     "taxper",
#     "totrev",
#     "govgtestimate",
#     "govgtcode",
#     "govgtyr"
# )

charity_vars <- c(
  "ein",
  "fips",
  "fisyr",
  "nccskey",
  "totrev",
  "totrev2",
  "exps",
  "cont",
  "govgtestimate",
  "gvtsrv4"
)

nonprofit_vars <- c(
  "ein",
  "fips",
  "fisyr",
  "nccskey",
  "totrev",
  "totrev2",
  "exps",
  "cont",
  "gvtsrv4"
)

foundation_vars <- c(
  "ein",
  "fips",
  "fisyr",
  "nccskey",
  "p1totrev",
  "p1totexp",
  "p1tcont"
)


charities <- lapply(
  2000:2013,
  function (x) {
    df <- data.table::fread(
      paste0("Data/NCCS/pz-c3/CORE-", x, "-501C3-CHARITIES-PZ.csv"),
      select = charity_vars)
    df <- df %>% mutate(across(everything(), as.numeric))
    df$fips <- str_pad(df$fips, width = 5, side = 'left', pad = "0")
    df$ein <- str_pad(df$ein, width = 9, side = 'left', pad = '0')
    df$filename <- paste0("charities", x)
    return(df)
  })
charities <- bind_rows(charities)
# We have multiple observations for the same entity-year, make unique by keeping
# whichever was returned first
charities <- arrange(charities, ein, fisyr, nccskey)
charities <- charities %>% distinct(ein, fisyr, .keep_all = T)
charities <- charities %>% 
  mutate(totrev = ifelse(is.na(totrev),totrev2,totrev)) %>% 
  select(-totrev2)
# ^ gov Services not available in 2011-2013 date


nonprofits <- lapply(
  2000:2013,
  function (x) {
    df <- data.table::fread(
      paste0("Data/NCCS/pz-ce/CORE-", x, "-501CE-NONPROFIT-PZ.csv"),
      select = nonprofit_vars)
    df <- df %>% mutate(across(everything(), as.numeric))
    df$fips <- str_pad(df$fips, width = 5, side = 'left', pad = "0")
    df$ein <- str_pad(df$ein, width = 9, side = 'left', pad = '0')
    df$filename <- paste0("nonprofits", x)
    return(df)
  })
nonprofits <- bind_rows(nonprofits)
# We have multiple observations for the same entity-year, make unique by keeping
# whichever was returned first
nonprofits <- arrange(nonprofits, ein, fisyr, nccskey)
nonprofits <- nonprofits %>% distinct(ein, fisyr, .keep_all = T)
nonprofits <- nonprofits %>% 
  mutate(totrev = ifelse(is.na(totrev),totrev2,totrev))%>% 
  select(-totrev2)


foundations <- lapply(
  2000:2013,
  function (x) {
    df <- data.table::fread(
      paste0("Data/NCCS/pf/CORE-", x, "-501C3-PRIVFOUND-PF.csv"),
      select = foundation_vars)
    df <- df %>% mutate(across(everything(), as.numeric))
    df$fips <- str_pad(df$fips, width = 5, side = 'left', pad = "0")
    df$ein <- str_pad(df$ein, width = 9, side = 'left', pad = '0')
    df$filename <- paste0("foundation", x)
    return(df)
  })
foundations <- bind_rows(foundations)
# We have multiple observations for the same entity-year, make unique by keeping
# whichever was returned first
foundations <- arrange(foundations, ein, fisyr, nccskey)
foundations <- foundations %>% distinct(ein, fisyr, .keep_all = T)
foundations <- foundations %>% 
  rename(totrev = p1totrev, exps = p1totexp, cont = p1tcont)

nccs <- bind_rows(list(charities, foundations, nonprofits))
rm(charities, foundations, nonprofits)
rm(charity_vars,foundation_vars,nonprofit_vars)

# # Format everything just as in the other NCCS data
# charities <- charities %>% rename(EIN = ein, FISYR = fisyr)
# charities$EIN <- as.character(charities$EIN)
# 
# # Aggregae to the county level
# charities <- charities %>% 
#   filter(!is.na(fips)) %>% 
#   group_by(fips, FISYR) %>% 
#   summarise(govgtestimate = sum(govgtestimate, na.rm = T))
#   
# # All of these datasets are unique on EIN and fiscal year
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
# gc(reset = T)
# nccs <- bind_rows(df1, df2, df3)
# rm(df1, df2, df3, my_vars1, my_vars2)
# gc(reset = T)

#### Clean the NCCS Data -------------------------------------------------------

# Some that appear in several of the trend files, keep in order of co, pc, pf
nccs <- arrange(nccs, ein, fisyr, filename, nccskey)
gc(reset =  T)
nccs <- tibble(nccs)
nccs <- distinct(nccs, ein, fisyr, .keep_all = T)

# We can only look at the period from 2000 to 2012
nccs <- nccs[nccs$fisyr >= 2000, ]
nccs <- nccs[nccs$fisyr <= 2012, ]

# Remove any observations with a missing county
nccs <- nccs[!is.na(nccs$fips),]

colnames(nccs) <- str_to_upper(colnames(nccs))

backup <- nccs

# Group and summarize variables for each county and fiscal year
nccs <- nccs %>%
  group_by(FIPS, FISYR) %>%
  summarize(
    # # Concentration Indexes
    # # For now, do not group by industry
    # HHI_CONT = sum((CONT/sum(CONT, nar.rm=T))^2, na.rm = T),
    # HHI_EXPS = sum((EXPS/sum(EXPS, nar.rm=T))^2, na.rm = T),
    # HHI_GRPROF = sum((GRPROF/sum(GRPROF, nar.rm=T))^2, na.rm = T),
    # HHI_INVINC = sum((INVINC/sum(INVINC, nar.rm=T))^2, na.rm = T),
    # HHI_REV = sum((TOTREV/sum(TOTREV, nar.rm=T))^2, na.rm = T),
    # HHI_ASS = sum((ASS_EOY/sum(ASS_EOY, nar.rm=T))^2, na.rm = T),
    # Base sums
    NONPROFITS = n(),
    CONT = sum(CONT, na.rm = T),
    EXPS = sum(EXPS, na.rm = T),
    # GRPROF = sum(GRPROF, na.rm = T),
    # INVINC = sum(INVINC, na.rm = T),
    # OTHINC = sum(OTHINC, na.rm = T),
    TOTREV = sum(TOTREV, na.rm = T),
    # TOTASS = sum(ASS_EOY, na.rm = T),
    GOVGT = sum(GOVGTESTIMATE, na.rm = T),
    GOVSVC = sum(GVTSRV4, na.rm = T)
  ) %>%
  ungroup()

# Remove unused data from memory
gc()

# Just in case: Be sure to remove counties with a missing FIPS code
nccs <- nccs[!is.na(nccs$FIPS),]

# Create the unique ID variable
nccs$co_year <- paste(nccs$FIPS, nccs$FISYR, sep="-")

# # Read in and combine with the gov transfers data from the BEA
# bea <- read_csv("Data/GovTransfrs2npos.csv")
# bea <- bea %>% 
#   select(-Year) %>% 
#   pivot_longer(cols = as.character(2000:2013), names_to = "FISYR", values_to = "bea_gov") %>% 
#   mutate(FISYR = as.numeric(FISYR))
# 
# nccs <- left_join(nccs, bea, by = c("FIPS","FISYR"))

# Write to another file that will be small enough for GitHub
write_csv(nccs, "Data/nccs.csv", na="")

# Read in the cleaned NCCS data 
nccs <- read_csv("Data/nccs.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

