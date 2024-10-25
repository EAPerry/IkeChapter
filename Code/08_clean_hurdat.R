# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Clean HURDAT Data

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2024-10-19

# Purpose: This script reads and prepares the HURDAT data.

# All necessary packages are loaded in 00_MAIN.R

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read and Format HURDAT Data --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Read the HURDAT2 Data
hurdat <- read_delim("Data/hurdat/hurdat2-1851-2023-051124.txt", delim = ",", skip = 1, col_names = F)

# Add rownumbers
hurdat$rownum <- 1:nrow(hurdat)

# Warnings here are because it has rows in the data with only four columns
# as headers for each hurricane
tmp_header <- hurdat %>% 
  filter(substr(X1,1,1) == "A") %>% 
  select(X1,X2,X3,rownum) %>% 
  rename(stormid = X1, stormname = X2, stormnumber = X3) %>% 
  mutate(across(everything(), str_trim),
         rownum = cumsum(rownum))
  
# Use rownumber to identify what rows we want here
hurdat <- hurdat %>% 
  mutate(rownum = ifelse(substr(X1,1,1) == "A", rownum, 0)) %>% 
  mutate(rownum = cumsum(rownum))
  
# Merge these together
hurdat <- hurdat %>% 
  left_join(tmp_header, by='rownum')
# The unmatched rows are from the first hurricane in 1851; we don't mind losing that


# Remove the rows with just the storm headers and clean the columns
hurdat <- hurdat %>% 
  filter(substr(X1,1,1) != "A") %>% 
  mutate(across(paste0("X",c(1:2,7:21)), as.numeric)) %>% 
  mutate(across(paste0("X",7:21), ~ ifelse(.x == -999, NA, .x)))

# Coordinates
hurdat <- hurdat %>% 
  mutate(
    lat = as.numeric(str_remove(str_remove(X5, "N"), "S")),
    lat = ifelse(str_detect(X5, "S"), -lat, lat),
    lon = as.numeric(str_remove(str_remove(X6, "W"), "E")),
    lon = ifelse(str_detect(X6, "W"), -lon, lon)
  )

# Rename all the columns
hurdat <- hurdat %>% 
  rename(
    date = X1,
    time = X2,
    record_status_id = X3,
    storm_class = X4,
    lat_str = X5,
    lon_str = X6,
    max_wind = X7,
    min_pressure = X8,
    ne_34kt_wind_radii = X9,
    se_34kt_wind_radii = X10,
    sw_34kt_wind_radii = X11,
    nw_34kt_wind_radii = X12,
    ne_50kt_wind_radii = X13,
    se_50kt_wind_radii = X14,
    sw_50kt_wind_radii = X15,
    nw_50kt_wind_radii = X16,
    ne_64kt_wind_radii = X17,
    se_64kt_wind_radii = X18,
    sw_64kt_wind_radii = X19,
    nw_64kt_wind_radii = X20,
    max_wind_radii = X21
  )

# Just keep the hurricanes in there since 2000 for now
hurdat <- hurdat %>% 
  filter(date > 20040000)

# Add mean radii for each knots band
hurdat <- hurdat %>% 
  mutate(
    knts34_radii = .25*(ne_34kt_wind_radii + nw_34kt_wind_radii + se_34kt_wind_radii + sw_34kt_wind_radii),
    knts50_radii = .25*(ne_50kt_wind_radii + nw_50kt_wind_radii + se_50kt_wind_radii + sw_50kt_wind_radii),
    knts64_radii = .25*(ne_64kt_wind_radii + nw_64kt_wind_radii + se_64kt_wind_radii + sw_64kt_wind_radii)
  )

rm(tmp_header)
  
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Convert to Spatial -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# US States
all_states <- tigris::states(cb = T, year = 2023)
all_states <- all_states %>% 
  filter(GEOID %in% fips_codes$state_code) %>% 
  filter(!(STUSPS %in% c('AS','MP','GU','PR','VI','AK','HI')))

# Counties
county_shp <- lapply(all_states$STATEFP, function(x) tigris::counties(x, cb = T))
county_shp <- bind_rows(county_shp)

# Add geometry
hurdat_sf <- st_as_sf(hurdat, coords = c('lon','lat'), crs = st_crs(county_shp))

# Write function to get circular zones
get_circ_zone <- function (i, x_df, distvar) {
  
  x_sf_tmp <- x_df[i,]
  radii_tmp = x_df[i,distvar] %>% st_drop_geometry()
  radii_tmp = unlist(as.vector.data.frame(radii_tmp))
  x_sf_tmp = x_sf_tmp %>% 
    st_buffer(dist = radii_tmp * 1.15078 * 1609.34)
  return(x_sf_tmp)
  
}

# Write wrapper function
make_shp_files <- function (x_df, distvar) {
  
  # Only make shapes for non-missing, non-zero radii
  x_df <- x_df[(!is.na(x_df[[distvar]])),]
  x_df <- x_df[(x_df[[distvar]] > 0),]
  
  # Make circles for each observation
  x_sf <- lapply(1:nrow(x_df), function (x) get_circ_zone(x, x_df, distvar))
  
  # Combine these into one dataset
  x_sf <- bind_rows(x_sf)
  
  return(x_sf)
  
}

# Make all the circles
max_winds_sf <- make_shp_files(hurdat_sf, 'max_wind_radii')
high_winds_sf <- make_shp_files(hurdat_sf, 'knts64_radii')
mid_winds_sf <- make_shp_files(hurdat_sf, 'knts50_radii')
low_winds_sf <- make_shp_files(hurdat_sf, 'knts34_radii')

# Combine these
max_winds_sf$circle_of <- "max winds"
high_winds_sf$circle_of <- "high winds"
mid_winds_sf$circle_of <- "mid winds"
low_winds_sf$circle_of <- "low winds"
wind_shp <- bind_rows(max_winds_sf,high_winds_sf,mid_winds_sf,low_winds_sf)
rm(max_winds_sf,high_winds_sf,mid_winds_sf,low_winds_sf)


# Total area of each county
county_shp$total_area <- st_area(county_shp)

# Compute the area of the intersection of all the hurricanes with each county
county_measures <- st_intersection(county_shp, wind_shp)
county_measures$area <- st_area(county_measures)

# Adjust area covered with the concentric circles in mind
county_measures <- county_measures %>% 
  st_drop_geometry() %>% 
  select(GEOID,date,time,area,circle_of,max_wind)
county_measures <- county_measures %>% 
  pivot_wider(names_from = circle_of, values_from = area)
county_measures <- county_measures %>% 
  mutate(`max winds` = ifelse(is.na(`max winds`),0,`max winds`),
         `high winds` = ifelse(is.na(`high winds`),0,`high winds`),
         `mid winds` = ifelse(is.na(`mid winds`),0,`mid winds`),
         `low winds` = ifelse(is.na(`low winds`),0,`low winds`))
county_measures <- county_measures %>% 
  mutate(
    `high winds` = ifelse(`high winds` > 0, `high winds` - `max winds`, 0),
    `mid winds` = ifelse(`mid winds` > 0, `mid winds` - `high winds` - `max winds`, 0),
    `low winds` = ifelse(`low winds` > 0, `low winds` - `mid winds` - `high winds` - `max winds`, 0)
  ) %>% 
  mutate(across(ends_with('winds'), ~ round(.x, 0)))

# Bring back the counties
county_measures <- merge(county_shp, county_measures, by='GEOID', all.x = T)
county_measures <- county_measures %>% 
  mutate(across(ends_with('winds'), ~ replace_na(.x, 0)))

# Compute the score in each time period
county_measures <- county_measures %>% 
  mutate(
    wind_score = `max winds`*max_wind/total_area +  `high winds`/total_area*64 + `mid winds`/total_area*50 + `low winds`/total_area*34
  )

# Sum over all time periods in a year
county_measures <- county_measures %>% 
  mutate(year = floor(date/10000)) %>% 
  group_by(GEOID,year) %>% 
  summarise(total_wind_score = sum(wind_score, na.rm = T)) 
county_measures <- county_measures %>% 
  ungroup() %>% 
  mutate(total_wind_score = replace_na(total_wind_score, 0))

# Balance out the panel
county_measures <- county_measures %>% 
  st_drop_geometry() %>% 
  select(GEOID,year,total_wind_score) %>% 
  mutate(total_wind_score = replace_na(total_wind_score,0))
county_measures = left_join(
  expand.grid(GEOID = unique(county_measures$GEOID), year = unique(county_measures$year)),
  county_measures, by=c('GEOID','year'))
county_measures <- county_measures %>% 
  mutate(total_wind_score = ifelse(is.na(total_wind_score),0,total_wind_score))

# Save this 
write_csv(county_measures, "Data/hurdat/process_county_measures.csv")


# Now bring back the geometry
county_measures <- merge(county_shp, county_measures, by = "GEOID")

pdf("Results/Figures/hurdat_construction/full_measure_2017.pdf", width = 7.15, height = 4.75)
tm_shape(county_measures %>% filter(year == 2017)) +
  tm_polygons(col = 'total_wind_score', border.alpha = 0.05,
              title = "Time-Area Weighted Average\nBase Wind Speed (knots x time periods)", 
              palette = 'Blues',legend.is.portrait=F) +
  tm_shape(all_states) +
  tm_borders() + 
  tm_layout(legend.outside = T, legend.outside.position = 'bottom', frame = F)
dev.off()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Example: Hurricane Ike -------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Change the CRS to something less ugly
county_shp <- st_transform(county_shp, crs = 'ESRI:102003')
all_states <- st_transform(all_states, crs = 'ESRI:102003')
wind_shp <- st_transform(wind_shp, crs = 'ESRI:102003')

# Plot Ike as an example
ike <- wind_shp %>% filter(stormname == "IKE") %>% 
  mutate(
    wind_speed = case_when(
      circle_of == 'low winds' ~ 34,
      circle_of == 'mid winds' ~ 50,
      circle_of == 'high winds' ~ 64,
      circle_of == 'max winds' ~ max_wind,
    )
  )

# # The path of Ike
# pdf("")
# tm_shape(county_shp) +
#   tm_borders(alpha = 0.1) +
#   tm_shape(all_states) +
#   tm_borders() + 
#   tm_shape(hurdat_sf %>% filter(stormname == "IKE")) +
#   tm_dots(col = 'red', size = 0.2) +
#   tm_layout(frame=F)



# Single point in time: show wind speed contours
pdf("Results/Figures/hurdat_construction/ike_single_period.pdf", width = 7.15, height = 4.75)
tm_shape(county_shp) +
  tm_borders(alpha = 0.1) +
  tm_shape(all_states) +
  tm_borders() +
  tm_shape(ike %>% filter(date == 20080913, time == 0) %>% arrange(wind_speed)) +
  tm_polygons(col = 'wind_speed', alpha = 0.7, palette = 'Blues', legend.is.portrait = F, style = 'cat', title='Base Wind Speeds (knots)') + 
  tm_layout(legend.outside = F, legend.position = c(0,0))
dev.off()

# All the points in time
pdf("Results/Figures/hurdat_construction/ike_all_period.pdf", width = 7.15, height = 4.75)
tm_shape(county_shp) +
  tm_borders(alpha = 0.1) +
  tm_shape(all_states) +
  tm_borders() +
  tm_shape(ike %>% arrange(wind_speed)) +
  tm_polygons(col = 'wind_speed', alpha = 0.7, palette = 'Blues', legend.is.portrait = F, style = 'cat', title='Base Wind Speeds (knots)') + 
  tm_layout(legend.outside = F, legend.position = c(0,0))
dev.off()



# # Animation
# facet_version <- tm_shape(county_shp) +
#   tm_borders(alpha = 0.1) +
#   tm_shape(all_states) +
#   tm_borders() +
#   tm_shape(ike %>% mutate(datetime = date*10000 + time) %>% arrange(wind_speed)) +
#   tm_polygons(col = 'wind_speed', alpha = 0.7, palette = 'Blues') +
#   tm_facets(along = 'datetime')
# tmap_animation(facet_version)




# Compute the intersection of all the hurricanes with each county
county_measures <- st_intersection(county_shp, wind_shp %>% filter(stormname == "IKE"))
county_measures$area <- st_area(county_measures)
county_measures <- county_measures %>% 
  st_drop_geometry() %>% 
  select(GEOID,date,time,area,circle_of,max_wind)
county_measures <- county_measures %>% 
  pivot_wider(names_from = circle_of, values_from = area)
county_measures <- county_measures %>% 
  mutate(`max winds` = ifelse(is.na(`max winds`),0,`max winds`),
         `high winds` = ifelse(is.na(`high winds`),0,`high winds`),
         `mid winds` = ifelse(is.na(`mid winds`),0,`mid winds`),
         `low winds` = ifelse(is.na(`low winds`),0,`low winds`))

county_measures <- county_measures %>% 
  mutate(
    `high winds` = `high winds` - `max winds`,
    `mid winds` = `mid winds` - `high winds` - `max winds`,
    `low winds` = `low winds` - `mid winds` - `high winds` - `max winds`
  ) %>% 
  mutate(across(ends_with('winds'), ~ round(.x, 0)))


county_measures <- merge(county_shp, county_measures, by='GEOID', all.x = T)
county_measures <- county_measures %>% 
  mutate(across(ends_with('winds'), ~ replace_na(.x, 0)))

# Compute the score in each time period
county_measures <- county_measures %>% 
  mutate(
    wind_score = `max winds`*max_wind/total_area +  `high winds`/total_area*64 + `mid winds`/total_area*50 + `low winds`/total_area*34
  )


# tm_shape(county_measures %>% filter(date==20080913,time==700)) +
#   tm_polygons(col = 'wind_score', title = "Area Weighted Average\nBase Wind Speed (knots)", palette = 'Blues',legend.is.portrait=F) +
#   tm_shape(wind_shp %>% filter(stormname=="IKE",date==20080913,time==700)) +
#   tm_borders() +
#   tm_layout(legend.outside = T, legend.outside.position = 'bottom')

# Sum over all time periods
county_measures <- county_measures %>% 
  mutate(year = floor(date/10000)) %>% 
  group_by(GEOID,year) %>% 
  summarise(total_wind_score = sum(wind_score, na.rm = T)) 

county_measures <- county_measures %>% 
  ungroup() %>% 
  mutate(total_wind_score = replace_na(total_wind_score, 0))


pdf("Results/Figures/hurdat_construction/full_measure_ike.pdf", width = 7.15, height = 4.75)
tm_shape(county_measures) +
  tm_polygons(col = 'total_wind_score', border.alpha = 0.1,
              title = "Time-Area Weighted Average\nBase Wind Speed (knots x time periods)", 
              palette = 'Blues',legend.is.portrait=F) +
  tm_shape(all_states) +
  tm_borders() + 
  tm_layout(legend.outside = T, legend.outside.position = 'bottom', frame = F)
dev.off()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

