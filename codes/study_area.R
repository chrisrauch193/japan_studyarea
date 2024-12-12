### MPA ASIA PROJECT - OBIS ###
### Define the study area for the project ###

# Modified from MPA Europe code by s.principe@unesco.org
# For Anemonefish and Anemone Host SDM/HSM and Larval Dispersal
# May 2024 - [Your Name]

# Load packages ----
# Obtaining marine regions
library(mregions)
# Spatial manipulation
library(sf)
library(tidyverse)
# Other/visualization
library(rnaturalearth)
library(ggplot2)
library(mapview)
library(dplyr)
# Settings
sf_use_s2(FALSE)

# Define version
version <- 1

# Load shapefiles ----
# Load IHO borders
iho <- mr_shp(key = "MarineRegions:eez_iho", maxFeatures = 3000)

# Load Asia shapefile from Natural Earth
asia <- ne_countries(scale = "large", continent = "asia")
asia <- st_as_sf(asia)

# Load world shapefile (for plotting)
world <- ne_countries(scale = "large")
world <- st_as_sf(world)

# Define Bounding Box for Kuroshio Current Region
# xmin: 110 (covers the eastern part of the South China Sea)
# xmax: 160 (extends east to follow the Kuroshio Current path)
# ymin: -5 (includes areas south of Taiwan, Philippines, potential for southward dispersal)
# ymax: 50 (extends north along the Japanese coast, following the Kuroshio Current)
bbox <- c(xmin = 110, xmax = 160, ymin = -5, ymax = 50)

# Select relevant area ----
# Filter Asia countries relevant to the study area (Japan, Taiwan, Philippines, etc.)
# You might need to adjust this list based on your specific needs.
asia.sel <- asia |>
  filter(admin %in% c("Japan", "Taiwan", "Philippines", "Vietnam", "China", 
                      "South Korea", "North Korea", "Indonesia", "Malaysia",
                      "Brunei", "East Timor")) |>
  select(admin)

# Crop to the bounding box
asia.sel <- st_crop(asia.sel, bbox)

# Intersect with IHO borders
# We add a small buffer to ensure all areas are covered
inter <- st_intersects(iho,
                       st_buffer(asia.sel, 0.3), sparse = F)

inter <- apply(inter, 1, any)

inter.iho <- iho[inter,]

# Plot to see initial intersection
ggplot() +
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = inter.iho, fill = "grey70", color = "orange") +
  geom_sf(data = asia.sel, fill = NA, color = "blue") +
  coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)])

# Manual adjustments: Remove areas outside the desired scope
# This is where you can be creative and tailor the selection based on your knowledge of 
# anemonefish/host distribution and the Kuroshio Current.
# For example, you might want to exclude certain EEZs or IHO regions.

# Example: Remove some EEZs based on sovereign (you'll need to inspect the map and adjust)
# inter.iho <- inter.iho |>
#   filter(!sovereign1 %in% c("Some Country", "Another Country")) # Replace with actual country names

# Example: Remove specific IHO sea regions if needed
# inter.iho <- inter.iho |>
#   filter(!iho_sea %in% c("Some Sea Region", "Another Sea Region")) # Replace with actual sea region names

# Refine study area by keeping only areas within bounding box
study.area <- st_crop(inter.iho, st_bbox(bbox))

# Plot to see the refined study area
ggplot() +
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = study.area, fill = "grey70", color = "orange") +
  geom_sf(data = asia.sel, fill = NA, color = "blue") +
  coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)])

mapview(study.area)

# Unify in a single polygon and save ----
starea.un <- st_union(study.area)

# Further Smoothing (Optional)
starea.un <- st_buffer(starea.un, dist = 0.02)
starea.un <- st_buffer(starea.un, dist = -0.02)

plot(starea.un)

# Save shapefile ----
fs::dir_create("data/shapefiles")
st_write(study.area, paste0("data/shapefiles/mpa_asia_starea_eez_v", version, ".shp"), delete_layer = TRUE)
st_write(starea.un, paste0("data/shapefiles/mpa_asia_starea_v", version, ".shp"), delete_layer = TRUE)





# Load occurrence data
occurrence_data_list <- readRDS("~/seminar/japan_studyarea/data/occurrence/occurrence_data_list.rds")

# Combine all occurrence data into one data frame (if it's a list)
occurrence_data <- do.call(rbind, occurrence_data_list)

# Convert to sf object (ensure CRS is WGS84)
occurrence_sf <- st_as_sf(occurrence_data, coords = c("longitude", "latitude"), crs = 4326)

# Create the plot with a global view
ggplot() +
  # World background
  geom_sf(data = world, fill = "grey40", color = NA) +
  # Study area
  geom_sf(data = study.area, fill = "grey70", color = "orange") +
  # Asia selection (if applicable)
  geom_sf(data = asia.sel, fill = NA, color = "blue") +
  # Occurrence points
  geom_sf(data = occurrence_sf, color = "red", size = 1, alpha = 0.7) +
  # Remove bounding box limits to show the whole world
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Global Map with Study Area and Occurrence Points",
    subtitle = "Overlay of occurrence points on study area across the world"
  )