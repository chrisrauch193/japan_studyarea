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
# Settings
sf_use_s2(FALSE)

# Define version
version <- 1

# Load shapefiles ----
# Load IHO borders
iho <- mr_shp(key = "MarineRegions:eez_iho", maxFeatures = 1000)

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
bbox <- c(xmin = 75, xmax = 175, ymin = -20, ymax = 75)

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

# --- Incorporate South Korea EEZ (MRGID 8327) ---
south_korea_eez <- mr_shp(key = "MarineRegions:eez", filter = 8327)

# Combine South Korea EEZ with the intersected IHO regions
inter.iho <- rbind(inter.iho, south_korea_eez)

# # Plot to see initial intersection
# ggplot() +
#   geom_sf(data = world, fill = "grey40", color = NA) +
#   geom_sf(data = inter.iho, fill = "grey70", color = "orange") +
#   geom_sf(data = asia.sel, fill = NA, color = "blue") +
#   coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)])

# Manual adjustments: Remove areas outside the desired scope
# This is where you can be creative and tailor the selection based on your knowledge of 
# anemonefish/host distribution and the Kuroshio Current.
# For example, you might want to exclude certain EEZs or IHO regions.

# # Example: Remove some EEZs based on sovereign (you'll need to inspect the map and adjust)
# inter.iho <- inter.iho |>
#   filter(!sovereign1 %in% c("Some Country", "Another Country")) # Replace with actual country names
# 
# # Example: Remove specific IHO sea regions if needed
# inter.iho <- inter.iho |>
#   filter(!iho_sea %in% c("Some Sea Region", "Another Sea Region")) # Replace with actual sea region names

# Refine study area by keeping only areas within bounding box
study.area <- st_crop(inter.iho, st_bbox(bbox))

# # Plot to see the refined study area
# ggplot() +
#   geom_sf(data = world, fill = "grey40", color = NA) +
#   geom_sf(data = study.area, fill = "grey70", color = "orange") +
#   geom_sf(data = asia.sel, fill = NA, color = "blue") +
#   coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)])

mapview(study.area)

# Unify in a single polygon and save ----
starea.un <- st_union(study.area)

# Further Smoothing (Optional)
# starea.un <- st_buffer(starea.un, dist = 0.02)
# starea.un <- st_buffer(starea.un, dist = -0.02)

# plot(starea.un)

# Save shapefile ----
fs::dir_create("data/shapefiles")
st_write(study.area, paste0("data/shapefiles/mpa_asia_starea_eez_v", version, ".shp"), delete_layer = TRUE)
st_write(starea.un, paste0("data/shapefiles/mpa_asia_starea_v", version, ".shp"), delete_layer = TRUE)


# --- Generate Explanatory Plots ---

# 1. Map of Asia with selected countries highlighted
plot_asia_countries <- ggplot() +
  geom_sf(data = world, fill = "grey60", color = "grey80") + # Show the whole world in the background
  geom_sf(data = asia.sel, fill = "deepskyblue", color = "grey80") + # Highlight selected countries
  coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)]) +
  labs(title = "A) Relevant Asian Countries for the Study")

ggsave(paste0("data/shapefiles/plot_asia_countries_v", version, ".png"), plot_asia_countries, width = 8, height = 6)

# 2. Map of the IHO/EEZ intersection grid within the bounding box
plot_iho_eez <- ggplot() +
  geom_sf(data = world, fill = "grey60", color = "grey80") +
  geom_sf(data = inter.iho, fill = NA, color = "deepskyblue") +
  coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)]) +
  labs(title = "B) IHO/EEZ Intersection Grid within Bounding Box")

ggsave(paste0("data/shapefiles/plot_iho_eez_v", version, ".png"), plot_iho_eez, width = 8, height = 6)

# 3. Final study area map
plot_final_study_area <- ggplot() +
  geom_sf(data = world, fill = "grey60", color = "grey80") + # Show the whole world in the background
  geom_sf(data = study.area, fill = "deepskyblue", color = "deepskyblue") +
  geom_sf(data = asia.sel, fill = NA, color = "grey80") + # Highlight selected countries
  coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)]) +
  labs(title = "C) Final Study Area for Anemonefish and Anemone Host Analysis")

ggsave(paste0("data/shapefiles/plot_final_study_area_v", version, ".png"), plot_final_study_area, width = 8, height = 6)

# --- End of Plot Generation ---