###################INSTALLATION###################
install.packages("devtools")

if (!require(remotes)) {
  install.packages("remotes", repos = "http://cran.us.r-project.org")
}

# installation using remotes
if (!require(atlastools)) {
  remotes::install_github("pratikunterwegs/atlastools", upgrade = FALSE)
}

install.packages("data.table")
install.packages("stringi")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("atlastools")
install.packages("recurse")
install.packages("ggthemes")
install.packages("sp")
install.packages("sf")
install.packages("rgdal")
install.packages("dplyr")
install.packages("knitr")



#####libraries######

library(data.table)
library(atlastools)
library(stringi)
library(sf)
library(rgdal)
library(dplyr)
library(knitr)
library(recurse)
library(ggplot2)
library(patchwork)
library(sp)

# making a colour palette
pal <- RColorBrewer::brewer.pal(5, "Set1")
pal[3] <- "seagreen"

############Access data and preliminary visualisation##############

# read and plot example data
## PC: D:/Desktop/Masterarbeit/GitRepository/RStudioMA/allfemales_till02-2023.csv
## Laptop: C:/Users/pauls/Desktop/Masterarbeit/RStudioMA/allfemales_till02-2023.csv
data <- fread("D:/Desktop/Masterarbeit/GitRepository/RStudioMA/allfemales_till02-2023.csv")
data_raw <- copy(data)



# see raw data
head(data_raw)

#####data category and omitting NAs#####

#str(data)

#sum(is.na(data_raw$Longitude))
#sum(is.na(data_raw$Latitude))

#na.omit(data_raw$Longitude)
#na.omit(data_raw$Latitude)

############GPS TO UTM####################

latitude <- data$Latitude
longitude <- data$Longitude

# Create a SpatialPoints object with the GPS coordinates
gps_coords <- SpatialPoints(matrix(c(longitude, latitude), ncol = 2), proj4string = CRS("+proj=longlat +datum=WGS84"))

# Define the UTM projection for Zone 32
utm_proj <- CRS("+proj=utm +zone=32 +datum=WGS84")
utm_coords <- spTransform(gps_coords, utm_proj)

# Extract UTM coordinates
utm_easting <- utm_coords@coords[, 1]
utm_northing <- utm_coords@coords[, 2]

# Add the UTM coordinates to the original data frame
data$UTM_Easting <- utm_easting
data$UTM_Northing <- utm_northing

# Select specific columns from the old file and UTM coordinates
selected_columns <- c("GMT Time", "Altitude", "Duration", "Temperature", "DOP", "Satellites", "Cause of Fix", "ID", "Voltage", "DateTime", "UTM_Easting", "UTM_Northing")
new_data <- data[selected_columns]

# Save the new data frame to a new CSV file
write.csv(new_data, "D:/Desktop/Masterarbeit/GitRepository/RStudioMA/updated_data_with_utm.csv", row.names = FALSE)

# Print a message
cat("UTM coordinates added to the same file: allfemales_till02-2023.csv\n")


# plot data
fig_new_data <-
  ggplot(new_data) +
  geom_path(aes(x = UTM_Easting, y = UTM_Northing),
            col = "grey", alpha = 1, size = 0.2
  ) +
  geom_point(aes(x = Longitude, y = Latitude),
             col = "grey", alpha = 0.2, size = 0.2
  ) +
  ggthemes::theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  coord_sf(crs = 25832) 

# save figure
ggsave(fig_data_raw,
       filename = "D:/Desktop/Masterarbeit/GitRepository/RStudioMA/Plots/fig_calibration_raw.png",
       width = 185 / 25
)

##########Read Data from GeoPackage (Studiengebiete) and find minimum and maximum values of polygons##########

gpkg_data <- st_read("D:/Desktop/Masterarbeit/Studiengebiete/Polygone/Oettingen_Steingaden.gpkg")
gpkg_sf <- st_as_sf(gpkg_data)

# Filter polygons
polygon_sf <- gpkg_sf[st_geometry_type(gpkg_sf) == "POLYGON"]

# Initialize empty lists to store coordinates
polygon_coordinates <- list()

# Loop through each polygon
for (i in 1:length(polygon_sf)) {
  bbox <- st_bbox(polygon_sf[i])
  polygon_coordinates[[i]] <- list(
    polygon_name = polygon_sf[i]$"1",  # Replace with your polygon's identifier
    min_coordinates = c(bbox["xmin"], bbox["ymin"]),
    max_coordinates = c(bbox["xmax"], bbox["ymax"])
  )
}

# Print the results for each polygon
for (i in 1:length(polygon_coordinates)) {
  polygon_info <- polygon_coordinates[[i]]
  cat("Polygon Name:", polygon_info$polygon_name, "\n")
  cat("Minimum Coordinates:", polygon_info$min_coordinates, "\n")
  cat("Maximum Coordinates:", polygon_info$max_coordinates, "\n")
  cat("\n")
}


##########Read Data from GeoPackage (Studiengebiete) and define Bounding Boxes ##########

gpkg_data <- st_read("D:/Desktop/Masterarbeit/Studiengebiete/Polygone/Oettingen_Steingaden.gpkg")


bounding_boxes <- data.frame(
  xmin = c(590761.3),  # Replace with actual values
  xmax = c(646606),    # Replace with actual values
  ymin = c(5276998),    # Replace with actual values
  ymax = c(5468585)     # Replace with actual values
)

# Define study areas
study_areas <- list(
  list(name = "Study Area 1", x_range = c(bounding_boxes[1, "xmin"], bounding_boxes[1, "xmax"]), y_range = c(bounding_boxes[1, "ymin"], bounding_boxes[1, "ymax"]))
)

filtered_data <- new_data

point_in_bbox <- function(point, bbox) {
  x_in_range <- point["UTM_Easting"] >= bbox["xmin"] & point["UTM_Easting"] <= bbox["xmax"]
  y_in_range <- point["UTM_Northing"] >= bbox["ymin"] & point["UTM_Northing"] <= bbox["ymax"]
  return(x_in_range & y_in_range)
}

# Filter data for each study area
filtered_data <- list()

for (area in study_areas) {
  bbox <- c("xmin" = area$x_range[1], "xmax" = area$x_range[2], "ymin" = area$y_range[1], "ymax" = area$y_range[2])
  filtered_data[[area$name]] <- new_data[apply(new_data, 1, point_in_bbox, bbox), ]
}

# Define the base plot
base_plot <- ggplot() +
  geom_path(data = new_data, aes(x = UTM_Easting, y = UTM_Northing),
            col = "grey", alpha = 1, size = 0.2) +
  geom_point(data = new_data, aes(x = UTM_Easting, y = UTM_Northing),
             col = "grey", alpha = 0.2, size = 0.2) +
  ggthemes::theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  coord_sf(crs = 25832)

# Loop through each study area and create a plot
for (area_name in names(filtered_data)) {
  study_data <- filtered_data[[area_name]]
  
  plot <- base_plot +
    geom_path(data = study_data, aes(x = UTM_Easting, y = UTM_Northing),
              col = pal[1], alpha = 1, size = 0.2) +
    geom_point(data = study_data, aes(x = UTM_Easting, y = UTM_Northing),
               col = pal[1], alpha = 0.5, size = 0.5)
  
  # Save the figure
  ggsave(plot,
         filename = "D:/Desktop/Masterarbeit/GitRepository/RStudioMA/Plots/fig_bounding_box.png",
         width = 185 / 25)
}


#for (area in study_areas) {
#  filtered_data <- atl_filter_bounds(
#    data = filtered_data,
#    longitude = "UTM_Easting", latitude = "UTM_Northing",
#    x_range = area$x_range,
#    y_range = area$y_range,
#    remove_inside = FALSE
#  )
#}

########## FILTER BY BOUNDING BOX #############

#data_unproc <- new_data

# remove inside must be set to falses
#new_data <- atl_filter_bounds(
#  data = new_data,
#  longitude = "Longitude", latitude = "Latitude",
#  x_range = c(645000, max(data$Longitude)),
#  remove_inside = FALSE
#)