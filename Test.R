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

str(data)

sum(is.na(data_raw$Longitude))
sum(is.na(data_raw$Latitude))

na.omit(data_raw$Longitude)
na.omit(data_raw$Latitude)

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


#> # plot data
fig_new_data <-
  ggplot(new_data) +
  geom_path(aes(x = Longitude, y = Latitude),
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
    polygon_name = polygon_sf[i]$name_column,  # Replace with your polygon's identifier
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

study_areas <- list(
  list(name = "Oettingen", x_range = c(590761.3, 646606)),
  list(name = "Steingaden", x_range = c(5276998, 5468585)),
)

filtered_data <- new_data

for (area in study_areas) {
  filtered_data <- atl_filter_bounds(
    data = filtered_data,
    longitude = "Longitude", latitude = "Latitude",
    x_range = area$x_range,
    remove_inside = FALSE
  )
}

########## FILTER BY BOUNDING BOX #############

data_unproc <- new_data

# remove inside must be set to falses
new_data <- atl_filter_bounds(
  data = new_data,
  longitude = "Longitude", latitude = "Latitude",
  x_range = c(645000, max(data$Longitude)),
  remove_inside = FALSE
)