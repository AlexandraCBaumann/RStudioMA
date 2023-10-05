install.packages("devtools", "dplyr", "data.table", "stringi", "ggplot2", "patchwork", "atlastools", "recurse", "ggthemes", "sp", "sf", "rgdal", "knitr")

library(dplyr)
library(data.table)
library(atlastools)
library(stringi)
library(sf)
library(rgdal)
library(knitr)
library(recurse)
library(ggplot2)
library(patchwork)
library(sp)


# Set your working directory
setwd("D:/Desktop/Masterarbeit/GitRepository/RStudioMA")

# Read the CSV file
allfemales_data <- read.csv("clipped_allfemales_till02-2023_UTM_unixtime.csv")

# Clean and standardize the IDs in the 'ID' column
allfemales_data$ID <- as.character(allfemales_data$ID)  # Convert to character
allfemales_data$ID <- gsub("\"", "", allfemales_data$ID)  # Remove double quotes if present
allfemales_data$ID <- gsub("^0+", "", allfemales_data$ID)  # Remove leading zeros

# Get unique, cleaned IDs
unique_ids <- unique(allfemales_data$ID)

# Create an empty list to store data frames
individual_data_frames <- list()


# Loop through unique IDs and split data
for (id in unique_ids) {
  # Subset data for the current ID
  current_data <- allfemales_data[allfemales_data$ID == id, ]

  # Rename columns if needed (similar to your previous code)
  colnames(current_data)[colnames(current_data) == "UTM_Northing"] <- "x"
  colnames(current_data)[colnames(current_data) == "UTM_Easting"] <- "y"
  colnames(current_data)[colnames(current_data) == "Unix_Time"] <- "time"


  # Store the data frame in the list
  individual_data_frames[[id]] <- current_data
}

#####FILTER TRAJECTORIES ####

# Loop through the list of individual data frames to calculate speed and angle
for (id in unique_ids) {
  # Get the current individual's data frame
  current_data <- individual_data_frames[[id]]
  # Convert the data frame to a data.table
  current_data <- as.data.table(current_data)
  #Calculate incoming and outgoing speed columns
  current_data[, `:=`(
    speed_in = atl_get_speed(current_data,
                             x = "x",
                             y = "y",
                             time = "time"),
    speed_out = atl_get_speed(current_data, type = "out")
  )]

  
  # Calculate turning angle column
  current_data[, angle := atl_turning_angle(data = current_data)]
  
  # Calculate speed and angle thresholds
  speed_angle_thresholds <-
    sapply(current_data[, list(speed_in, speed_out, angle)],
           quantile,
           probs = 0.9, na.rm = TRUE
    )
  
  # Store the updated data frame in the list
  individual_data_frames[[id]] <- current_data
}

# Create an empty list to store individual plots
individual_plots <- list()

# Loop through unique IDs and create ggplot2 plots
for (id in unique_ids) {
  # Get the current individual's data frame
  current_data <- individual_data_frames[[id]]
  
  # Print the ID to check which one is being processed
  print(id)
  
  # Create a ggplot2 plot for the current individual
  individual_plots[[id]] <- ggplot(current_data, aes(x = x, y = y, group = ID, color = as.factor(ID))) +
    geom_path() +
    labs(title = paste("Individual ID:", id),
         x = "X Coordinate",
         y = "Y Coordinate") +
    theme_minimal()
}

# Print the individual plots to the console
print(individual_plots)



# # Loop through the list of individual data frames to process each one
# for (id in unique_ids) {
#   # Get the current individual's data frame
#   current_data <- individual_data_frames[[id]]
#   
#   # Remove speed outliers
#   current_data <- atl_filter_covariates(
#     allfemales_data = current_data,
#     filters = c("(speed_in < 15 & speed_out < 15)")
#   )
#   
#   # Recalculate speed and angle
#   current_data[, `:=`(
#     speed_in = atl_get_speed(current_data, x = "x", y = "y", time = "time"),
#     speed_out = atl_get_speed(current_data, type = "out")
#   )]
#   
#   current_data[, angle := atl_turning_angle(allfemales_data = current_data)]
#   
#   # Apply the atl_median_smooth function
#   current_data <- atl_median_smooth(
#     allfemales_data = current_data,
#     x = "x", y = "y", time = "time",
#     moving_window = 5
#   )
#   
#   # Update the data frame in the list
#   individual_data_frames[[id]] <- current_data
# }

# 
# 
# # remove speed outliers
# allfemales_data <- atl_filter_covariates(
#   allfemales_data = allfemales_data,
#   filters = c("(speed_in < 15 & speed_out < 15)")
# )
# 
# # recalculate speed and angle
# allfemales_data[, `:=`(
#   speed_in = atl_get_speed(allfemales_data,
#                            x = "x",
#                            y = "y",
#                            time = "time"
#   ),
#   speed_out = atl_get_speed(allfemales_data, type = "out")
# )]
# 
# # add turning angle
# allfemales_data[, angle := atl_turning_angle(allfemales_data = allfemales_data)]
# 
# 
# 
# # now apply the smooth
# atl_median_smooth(
#   allfemales_data = allfemales_data,
#   x = "x", y = "y", time = "time",
#   moving_window = 5
# )


# Define the UTM32N CRS
utm_crs <- st_crs("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")


# 
# # Create a ggplot object to plot trajectories
# ggplot(filtered_data, aes(x = x, y = y, group = ID, color = as.factor(ID))) +
#   geom_path() +
#   labs(title = "Trajectories of Individuals",
#        x = "X Coordinate",
#        y = "Y Coordinate") +
#   theme_minimal()