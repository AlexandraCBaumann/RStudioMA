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


# Set working directory
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

  # Rename columns
  colnames(current_data)[colnames(current_data) == "UTM_Northing"] <- "x"
  colnames(current_data)[colnames(current_data) == "UTM_Easting"] <- "y"
  colnames(current_data)[colnames(current_data) == "Unix_Time"] <- "time"

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

####Plot and save unfiltered Trajectories####

output_dir <- "D:/Desktop/Masterarbeit/GitRepository/RStudioMA/Plots/Trajectory_Plots_Raw"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

individual_plots <- list()

# Loop through unique IDs and create ggplot2 plots
for (id in unique_ids) {
  # Get the current individual's data frame
  current_data <- individual_data_frames[[id]]
  
  # Create a ggplot2 plot for the current individual
  individual_plots[[id]] <- ggplot(current_data, aes(x = x, y = y, group = ID, color = as.factor(ID))) +
    geom_path() +
    labs(title = paste("Individual ID:", id),
         x = "X Coordinate",
         y = "Y Coordinate") +
    theme_minimal()
  
  # Generate a filename and save
  plot_filename <- file.path(output_dir, paste("Individual_ID_", id, ".png", sep = ""))
  ggsave(plot_filename, plot = individual_plots[[id]], width = 6, height = 4, dpi = 300)
}

cat("All individual plots have been saved to", output_dir, "\n")


####Smoothing trajectories#####

# Loop through the list of individual data frames to process each one
for (id in unique_ids) {
  # Get the current individual's data frame
  current_data <- individual_data_frames[[id]]
  
  # Remove speed outliers
  current_data <- atl_filter_covariates(
    data = current_data,  # Use 'data' instead of 'allfemales_data'
    filters = c("(speed_in < 15 & speed_out < 15)")
  )
  
  # Recalculate speed and angle
  current_data[, `:=`(
    speed_in = atl_get_speed(current_data, x = "x", y = "y", time = "time"),
    speed_out = atl_get_speed(current_data, type = "out")
  )]
  
  # add turning angle
  current_data[, angle := atl_turning_angle(data = current_data)]
  
  # Apply the atl_median_smooth function
  current_data <- atl_median_smooth(
    data = current_data,  # Use 'data' instead of 'allfemales_data'
    x = "x", y = "y", time = "time",
    moving_window = 5
  )
  
  # Update the data frame in the list
  individual_data_frames[[id]] <- current_data
}

####Plot and save smoothed Data###

individual_smoothed_plots <- list()
smoothed_output_dir <- "D:/Desktop/Masterarbeit/GitRepository/RStudioMA/Plots/Trajectory_Plots_Smoothed"

if (!dir.exists(smoothed_output_dir)) {
  dir.create(smoothed_output_dir, recursive = TRUE)
}

for (id in unique_ids) {
  # Get the current individual's smoothed data frame
  current_smoothed_data <- individual_data_frames[[id]]
  
  individual_smoothed_plots[[id]] <- ggplot(current_smoothed_data, aes(x = x, y = y, group = ID, color = as.factor(ID))) +
    geom_path() +
    labs(title = paste("Smoothed Individual ID:", id),
         x = "X Coordinate",
         y = "Y Coordinate") +
    theme_minimal()
  
  # Generate a filename for the smoothed plot, save
  smoothed_plot_filename <- file.path(smoothed_output_dir, paste("Smoothed_Individual_ID_", id, ".png", sep = ""))
  ggsave(smoothed_plot_filename, plot = individual_smoothed_plots[[id]], width = 6, height = 4, dpi = 300)
}

cat("All smoothed individual plots have been saved to", smoothed_output_dir, "\n")

