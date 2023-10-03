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
#library(lubridate)
#install.packages("lubridate")

# Set your working directory
setwd("D:/Desktop/Masterarbeit/GitRepository/RStudioMA")

# Read the CSV file
allfemales_data <- read.csv("clipped_allfemales_till02-2023_UTM_unixtime.csv")

# List of IDs you want to split by
ids_to_split <- c(187026, 187369, 187372, 287370, 87026, 87368, 87369, 87371, 87371, 87372, 87373, 87373, 87374, 87374, 87375, 87375, 87376, 87376, 87377, 87378, 87379, 87380, 87380, 87381, 87381, 87382, 87382, 87383, 87383, 87384, 87384, 87385, 87385, 87386, 87386, 87388, 87389, 87390, 87391, 87391, 87393, 87395, 87396, 87397, 89183, 89184, 89185, 89186, 89187, 89188, 89189, 89190, 89191, 89192, 89193, 187370, 187377, 187388, 89191, 89192, 89193, 89188, 89189, 89183, 89184, 87393, 87395, 87396, 87389, 87390, 187382, 189186)

# Create a list of data frames, each containing data for a specific individual
individual_data_frames <- split(allfemales_data, allfemales_data$ID)

# Loop through each ID in ids_to_split
for (i in ids_to_split) {
  # Check if the ID exists in the list
  if (i %in% names(individual_data_frames)) {
    # Get the data frame for the current individual
    current_data_frame <- individual_data_frames[[i]]
    
    # Rename columns "UTM_Northing" to "x", "UTM_Easting" to "y", and "Unix_Time" to "time"
    colnames(current_data_frame)[colnames(current_data_frame) == "UTM_Northing"] <- "x"
    colnames(current_data_frame)[colnames(current_data_frame) == "UTM_Easting"] <- "y"
    colnames(current_data_frame)[colnames(current_data_frame) == "Unix_Time"] <- "time"
    
    # Replace the data frame in the list with the updated data frame
    individual_data_frames[[i]] <- current_data_frame
  }
}

#define x, y and time
colnames(data)[colnames(allfemales_data) == "UTM_Northing"] <- "x"
colnames(data)[colnames(allfemales_data) == "UTM_Easting"] <- "y"
colnames(data)[colnames(allfemales_data) == "Unix_Time"] <- "time"


# Create a list to store individual plots
individual_plots <- list()

#####FILTER TRAJECTORIES ####




# add incoming and outgoing speed
allfemales_data[, `:=`(
  speed_in = atl_get_speed(allfemales_data,
                           x = "x",
                           y = "y",
                           time = "time"
  ),
  speed_out = atl_get_speed(allfemales_data, type = "out")
)]

# add turning angle
allfemales_data[, angle := atl_turning_angle(allfemales_data = allfemales_data)]

# use sapply
speed_angle_thresholds <-
  sapply(allfemales_data[, list(speed_in, speed_out, angle)],
         quantile,
         probs = 0.9, na.rm = T
  )


# remove speed outliers
allfemales_data <- atl_filter_covariates(
  allfemales_data = allfemales_data,
  filters = c("(speed_in < 15 & speed_out < 15)")
)

# recalculate speed and angle
allfemales_data[, `:=`(
  speed_in = atl_get_speed(allfemales_data,
                           x = "x",
                           y = "y",
                           time = "time"
  ),
  speed_out = atl_get_speed(allfemales_data, type = "out")
)]

# add turning angle
allfemales_data[, angle := atl_turning_angle(allfemales_data = allfemales_data)]



# now apply the smooth
atl_median_smooth(
  allfemales_data = allfemales_data,
  x = "x", y = "y", time = "time",
  moving_window = 5
)


# Define the UTM32N CRS
utm_crs <- st_crs("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

# List of IDs to plot
ids_to_plot <- c(187026, 187369, 187372, 287370, 087026, 087368, 087369, 087371, 087371, 087372, 087373, 087373, 087374, 087374, 087375, 087375, 087376, 087376, 087377, 087378, 087379, 087380, 087380, 087381, 087381, 087382, 087382, 087383, 087383, 087384, 087384, 087385, 087385, 087386, 087386, 087388, 087389, 087390, 087391, 087391, 087393, 087395, 087396, 087397, 089183, 089184, 089185, 089186, 089187, 089188, 089189, 089190, 089191, 089192, 089193, 187370, 187377, 187388, 089191, 089192, 089193, 089188, 089189, 089183, 089184, 087393, 087395, 087396, 087389, 087390, 187382, 189186)

# Filter data for selected IDs
filtered_data <- data[data$ID %in% ids_to_plot, ]

# Create a ggplot object to plot trajectories
ggplot(filtered_data, aes(x = x, y = y, group = ID, color = as.factor(ID))) +
  geom_path() +
  labs(title = "Trajectories of Individuals",
       x = "X Coordinate",
       y = "Y Coordinate") +
  theme_minimal()