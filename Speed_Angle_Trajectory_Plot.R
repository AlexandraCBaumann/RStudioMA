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
data <- fread("D:/Desktop/Masterarbeit/GitRepository/RStudioMA/clipped_allfemales_till02-2023_UTM_unixtime.csv")
data_raw <- copy(data)


# see raw data
head(data_raw)

# List of IDs you want to split by
ids_to_split <- c(187026, 187369, 187372, 287370, 087026, 087368, 087369, 087371, 087371, 087372, 087373, 087373, 087374, 087374, 087375, 087375, 087376, 087376, 087377, 087378, 087379, 087380, 087380, 087381, 087381, 087382, 087382, 087383, 087383, 087384, 087384, 087385, 087385, 087386, 087386, 087388, 087389, 087390, 087391, 087391, 087393, 087395, 087396, 087397, 089183, 089184, 089185, 089186, 089187, 089188, 089189, 089190, 089191, 089192, 089193, 187370, 187377, 187388, 089191, 089192, 089193, 089188, 089189, 089183, 089184, 087393, 087395, 087396, 087389, 087390, 187382, 189186)

# Create a list of data frames, each containing data for a specific individual
individual_data_frames <- split(data, data$ID)

# Filter the list to keep only the data frames for the specified IDs
filtered_data_frames <- individual_data_frames[ids_to_split]

# Now, you have a list of data frames, with each data frame containing data for a specific individual
# You can access each individual's data using list indexing, e.g., filtered_data_frames[[1]] for the first individual



#####FILTER TRAJECTORIES ####


#define x, y and time
colnames(data)[colnames(data) == "UTM_Northing"] <- "x"
colnames(data)[colnames(data) == "UTM_Easting"] <- "y"
colnames(data)[colnames(data) == "Unix_Time"] <- "time"

# add incoming and outgoing speed
data[, `:=`(
  speed_in = atl_get_speed(data,
                           x = "x",
                           y = "y",
                           time = "time"
  ),
  speed_out = atl_get_speed(data, type = "out")
)]

# add turning angle
data[, angle := atl_turning_angle(data = data)]

# use sapply
speed_angle_thresholds <-
  sapply(data[, list(speed_in, speed_out, angle)],
         quantile,
         probs = 0.9, na.rm = T
  )


# remove speed outliers
data <- atl_filter_covariates(
  data = data,
  filters = c("(speed_in < 15 & speed_out < 15)")
)

# recalculate speed and angle
data[, `:=`(
  speed_in = atl_get_speed(data,
                           x = "x",
                           y = "y",
                           time = "time"
  ),
  speed_out = atl_get_speed(data, type = "out")
)]

# add turning angle
data[, angle := atl_turning_angle(data = data)]



# now apply the smooth
atl_median_smooth(
  data = data,
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