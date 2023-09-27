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

#####FILTER TRAJECTORIES ####

# add incoming and outgoing speed
data[, `:=`(
  speed_in = atl_get_speed(data,
                           Longitude = "x",
                           Latitude = "y",
                           Unix_Time = "time"
  ),
  speed_out = atl_get_speed(data, type = "out")
)]

# add turning angle
data[, angle := atl_turning_angle(data = data)]
