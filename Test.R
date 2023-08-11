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



# for data handling
library(data.table)
library(atlastools)
library(stringi)

# for recursion analysis
library(recurse)

# for plotting
library(ggplot2)
library(patchwork)

# making a colour palette
pal <- RColorBrewer::brewer.pal(5, "Set1")
pal[3] <- "seagreen"

############Access data and preliminary visualisation##############

# read and plot example data
data <- fread("D:/Desktop/Masterarbeit/GitRepository/RStudioMA/allfemales_till02-2023.csv")
data_raw <- copy(data)

# see raw data
head(data_raw)
#>            TAG          TIME NBS VARX VARY COVXY   SD           Timestamp
#> 1: 31001001060 1598027365845   6 6.28 2.85 1.682 3.53 2020-08-21 17:29:25
#> 2: 31001001060 1598027366845   6 2.23 2.23 0.277 2.24 2020-08-21 17:29:26
#> 3: 31001001060 1598027367845   6 2.94 2.82 0.612 2.64 2020-08-21 17:29:27
#> 4: 31001001060 1598027368845   6 8.45 3.68 2.734 4.20 2020-08-21 17:29:28
#> 5: 31001001060 1598027369845   5 6.80 3.26 2.273 3.82 2020-08-21 17:29:29
#> 6: 31001001060 1598027370845   6 3.95 2.94 0.983 2.98 2020-08-21 17:29:30
#>            id      x       y Long  Lat             UTCtime    tID
#> 1: 2020-08-21 650083 5902624 5.25 53.3 2020-08-21 16:29:25 DELETE
#> 2: 2020-08-21 650083 5902624 5.25 53.3 2020-08-21 16:29:26 DELETE
#> 3: 2020-08-21 650073 5902622 5.25 53.3 2020-08-21 16:29:27 DELETE
#> 4: 2020-08-21 650079 5902625 5.25 53.3 2020-08-21 16:29:28 DELETE
#> 5: 2020-08-21 650067 5902621 5.25 53.3 2020-08-21 16:29:29 DELETE
#> 6: 2020-08-21 650071 5902621 5.25 53.3 2020-08-21 16:29:30 DELETE
#> 
#> 
#> 
#> 
#> # plot data
fig_data_raw <-
  ggplot(data) +
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


########## FILTER BY BOUNDING BOX #############

data_unproc <- copy(data)

# remove inside must be set to falses
data <- atl_filter_bounds(
  data = data,
  Longitude = "x", Latitude = "y",
  x_range = c(645000, max(data$x)),
  remove_inside = FALSE
)