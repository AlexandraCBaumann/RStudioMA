#install.packages ("lidR")
#install.packages("RCSF")

PLOT <- FALSE
YEAR <- 2021

library(glue)
library(lidR)
library(RCSF)

input_folder <- toString(glue('F:/DOM/{YEAR}'))
dom_files <- list.files(input_folder, full.names=TRUE, recursive=TRUE, pattern = "*by.laz")

for (dom_file in dom_files) {
  
  # Find files and read them
  str_parts <- unlist(strsplit(dom_file, split="_"))
  x <- str_parts[2]
  y <- str_parts[3]
  dgm_file = toString(glue('F:/DGM/Oettingen/txt2las/{x}_{y}.laz'))
  if (!file.exists(dgm_file)) {
    print(glue("dgm file {dgm_file} does not exist."))
    next
  }
  lasDOM <- readLAS(dom_file)
  lasDGM <- readLAS(dgm_file)
  
  # Calculate nDOM
  ground = classify_ground(lasDGM, algorithm = csf()) 
  dtm <- grid_terrain(ground, 1, knnidw(k = 8, p = 2))
  #lasnDOM <- lasnormalize(lasDOM, dtm = dtm) # deprecated
  lasnDOM <- normalize_height(lasDOM, dtm)
 
  # Save File
  output_file <- toString(glue("F:/nDOM_laz/Oettingen/{YEAR}/{x}_{y}.laz"))
  writeLAS(lasnDOM, output_file)
  
  # Plot and stop for checking on the result
  if (PLOT) {
    plot(lasDGM,  size = 10, bg = "white")
    plot(lasDOM,  size = 10, bg = "white")
    plot(lasnDOM, size = 10, bg = "white")
    stop()
  }
  
  print(glue("Writing {output_file}."))
}




#lasunnormalize(las)

#‪F:\DGM\txt2las\646_5278.laz
#F:\DOM\2022\122040\dom04_646_5278_1_by.laz


