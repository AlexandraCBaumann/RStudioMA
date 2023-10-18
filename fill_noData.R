# Install and load the lidR package if not already loaded
if (!require(lidR)) {
  install.packages("lidR")
  library(lidR)
}

# Input and output folders
input_folder <- "F:/nDOM_laz/Steingaden/2022/proj/Clipped"
output_folder <- "F:/nDOM_laz/Steingaden/2022/proj/Clipped/NAs"

# List .laz files in the input folder
laz_files <- list.files(input_folder, pattern = "\\.laz$", full.names = TRUE)

# Check if the output folder exists, create it if not
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Loop through .laz files, fill missing values with zeroes, and save in the output folder
for (laz_file in laz_files) {
  # Load the .laz file
  point_cloud <- readLAS(laz_file)
  
  # Fill missing values with zeroes
  point_cloud$Z[is.na(point_cloud$Z)] <- 0
  
  # Create the output file path by replacing the input folder path with the output folder path
  output_file <- file.path(output_folder, basename(laz_file))
  
  # Save the modified data to the output file
  writeLAS(point_cloud, output_file)
}

cat("Processing completed. Modified files saved in the output folder.\n")
