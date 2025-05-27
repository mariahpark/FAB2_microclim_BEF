# Calculate LMA from leaf scans

rm(list=ls())

library(magick)
library(LeafArea)

# Leaf scan notes
# 118 pixels = 1 cm
# Trim from left: 3.03 cm
# Trim from top: 2.97 cm
# Have a little buffer

left.trim = 118*3.04
top.trim.tape = 118*2.99 # Some images have tape
top.trim.no.tape = 118*0.3 # Some images don't have tape

# There are two input folders to analyze images from (tape and no tape)

#input_folder <- "C:/Users/maria/Desktop/Research/2024/lma/RAW_LMA_2024/tape"
input_folder <- "C:/Users/maria/Desktop/Research/2024/lma/RAW_LMA_2024/no_tape"
output_folder <- "C:/Users/maria/Desktop/Research/2024/lma/trimmed_LMA_2024"
dir.create(output_folder, showWarnings = FALSE)  # Create output folder

#-------------------------------------------------------------------------------
# List of JPEG images in the folder
image_files <- list.files(input_folder, pattern = "(?i)\\.jpe?g$", full.names = TRUE)

# Define the trimming amounts for each edge (top, right, bottom, left) for each image
# This is a data frame where each row corresponds to an image in `image_files`
# Adjust these values as per image requirements
trim_values <- data.frame(
  top = top.trim.no.tape, # Change based on if image has tape or no tape
  right = 0,
  bottom = 0,
  left = left.trim
)

#-------------------------------------------------------------------------------
# Loop through images and apply the trimming
for (image_path in image_files) {
  img <- image_read(image_path)  # Read image
  
  # Calculate the new width and height after trimming
  new_width <- image_info(img)$width - trim_values$left - trim_values$right
  new_height <- image_info(img)$height - trim_values$top - trim_values$bottom
  
  # Apply the cropping with specified offsets
  img_trimmed <- image_crop(
    img,
    geometry = geometry_area(
      width = new_width,
      height = new_height,
      x_off = trim_values$left,
      y_off = trim_values$top
    )
  )
  
  # Define output path
  output_path <- file.path(output_folder, basename(image_path))
  
  # Write the trimmed image to the output folder
  image_write(img_trimmed, output_path, format = "jpg")
  
  cat("Trimmed and saved:", output_path, "\n")
}

#-------------------------------------------------------------------------------
# Rename files so they aren't batch processed
# Define the folder path containing the images
folder_path <- "C:/Users/maria/Desktop/Research/2024/lma/trimmed_LMA_2024"

# List all JPEG files in the folder, case-insensitive
image_files <- list.files(folder_path, pattern = "(?i)\\.jpe?g$", full.names = TRUE)

# Loop through each file, remove hyphens, and rename
for (file_path in image_files) {
  # Extract the file name and remove hyphens
  file_name <- basename(file_path)
  new_file_name <- gsub("-", "", file_name)
  
  # Create the new file path
  new_file_path <- file.path(folder_path, new_file_name)
  
  # Rename the file
  file.rename(file_path, new_file_path)
  
  cat("Renamed:", file_path, "to", new_file_path, "\n")
}

#-------------------------------------------------------------------------------
# Save trimmed images and resulting dataframe with LMA data of each leaf scan

res <- run.ij (set.directory = "C:/Users/maria/Desktop/Research/2024/lma/trimmed_LMA_2024",
               distance.pixel = 118, known.distance = 1, save.image = TRUE)

fwrite(res, "C:/Users/maria/Desktop/Research/2024/lma/lma.2024.csv")
