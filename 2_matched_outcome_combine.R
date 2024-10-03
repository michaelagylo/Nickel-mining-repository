#                 #2. Combine matched samples with outcome data
# Introduction:
# This R script combines the matched samples with the outcome variables
# There are two sources of matched samples that could be used: 
# 1. prepared data: already available data to save computer processing time
# 2. gm_output/matched data:  these data are results from after running 
#                             the gm_matching script.
#  Sources can be chosen by changing the "input_path" 
# All data will be processed and saved in the "final_output/final_data/" folder


# Prepare workspace ----
# Clean the working environment
rm(list=ls())

#load packages
library("dplyr")

# If not using the RStudio project, set working directory to the repository
# directory. 
# setwd("../")

# Get the outcome variable data  
y_path <- c("prepared_data/")
NiYdata <- readRDS(paste0(y_path, "Nidata_outcome.RDS"))
NiYdata <- NiYdata %>% 
   mutate_at(.,vars(kab_code, prov_code),as.factor)

OtYdata <- readRDS(paste0(y_path, "OTdata_outcome.RDS"))
OtYdata <- OtYdata %>% 
   mutate_at(.,vars(kab_code, prov_code),as.factor)

#Process function ----
# Define the function to read, join, and save RDS files to final_data folder
process_rds <- function(file_name,
                        input_path,
                        output_path,
                        join_data,
                        join_cols) {
   file_path <- paste0(input_path, file_name)
   data <- readRDS(file_path)
   var.factor <- c("MPIBASE_CLS2", "LTYPE.11", "prov_code", "kab_code",
                   "id", "subclass")
   data <- data %>% 
      mutate_at(.,vars(paste0(var.factor)),as.factor)
   data <- data %>% left_join(join_data, by = join_cols)
   saveRDS(data, paste0(output_path, file_name))
}

# Set the path where the matched data will be sourced from
input_path <- c("prepared_data/")
# replace input path if running data from gm_matching output:
# import_file_path <- c("gm_output/matched-data/")

# Set up the path where the combined data will be stored:
dir.create("final_output/")
dir.create("final_output/final_data/")
output_path <- c("final_output/final_data/")
join_cols <- c("UniqueID", "kab_code", "prov_code")

# List of files to process
# matched data for nickel mines 
nickel_file_names <- c("Nickel_whole.RDS",
                       "Nickel_short1.RDS",
                       "Nickel_short2.RDS",
                       "Nickel_long.RDS",
                       "Nickel_short1_Low.RDS",
                       "Nickel_short1_High.RDS",
                       "Nickel_short2_Low.RDS",
                       "Nickel_short2_High.RDS",
                       "Nickel_long_Low.RDS",
                       "Nickel_long_High.RDS"
)
# matched data for other mines
other_file_names <- c("Other_whole.RDS",
                      "Other_short1.RDS",
                      "Other_short2.RDS",
                      "Other_long.RDS"
)

# Process each file using the function
for (file_name in nickel_file_names) {
   process_rds(file_name, input_path, output_path, NiYdata, join_cols)
}

# Process each file using the function
for (file_name in other_file_names) {
   process_rds(file_name, input_path, output_path, OtYdata, join_cols)
}

# END
