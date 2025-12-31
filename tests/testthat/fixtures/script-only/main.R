# Non-Shiny R script
# Tests that portR can build distributions for any R script, not just Shiny

message("Starting data processing pipeline...")

# Load required libraries
library(dplyr)
library(readr)

# Configuration
input_dir <- "input"
output_dir <- "output"

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process all CSV files in input directory
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  message("No CSV files found in input directory. Creating sample data...")
  
  # Create sample data
  sample_data <- data.frame(
    id = 1:100,
    value = rnorm(100, mean = 50, sd = 10),
    category = sample(LETTERS[1:5], 100, replace = TRUE),
    date = seq.Date(Sys.Date() - 99, Sys.Date(), by = "day")
  )
  
  write_csv(sample_data, file.path(input_dir, "sample.csv"))
  csv_files <- file.path(input_dir, "sample.csv")
}

# Process each file
for (file in csv_files) {
  message("Processing: ", basename(file))
  
  data <- read_csv(file, show_col_types = FALSE)
  
  # Apply transformations
  processed <- data %>%
    mutate(processed_at = Sys.time()) %>%
    arrange(desc(value))
  
  # Write output
  output_file <- file.path(output_dir, paste0("processed_", basename(file)))
  write_csv(processed, output_file)
  
  message("  -> Saved to: ", output_file)
}

# Generate summary report
summary_report <- lapply(csv_files, function(f) {
  data <- read_csv(f, show_col_types = FALSE)
  data.frame(
    file = basename(f),
    rows = nrow(data),
    cols = ncol(data)
  )
}) %>% bind_rows()

write_csv(summary_report, file.path(output_dir, "summary.csv"))

message("Processing complete!")
message("Summary saved to: ", file.path(output_dir, "summary.csv"))
