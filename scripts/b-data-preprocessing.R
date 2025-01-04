# Load necessary libraries
library(tidyverse)  # Core for data manipulation
library(janitor)    # For cleaning column names
library(skimr)      # For data summarization

# Import the dataset
data_file_path <- "data/movie_data.csv"
movie_data <- read_csv(data_file_path)

# Create output directory if it doesn't exist
output_dir <- "data/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Clean column names for consistency
movie_data <- movie_data %>%
  clean_names()  # Ensures all column names are in snake_case and consistent

# Check for missing values
# - Count missing values for each column
missing_values <- movie_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))
print("Missing values per column:")
print(missing_values)

# Impute or remove missing values
# - Impute `duration` with the mean
# - Remove rows where `title_year` or `imdb_score` are missing
movie_data <- movie_data %>%
  mutate(duration = ifelse(is.na(duration), mean(duration, na.rm = TRUE), duration)) %>%
  drop_na(title_year, imdb_score)

# Reformat columns for consistency
# - Rename specific columns for clarity
movie_data <- movie_data %>%
  rename(
    imdb_rating = imdb_score,
    release_year = title_year
  )

# Reorder columns for better readability
# - Move key columns like `movie_title` and `imdb_rating` to the front
movie_data <- movie_data %>%
  select(movie_title, imdb_rating, release_year, everything())

# Summarize cleaned dataset and save to a text file
skimmed_data <- skim(movie_data)
skim_summary_path <- file.path(output_dir, "skim_summary.txt")
skim_summary <- capture.output(print(skimmed_data))  
writeLines(skim_summary, skim_summary_path)          
print(paste("Skim summary saved to:", skim_summary_path))

# Save the cleaned dataset to the output folder
cleaned_data_path <- file.path(output_dir, "cleaned_movie_data.csv")
write_csv(movie_data, cleaned_data_path)
print(paste("Cleaned dataset saved to:", cleaned_data_path))
