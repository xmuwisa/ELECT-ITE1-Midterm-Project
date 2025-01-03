# Load required libraries
library(tidyverse)
library(janitor)

# Define file paths
input_file <- "data/movie_data.csv"
output_file <- "data/cleaned_movie_data.csv"

# Import the dataset
cat("Importing the dataset...\n")
if (!file.exists(input_file)) {
  stop("Input file not found. Please ensure the file exists in the 'data/' folder.")
}
movie_data <- read.csv(input_file)

# Display basic structure of the dataset
cat("\nDataset structure:\n")
print(str(movie_data))

# Check for missing values
cat("\nChecking for missing values...\n")
missing_summary <- colSums(is.na(movie_data))
print(missing_summary)

# Handle missing values
cat("\nHandling missing values...\n")

# Replace missing numerical values (e.g., duration, num_user_for_reviews, title_year) with mean
movie_data <- movie_data %>%
  mutate(across(c(duration, num_user_for_reviews, title_year), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Replace missing categorical values (e.g., director_name, language, country) with "Unknown"
movie_data <- movie_data %>%
  mutate(across(c(director_name, actor_2_name, actor_1_name, actor_3_name, language, country), 
                ~ ifelse(is.na(.), "Unknown", .)))

# Rename columns for consistency
cat("\nRenaming columns for consistency...\n")
movie_data <- movie_data %>%
  clean_names() # Standardize column names to snake_case

# Reorder columns if necessary (example: move 'imdb_score' to the first position)
movie_data <- movie_data %>%
  select(imdb_score, everything())

# Save the cleaned dataset
cat("\nSaving the cleaned dataset...\n")
write.csv(movie_data, output_file, row.names = FALSE)

cat("Preprocessing complete. Cleaned data saved to:", output_file, "\n")