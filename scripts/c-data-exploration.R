# Load necessary libraries
library(tidyverse)  # Core for data manipulation
library(skimr)      # For data summarization

# Import the cleaned dataset
cleaned_file_path <- "data/output/cleaned_movie_data.csv"
movie_data <- read_csv(cleaned_file_path)

# Create output directory if it doesn't exist
output_dir <- "data/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Generate a Skim summary of the dataset
skimmed_data <- skim(movie_data)

# Save the skim summary to a text file
skim_summary_path <- file.path(output_dir, "skim_summary.txt")
skim_summary <- capture.output(print(skimmed_data))  
writeLines(skim_summary, skim_summary_path)          
print(paste("Skim summary saved to:", skim_summary_path))

# Additional Descriptive Statistics for numeric columns
# - Calculate mean, median, standard deviation, variance, min, max, range, quantiles

numeric_cols <- movie_data %>% select(where(is.numeric))

# Calculate descriptive statistics using `reframe()` and `across()`
stats_summary <- numeric_cols %>%
  reframe(
    across(everything(), list(
      mean = ~mean(. , na.rm = TRUE),
      median = ~median(. , na.rm = TRUE),
      sd = ~sd(. , na.rm = TRUE),
      variance = ~var(. , na.rm = TRUE),
      min = ~min(. , na.rm = TRUE),
      max = ~max(. , na.rm = TRUE),
      range = ~diff(range(., na.rm = TRUE)),
      quantiles = ~quantile(., probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
      unique_values = ~n_distinct(.)
    ), .names = "{col}_{fn}")
  )

# Correlation matrix for numeric columns
correlation_matrix <- numeric_cols %>%
  cor(use = "complete.obs")

# Explore the structure of the dataset
dataset_structure <- capture.output(str(movie_data))
head_data <- capture.output(head(movie_data))
tail_data <- capture.output(tail(movie_data))

# Prepare output for additional statistics
stats <- c(
  "── Data Exploration and Descriptive Statistics ────────────────────────",
  
  "\nDescriptive Statistics for Numeric Columns:\n", capture.output(print(stats_summary)),
  
  "\nCorrelation Matrix for Numeric Columns:\n", capture.output(print(correlation_matrix)),
  
  "\nDataset Structure:\n", dataset_structure,
  
  "\nFirst 6 Rows (Head) of the Dataset:\n", head_data,
  
  "\nLast 6 Rows (Tail) of the Dataset:\n", tail_data
)

# Save the additional exploration summary to a text file
exploration_path <- file.path(output_dir, "exploration_summary.txt")
writeLines(stats, exploration_path)

# Print confirmation messages
print(paste("Exploration summary saved to:", exploration_path))
