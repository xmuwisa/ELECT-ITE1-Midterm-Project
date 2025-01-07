# Load necessary libraries
library(tidyverse)  # Data manipulation
library(janitor)    # Clean column names
library(skimr)      # Summary statistics
library(ggplot2)    # Visualization
library(car)        # Statistical tests

# --- B ---

# 1. Import dataset
import_dataset <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  read_csv(file_path)
}

# 2. Clean dataset
clean_dataset <- function(dataset) {
  dataset %>%
    clean_names() %>%
    mutate(duration = ifelse(is.na(duration), mean(duration, na.rm = TRUE), duration)) %>%
    drop_na(title_year, imdb_score) %>%
    rename(imdb_rating = imdb_score, release_year = title_year) %>%
    select(movie_title, imdb_rating, release_year, everything())
}

# 3. Save cleaned dataset
save_cleaned_dataset <- function(dataset, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_path <- file.path(output_dir, "cleaned_data.csv")
  write_csv(dataset, output_path)
  cat(paste("Cleaned dataset saved to:", output_path, "\n"))
}

# --- C ---

# 4. Generate skim summary
generate_data_summary <- function(dataset, output_dir) {
  skimmed_data <- skim(dataset)
  output_path <- file.path(output_dir, "skim_summary.txt")
  writeLines(capture.output(print(skimmed_data)), output_path)
  cat(paste("Skim summary saved to:", output_path, "\n"))
}

# 5. Generate descriptive statistics
generate_descriptive_statistics <- function(dataset, output_dir) {
  numeric_cols <- dataset %>% select(where(is.numeric))
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
  output_path <- file.path(output_dir, "descriptive_statistics.txt")
  writeLines(capture.output(print(stats_summary)), output_path)
  cat(paste("Descriptive statistics saved to:", output_path, "\n"))
}

# 6. Generate correlation matrix
generate_correlation_matrix <- function(dataset, output_dir) {
  numeric_cols <- dataset %>% select(where(is.numeric))
  correlation_matrix <- cor(numeric_cols, use = "complete.obs")
  output_path <- file.path(output_dir, "correlation_matrix.txt")
  writeLines(capture.output(print(correlation_matrix)), output_path)
  cat(paste("Correlation matrix saved to:", output_path, "\n"))
}

# 7. Generate dataset structure
generate_data_structure <- function(dataset, output_dir) {
  output_path <- file.path(output_dir, "data_structure.txt")
  structure_data <- c(
    "Dataset Structure:\n",
    capture.output(str(dataset)),
    "\nFirst 6 Rows:\n",
    capture.output(head(dataset)),
    "\nLast 6 Rows:\n",
    capture.output(tail(dataset))
  )
  writeLines(structure_data, output_path)
  cat(paste("Dataset structure saved to:", output_path, "\n"))
}

# --- D ---

# 9. Visualize histograms
visualize_histograms <- function(dataset, output_dir) {
  histograms <- list(
    imdb_rating = ggplot(dataset, aes(x = imdb_rating)) +
      geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Distribution of IMDb Ratings", x = "IMDb Rating", y = "Frequency"),
    duration = ggplot(dataset, aes(x = duration)) +
      geom_histogram(binwidth = 10, fill = "orange", color = "black", alpha = 0.7) +
      labs(title = "Distribution of Movie Duration", x = "Duration (minutes)", y = "Frequency")
  )
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  for (name in names(histograms)) {
    ggsave(file.path(output_dir, paste0(name, "_histogram.png")), histograms[[name]])
  }
}

# 10. Visualize boxplots
visualize_boxplots <- function(dataset, output_dir) {
  boxplots <- list(
    imdb_rating = ggplot(dataset, aes(y = imdb_rating)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      labs(title = "IMDb Rating Boxplot", y = "IMDb Rating"),
    duration = ggplot(dataset, aes(y = duration)) +
      geom_boxplot(fill = "lightcoral", color = "darkred") +
      labs(title = "Duration Boxplot", y = "Duration (minutes)")
  )
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  for (name in names(boxplots)) {
    ggsave(file.path(output_dir, paste0(name, "_boxplot.png")), boxplots[[name]])
  }
}

# 11. Visualize scatterplots
visualize_scatterplots <- function(dataset, output_dir) {
  scatterplots <- list(
    imdb_vs_duration = ggplot(dataset, aes(x = imdb_rating, y = duration)) +
      geom_point(color = "purple", alpha = 0.5) +
      labs(title = "IMDb Rating vs. Movie Duration", x = "IMDb Rating", y = "Duration (minutes)"),
    imdb_vs_votes = ggplot(dataset, aes(x = imdb_rating, y = num_voted_users)) +
      geom_point(color = "green", alpha = 0.5) +
      labs(title = "IMDb Rating vs. Number of Voted Users", x = "IMDb Rating", y = "Number of Voted Users")
  )
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  for (name in names(scatterplots)) {
    ggsave(file.path(output_dir, paste0(name, "_scatter.png")), scatterplots[[name]])
  }
}

# 12. Visualize bar charts
visualize_bar_charts <- function(dataset, output_dir) {
  genre_distribution <- dataset %>%
    mutate(genre = str_split(genres, "\\|")) %>%
    unnest(genre) %>%
    count(genre) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(genre, n), y = n, fill = genre)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Top 10 Most Frequent Movie Genres", x = "Genre", y = "Count")
  
  language_distribution <- dataset %>%
    count(language) %>%
    ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Language Distribution in Movies", x = "Language", y = "Count")
  
  ggsave(file.path(output_dir, "genre_distribution.png"), genre_distribution, height = 4)
  ggsave(file.path(output_dir, "language_distribution.png"), language_distribution, height = 12)
}

# 13. Visualize correlation matrix
visualize_correlation_matrix <- function(dataset, output_dir) {
  numeric_cols <- dataset %>% select(where(is.numeric))
  correlation_matrix <- cor(numeric_cols, use = "complete.obs")
  correlation_data <- as.data.frame(as.table(correlation_matrix))
  heatmap <- ggplot(correlation_data, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")
  ggsave(file.path(output_dir, "correlation_heatmap.png"), heatmap, width = 4, height = 4)
}

# --- E ---

# 14. Perform T-test
generate_t_test <- function(dataset, output_dir) {
  dataset <- dataset %>%
    mutate(rating_group = ifelse(imdb_rating > 7, "High", "Low"))
  t_test <- t.test(imdb_rating ~ rating_group, data = dataset)
  output_path <- file.path(output_dir, "analysis_summary.txt")
  write("── T-test for IMDb Ratings between High and Low-Rated Movies ──\n", output_path, append = TRUE)
  write(capture.output(print(t_test)), output_path, append = TRUE)
  cat("T-test results saved to:", output_path, "\n")
}

# 15. Perform Fisher's Exact Test
generate_fisher_test <- function(dataset, output_dir) {
  fisher_table <- dataset %>%
    mutate(genre = str_split(genres, "\\|")) %>%
    unnest(genre) %>%
    count(genre, language) %>%
    pivot_wider(names_from = language, values_from = n, values_fill = list(n = 0))
  fisher_test <- fisher.test(as.matrix(fisher_table[-1]), simulate.p.value = TRUE, B = 10000)
  output_path <- file.path(output_dir, "analysis_summary.txt")
  write("\n── Fisher's Exact Test for Independence between Genre and Language ──\n", output_path, append = TRUE)
  write(capture.output(print(fisher_test)), output_path, append = TRUE)
  cat("Fisher's Exact Test results saved to:", output_path, "\n")
}

# Perform Chi-square Test
generate_chi_square_test <- function(dataset, output_dir) {
  dataset <- dataset %>%
    mutate(
      country_group = ifelse(country %in% c("USA", "UK", "India"), country, "Other"),
      language_group = ifelse(language %in% c("English", "French", "Spanish"), language, "Other")
    )
  contingency_table <- table(dataset$country_group, dataset$language_group)
  chi_test_result <- chisq.test(contingency_table)
  output_path <- file.path(output_dir, "analysis_summary.txt")
  write("\n── Chi-square Test for Independence between Country Group and Language Group ──\n", output_path, append = TRUE)
  write(capture.output(print(chi_test_result)), output_path, append = TRUE)
  cat("Chi-square Test results saved to:", output_path, "\n")
}


# 16. Identify Outliers
generate_identified_outliers <- function(dataset, output_dir) {
  outliers <- dataset %>%
    filter(imdb_rating > 9 | imdb_rating < 2)
  output_path <- file.path(output_dir, "analysis_summary.txt")
  write("\n── Potential Anomalies in IMDb Ratings (Outliers) ──\n", output_path, append = TRUE)
  write(capture.output(print(outliers)), output_path, append = TRUE)
  cat("Outliers saved to:", output_path, "\n")
}

# 17. Generate Correlation Matrix
generate_correlation_heatmap <- function(dataset, output_dir) {
  numeric_cols <- dataset %>% select(where(is.numeric))
  correlation_matrix <- cor(numeric_cols, use = "complete.obs")
  output_path <- file.path(output_dir, "correlation_matrix.txt")
  write(capture.output(print(correlation_matrix)), output_path)
  cat("Correlation matrix saved to:", output_path, "\n")
}

# 18. Generate IMDb Rating Trend Plot
generate_trend_plot <- function(dataset, output_dir) {
  trend_plot <- ggplot(dataset, aes(x = release_year, y = imdb_rating)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    theme_minimal() +
    labs(title = "IMDb Rating Trend Over Time", x = "Release Year", y = "IMDb Rating")
  output_path <- file.path(output_dir, "rating_trend_over_time.png")
  ggsave(output_path, trend_plot)
  cat("IMDb Rating trend plot saved to:", output_path, "\n")
}


# --- Full Workflow ---

# 19. Perform all tests and visualizations
perform_test_and_analysis <- function(dataset_path, output_dir) {
  # Import and clean the dataset
  dataset <- import_dataset(dataset_path)
  cleaned <- clean_dataset(dataset)
  save_cleaned_dataset(cleaned, output_dir)
  
  # Generate data summaries
  generate_data_summary(cleaned, output_dir)
  generate_descriptive_statistics(cleaned, output_dir)
  generate_data_structure(cleaned, output_dir)
  
  # Generate correlation analysis
  generate_correlation_matrix(cleaned, output_dir)
  visualize_correlation_matrix(cleaned, output_dir)
  
  # Visualizations
  visualize_histograms(cleaned, output_dir)
  visualize_boxplots(cleaned, output_dir)
  visualize_scatterplots(cleaned, output_dir)
  visualize_bar_charts(cleaned, output_dir)
  
  # Statistical tests and outlier detection
  generate_t_test(cleaned, output_dir)
  generate_fisher_test(cleaned, output_dir)
  generate_chi_square_test(cleaned, output_dir)
  generate_identified_outliers(cleaned, output_dir)
  
  # Generate trend plot
  generate_trend_plot(cleaned, output_dir)
  
  cat("All analyses and visualizations completed successfully. Results saved to:", output_dir, "\n")
}


# Execute workflow
perform_test_and_analysis("data/movie_data.csv", "data/output/final")

