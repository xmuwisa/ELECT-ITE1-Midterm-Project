# Load necessary libraries
library(tidyverse)  # For data manipulation
library(car)        # For statistical tests

# T-test: Comparing IMDb Ratings Between High and Low-Rated Movies
movie_data <- movie_data %>%
  mutate(rating_group = ifelse(imdb_rating > 7, "High Rating", "Low Rating"))

t_test_result <- t.test(imdb_rating ~ rating_group, data = movie_data)
print("T-test result comparing High and Low-Rated Movies:")
print(t_test_result)

# Fisher's Exact Test with simulation for Independence between Genre and Language ---
genre_language_table <- movie_data %>%
  mutate(genre = str_split(genres, "\\|")) %>%
  unnest(genre) %>%
  count(genre, language) %>%
  pivot_wider(names_from = language, values_from = n, values_fill = list(n = 0)) %>%
  select(-genre) %>%
  as.data.frame()

# Perform Fisher's Exact Test with simulation for large datasets
fisher_test_result <- fisher.test(genre_language_table, simulate.p.value = TRUE, B = 10000)
print("Fisher's Exact Test result for Independence between Genre and Language (simulation):")
print(fisher_test_result)

# Exploratory Data Analysis 
numeric_cols <- movie_data %>% select(where(is.numeric))
correlation_matrix <- cor(numeric_cols, use = "complete.obs")

rating_trend_plot <- ggplot(movie_data, aes(x = release_year, y = imdb_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(title = "IMDb Rating Trend Over Time", x = "Release Year", y = "IMDb Rating")

ggsave("data/output/visualizations/rating_trend_over_time.png", rating_trend_plot)

outliers <- movie_data %>%
  filter(imdb_rating > 9 | imdb_rating < 2)

print("Potential Anomalies (Outliers) in IMDb Ratings:")
print(outliers)

# Save the results of the T-test and Fisher's Exact Test to a text file
results_path <- "data/output/analysis_summary.txt"
statistical_results <- c(
  "── Data Analysis ────────────────────────",
  "\nT-test for IMDb Ratings between High and Low-Rated Movies:\n", capture.output(print(t_test_result)),
  "\nFisher's Exact Test for Independence between Genre and Language (simulation):\n", capture.output(print(fisher_test_result)),
  "\nPotential Anomalies in IMDb Ratings (Outliers):\n", capture.output(print(outliers))
)

writeLines(statistical_results, results_path)
print(paste("Statistical Analysis results saved to:", results_path))
