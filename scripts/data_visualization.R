# Load necessary libraries for visualization
library(ggplot2)   # For plotting

# Create Output Directory for Plots if not already created
if (!dir.exists(file.path(output_dir, "visualizations"))) {
  dir.create(file.path(output_dir, "visualizations"), recursive = TRUE)
}

# Visualize Histograms for Continuous Variables
# - IMDb Rating Histogram
imdb_rating_histogram <- ggplot(movie_data, aes(x = imdb_rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of IMDb Ratings", x = "IMDb Rating", y = "Frequency")

# - Duration Histogram
duration_histogram <- ggplot(movie_data, aes(x = duration)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Movie Duration", x = "Duration (minutes)", y = "Frequency")

# Save the histograms as images
ggsave(file.path(output_dir, "visualizations", "imdb_rating_histogram.png"), imdb_rating_histogram)
ggsave(file.path(output_dir, "visualizations", "duration_histogram.png"), duration_histogram)

# Visualize Boxplots to Identify Outliers
# - IMDb Rating Boxplot
imdb_rating_boxplot <- ggplot(movie_data, aes(y = imdb_rating)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Boxplot of IMDb Ratings", y = "IMDb Rating")

# - Duration Boxplot
duration_boxplot <- ggplot(movie_data, aes(y = duration)) +
  geom_boxplot(fill = "lightcoral", color = "darkred") +
  theme_minimal() +
  labs(title = "Boxplot of Movie Duration", y = "Duration (minutes)")

# Save the boxplots as images
ggsave(file.path(output_dir, "visualizations", "imdb_rating_boxplot.png"), imdb_rating_boxplot)
ggsave(file.path(output_dir, "visualizations", "duration_boxplot.png"), duration_boxplot)

# Visualize Scatter Plots for Relationships Between Variables
# - IMDb Rating vs. Duration
rating_duration_scatter <- ggplot(movie_data, aes(x = imdb_rating, y = duration)) +
  geom_point(color = "purple", alpha = 0.5) +
  theme_minimal() +
  labs(title = "IMDb Rating vs. Movie Duration", x = "IMDb Rating", y = "Duration (minutes)")

# - IMDb Rating vs. Number of Voted Users
rating_voted_users_scatter <- ggplot(movie_data, aes(x = imdb_rating, y = num_voted_users)) +
  geom_point(color = "green", alpha = 0.5) +
  theme_minimal() +
  labs(title = "IMDb Rating vs. Number of Voted Users", x = "IMDb Rating", y = "Number of Voted Users")

# Save the scatter plots as images
ggsave(file.path(output_dir, "visualizations", "rating_duration_scatter.png"), rating_duration_scatter)
ggsave(file.path(output_dir, "visualizations", "rating_voted_users_scatter.png"), rating_voted_users_scatter)

# Visualize Bar Charts for Categorical Data
# - Genre Distribution (Top 10 most frequent genres)
genre_count <- movie_data %>%
  mutate(genre = str_split(genres, "\\|")) %>%
  unnest(genre) %>%
  count(genre) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(genre, n), y = n, fill = genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Most Frequent Movie Genres", x = "Genre", y = "Count")

# - Language Distribution
language_count <- movie_data %>%
  count(language) %>%
  ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Language Distribution in Movies", x = "Language", y = "Count")

# Save the bar charts as images
ggsave(file.path(output_dir, "visualizations", "genre_distribution.png"), genre_count, height = 4)
ggsave(file.path(output_dir, "visualizations", "language_distribution.png"), language_count, height = 12)

# Visualize the Correlation Matrix using ggplot2 (Heatmap)
# Calculate correlation matrix for numeric columns
correlation_matrix <- cor(numeric_cols, use = "complete.obs")

# Convert the correlation matrix to long format for ggplot2
correlation_data <- as.data.frame(as.table(correlation_matrix))
names(correlation_data) <- c("Variable1", "Variable2", "Correlation")

# Plot heatmap for correlation matrix
correlation_heatmap <- ggplot(correlation_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the correlation heatmap as an image
ggsave(file.path(output_dir, "visualizations", "correlation_heatmap.png"), correlation_heatmap, width=4, height=4)

# Print confirmation messages
print(paste("Visualizations saved to:", file.path(output_dir, "visualizations")))
