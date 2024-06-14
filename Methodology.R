# Data Loading and Exploration

library(readxl)
trade_data <- read_xlsx("C:/Users/stu/Desktop/Project/Import and export patterns/Clean african data.xlsx")
View(trade_data)

library(ggplot2)

# Density plot for import values
ggplot(trade_data, aes(x = `Import value (% of GDP)`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Import Values",
       x = "Import Value (% of GDP)",
       y = "Density")

# Density plot for export values
ggplot(trade_data, aes(x = `Export value (% of GDP)`)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of Export Values",
       x = "Export Value (% of GDP)",
       y = "Density")

shapiro_import <- shapiro.test(trade_data$`Import value (% of GDP)`)
shapiro_export <- shapiro.test(trade_data$`Export value (% of GDP)`)

print(shapiro_import)
print(shapiro_export)

friedman_import <- friedman.test(`Import value (% of GDP)` ~ Year | `Country Name`, data = trade_data)
friedman_export <- friedman.test(`Export value (% of GDP)` ~ Year | `Country Name`, data = trade_data)

print(friedman_import)
print(friedman_export)

library(dplyr)
library(tidyr)
library(cluster)

# Pivot the data from long to wide format
trade_data_wide <- trade_data %>%
  pivot_wider(
    id_cols = c("Country Name", "Country Code", "Continent", "Region"),
    names_from = "Year",
    values_from = c("Import value (% of GDP)", "Export value (% of GDP)"),
    names_sep = "_"
  ) %>%
  filter(complete.cases(.))  # Remove rows with missing values

# Perform hierarchical clustering
dist_mat <- dist(trade_data_wide[, -c(1:4)])  # Exclude non-numeric columns
hc <- hclust(dist_mat)
clusters <- cutree(hc, k = 4)
cluster_summary <- table(clusters)
cluster_summary_df <- as.data.frame(cluster_summary)
colnames(cluster_summary_df) <- c("Cluster", "No..of.countries.territories")
View(cluster_summary_df)

# K-Means Clustering
merged_data <- trade_data %>%
  group_by(`Country Code`) %>%
  summarise(
    Mean_Import_Value = mean(`Import value (% of GDP)`),
    Mean_Export_Value = mean(`Export value (% of GDP)`)
  )

k <- 4
kmeans_clusters <- kmeans(merged_data[, c("Mean_Import_Value", "Mean_Export_Value")], centers = k)
cluster_summary <- table(kmeans_clusters$cluster)
cluster_country_summary <- data.frame(Cluster = kmeans_clusters$cluster, `Country Code` = merged_data$`Country Code`)
View(cluster_summary)

print(cluster_summary)
print(cluster_country_summary)

# Hierarchical Clustering
hierarchical_clusters <- hclust(dist(merged_data[, c("Mean_Import_Value", "Mean_Export_Value")]), method = "ward.D")
num_clusters <- 4
clusters <- cutree(hierarchical_clusters, num_clusters)
cluster_summary <- table(clusters)
cluster_country_summary <- data.frame(Cluster = clusters, `Country Code` = merged_data$`Country Code`)
print(cluster_summary)
print(cluster_country_summary)

trade_data$cluster <- factor(clusters)

# Plot clusters
ggplot(trade_data, aes(x = `Import value (% of GDP)`, y = `Export value (% of GDP)`, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Plot of Import and Export Values",
       x = "Import Value (% of GDP)",
       y = "Export Value (% of GDP)",
       color = "Cluster")


# Load necessary library
library(ggplot2)

# Load necessary library
library(ggplot2)

# Perform hierarchical clustering on the merged_data
hierarchical_clusters <- hclust(dist(merged_data[, c("Mean_Import_Value", "Mean_Export_Value")]), method = "ward.D")
num_clusters <- 4
clusters <- cutree(hierarchical_clusters, num_clusters)

# Add the cluster assignments to the merged_data
merged_data$cluster <- factor(clusters)

# Create the cluster plot
ggplot(merged_data, aes(x = Mean_Import_Value, y = Mean_Export_Value, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Plot of Mean Import and Export Values",
       x = "Mean Import Value",
       y = "Mean Export Value",
       color = "Cluster") +
  theme_minimal()

cluster_summary <- merged_data %>%
  group_by(cluster) %>%
  summarise(
    Import_Max = max(Mean_Import_Value),
    Import_Min = min(Mean_Import_Value),
    Export_Max = max(Mean_Export_Value),
    Export_Min = min(Mean_Export_Value)
  )

