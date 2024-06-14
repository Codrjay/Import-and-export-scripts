# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)

# Load the data
trade_data <- read_xlsx("C:/Users/stu/Desktop/Project/Import and export patterns/Clean african data.xlsx")

# Ensure that the trade_data contains the columns 'Import value (% of GDP)' and 'Export value (% of GDP)'

# Perform hierarchical clustering on the mean import and export values
merged_data <- trade_data %>%
  group_by(`Country Code`, `Country Name`) %>%
  summarise(
    Mean_Import_Value = mean(`Import value (% of GDP)`, na.rm = TRUE),
    Mean_Export_Value = mean(`Export value (% of GDP)`, na.rm = TRUE)
  )

# Perform hierarchical clustering based on Mean Import Values
dist_mat_import <- dist(merged_data$Mean_Import_Value)
hierarchical_clusters_import <- hclust(dist_mat_import, method = "ward.D")

# Plot the dendrogram for imports
plot(hierarchical_clusters_import, labels = merged_data$`Country Name`, main = "Dendrogram of Countries based on Mean Import Values")

# Perform hierarchical clustering based on Mean Export Values
dist_mat_export <- dist(merged_data$Mean_Export_Value)
hierarchical_clusters_export <- hclust(dist_mat_export, method = "ward.D")

# Plot the dendrogram for exports
plot(hierarchical_clusters_export, labels = merged_data$`Country Name`, main = "Dendrogram of Countries based on Mean Export Values")

# Cut the tree to form clusters for both imports and exports
num_clusters <- 4
clusters_import <- cutree(hierarchical_clusters_import, num_clusters)
clusters_export <- cutree(hierarchical_clusters_export, num_clusters)

# Assign clusters to the merged_data
merged_data$cluster_import <- factor(clusters_import)
merged_data$cluster_export <- factor(clusters_export)

# Order clusters based on the mean import and export values
merged_data <- merged_data %>%
  mutate(cluster_import = factor(cluster_import, levels = order(tapply(Mean_Import_Value, cluster_import, mean))),
         cluster_export = factor(cluster_export, levels = order(tapply(Mean_Export_Value, cluster_export, mean))))

# Add the cluster assignments back to the original trade_data
trade_data <- trade_data %>%
  left_join(merged_data %>% select(`Country Code`, cluster_import, cluster_export), by = "Country Code")

# Generate cluster plots for imports and exports
ggplot(merged_data, aes(x = Mean_Import_Value, y = Mean_Export_Value, color = cluster_import)) +
  geom_point() +
  labs(title = "Cluster Plot of Mean Import Values",
       x = "Mean Import Value",
       y = "Mean Export Value",
       color = "Cluster (Import)") +
  theme_minimal()

ggplot(merged_data, aes(x = Mean_Import_Value, y = Mean_Export_Value, color = cluster_export)) +
  geom_point() +
  labs(title = "Cluster Plot of Mean Export Values",
       x = "Mean Import Value",
       y = "Mean Export Value",
       color = "Cluster (Export)") +
  theme_minimal()

# Find the countries with the highest and lowest import and export values in each cluster
cluster_summary_import <- trade_data %>%
  group_by(cluster_import) %>%
  summarise(
    Import_Max_Value = max(`Import value (% of GDP)`, na.rm = TRUE),
    Import_Max_Country = `Country Name`[which.max(`Import value (% of GDP)`)],
    Import_Min_Value = min(`Import value (% of GDP)`, na.rm = TRUE),
    Import_Min_Country = `Country Name`[which.min(`Import value (% of GDP)`)]
  )

cluster_summary_export <- trade_data %>%
  group_by(cluster_export) %>%
  summarise(
    Export_Max_Value = max(`Export value (% of GDP)`, na.rm = TRUE),
    Export_Max_Country = `Country Name`[which.max(`Export value (% of GDP)`)],
    Export_Min_Value = min(`Export value (% of GDP)`, na.rm = TRUE),
    Export_Min_Country = `Country Name`[which.min(`Export value (% of GDP)`)]
  )

# Merge the summaries
cluster_summary <- cluster_summary_import %>%
  rename(cluster = cluster_import) %>%
  left_join(cluster_summary_export %>% rename(cluster = cluster_export), by = "cluster", suffix = c("_Import", "_Export"))

# Print the summary table
print(cluster_summary)

# View the summary table in the Viewer pane (optional)
View(cluster_summary)

# Save the summary to an Excel file
library(writexl)
write_xlsx(cluster_summary, "cluster_summary.xlsx")
