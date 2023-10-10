# Load necessary packages
library(cluster)
library(readr)

# URL of the raw CSV file on GitHub
github_csv_url <- "https://raw.githubusercontent.com/SravaniRaoSava/Machine-Learning/main/path/to/cleaned_data.csv"

# Read the CSV file from the GitHub URL
data <- read.csv(url(github_csv_url))
data <- subset(data, select = -c(id, name, uri))

# Select the features for clustering
# Replace 'data' with your dataset and select the appropriate columns
cluster_data <- data[, c('acousticness', 'danceability', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence')]

# Perform Hierarchical Clustering
dist_matrix <- dist(cluster_data)  # Compute the distance matrix
hclust_result <- hclust(dist_matrix, method = "complete")  # Complete linkage method

# Plot the Dendrogram
plot(hclust_result, hang = -1, cex = 0.6, main = "Hierarchical Clustering Dendrogram")
