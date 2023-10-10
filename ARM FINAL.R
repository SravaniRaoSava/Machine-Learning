# Load necessary packages
#install.packages("reshape2")
library(arules)
library(dplyr)
library(reshape2)
library(arulesViz)


# URL of the raw CSV file on GitHub
github_csv_url <- "https://raw.githubusercontent.com/SravaniRaoSava/Machine-Learning/main/path/to/cleaned_data.csv"

# Read the CSV file from the GitHub URL
data <- read.csv(url(github_csv_url))

data <- subset(data, select = -c(album, id, uri, track_number))

# Select only the binary feature columns (excluding 'name' and 'popularity')
binary_features <- data[, -(1:2)]

# Create a transactions data frame with binary features
transactions <- as(binary_features, "transactions")

# Load the arules library if not already loaded
library(arules)

# Apply the Apriori algorithm
rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.7))

# Filter and inspect the rules (you can adjust thresholds as needed)
filtered_rules <- rules[quality(rules)$support > 0.1 & quality(rules)$confidence > 0.7]
inspect(filtered_rules)

#######################################################################################################################

# Plot the rules (scatterplot based on lift)
library(arulesViz)
plot(rules, method = "scatterplot", measure = "lift")

# Compute item frequencies
item_freq <- itemFrequency(transactions)

# Convert item frequencies to a data frame
item_frequency <- data.frame(item = names(item_freq), frequency = item_freq)

# Sort the data frame by frequency in descending order
item_frequency <- item_frequency[order(item_frequency$frequency, decreasing = TRUE), ]

# Visualize item frequencies using a bar chart
library(ggplot2)
ggplot(item_frequency, aes(x = item, y = frequency)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Rule Network Diagram
library(igraph)

# Extract antecedents and consequents from the rules
antecedents <- labels(lhs(rules))
consequents <- labels(rhs(rules))

# Create a data frame with antecedents and consequents
rule_edges <- data.frame(from = antecedents, to = consequents)

# Create a directed graph
rule_graph <- graph_from_data_frame(rule_edges, directed = TRUE)

# Plot the rule network diagram
plot(rule_graph, layout = layout.fruchterman.reingold)

# Sort rules by lift in descending order
sorted_rules <- sort(rules, by = "lift", decreasing = TRUE)

# Select the top 15 rules
top_15_rules <- sorted_rules[1:15]

# Display the top 15 rules
inspect(top_15_rules)

# Sort rules by support in descending order
sorted_rules <- sort(rules, by = "support", decreasing = TRUE)

# Select the top 15 rules
top_15_rules <- sorted_rules[1:15]

# Display the top 15 rules
inspect(top_15_rules)

# Sort rules by confidence in descending order
sorted_rules <- sort(rules, by = "confidence", decreasing = TRUE)

# Select the top 15 rules
top_15_rules <- sorted_rules[1:15]

# Display the top 15 rules
inspect(top_15_rules)

# Extract antecedents and consequents from the top 15 rules
antecedents <- labels(lhs(top_15_rules))
consequents <- labels(rhs(top_15_rules))

# Combine antecedents and consequents to get all unique items
all_items <- unique(c(antecedents, consequents))

# Create a data frame with item names
items_data <- data.frame(item = all_items)

# Create a data frame with edges connecting antecedents and consequents
edges_data <- data.frame(from = antecedents, to = consequents)

# Create a graph from the item and edges data
rule_graph <- graph_from_data_frame(edges_data, vertices = items_data, directed = TRUE)

# Plot the graph
plot(rule_graph, layout = layout.fruchterman.reingold)
