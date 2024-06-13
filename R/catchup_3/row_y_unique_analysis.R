# Load the necessary library
library(dplyr)

# Load the CSV file and specify that all columns should be integers
df <- read.csv("./data/simulation_catgories_n_cluster_c3_1.csv", colClasses = "integer")

df_y <- df%>% select(-cluster)
  
## way 2

# Sort each row and convert to a data frame where each row is sorted
sorted_df <- t(apply(df_y, 1, sort))

# Create a unique identifier for each sorted row (optional, for easier unique counting)
df$identifier <- apply(sorted_df, 1, paste, collapse = ",")

# Count unique rows based on the identifier
unique_rows <- df %>%
  group_by(identifier) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Sorted_Values = list(strsplit(identifier, ",")))

# Print the list of unique rows and their counts
print(unique_rows)


## way 1

# Count unique rows
unique_rows_count <- nrow(distinct(df))

# List each unique row and their counts
unique_rows <- df %>%
  group_by(across(everything())) %>%
  summarise(Count = n(), .groups = 'drop')

# Print the count of unique rows
print(unique_rows_count)

# Print the list of unique rows and their counts
print(unique_rows)
