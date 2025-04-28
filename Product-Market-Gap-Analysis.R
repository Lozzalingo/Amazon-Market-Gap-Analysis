# Load the configuration file.
source("config.R")

install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("caret")
install.packages("purrr")
install.packages("tidyr")
install.packages("R.utils")
install.packages("DT")
install.packages("stringr")
install.packages("lubridate")
install.packages("Matrix")
install.packages("recommenderlab")
install.packages("kableExtra")
install.packages("recosystem")
install.packages("rollama")
install.packages("float")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("data.table")
install.packages("Rtsne")
install.packages("irlba")

library(httr)
library(jsonlite)
library(dplyr)
library(caret)
library(jsonlite)
library(purrr)
library(tidyr)
library(R.utils)
library(DT)
library(stringr)
library(lubridate)
library(Matrix)
library(recommenderlab)
library(kableExtra)
library(recosystem)
library(httr)
library(jsonlite)
library(rollama)
library(float)
library(ggplot2)
library(reshape2)
library(data.table)
library(Rtsne)
library(irlba)


# Define save directories
toy_and_games_reviews_path <- file.path(workingDirectory, "Data", "Toys_and_Games_5.json.gz")
toy_and_games_meta_data_path <- file.path(workingDirectory, "Data", "meta_Toys_and_Games.json.gz")

# Define download URLs
toy_and_games_reviews_url <- "https://datarepo.eng.ucsd.edu/mcauley_group/data/amazon_v2/categoryFilesSmall/Toys_and_Games_5.json.gz"
toy_and_games_meta_url <- "https://datarepo.eng.ucsd.edu/mcauley_group/data/amazon_v2/metaFiles2/meta_Toys_and_Games.json.gz"

# Download the reviews file
toy_and_games_reviews_response <- GET(toy_and_games_reviews_url, write_disk(toy_and_games_reviews_path, overwrite = TRUE))

# Download the metadata file
toy_and_games_meta_response <- GET(toy_and_games_meta_url, write_disk(toy_and_games_meta_data_path, overwrite = TRUE))

# Check if the downloads were successful
if (toy_and_games_reviews_response$status_code == 200) {
  cat("Reviews file downloaded successfully:", toy_and_games_reviews_path, "\n")
} else {
  cat("Failed to download reviews file. Status code:", toy_and_games_reviews_response$status_code, "\n")
}

if (toy_and_games_meta_response$status_code == 200) {
  cat("Metadata file downloaded successfully:", toy_and_games_meta_data_path, "\n")
} else {
  cat("Failed to download metadata file. Status code:", toy_and_games_meta_response$status_code, "\n")
}

# Unzip the files
tryCatch({
  gunzip(toy_and_games_reviews_path, overwrite = TRUE)
  cat("Unzipped reviews file successfully.\n")
}, error = function(e) {
  cat("Failed to unzip reviews file:", e$message, "\n")
})

tryCatch({
  gunzip(toy_and_games_meta_data_path, overwrite = TRUE)
  cat("Unzipped metadata file successfully.\n")
}, error = function(e) {
  cat("Failed to unzip metadata file:", e$message, "\n")
})





# Read the file line-by-line
raw_json_lines_reviews <- readLines(filepath(workingDirectory, "Data", "Toys_and_Games_5.json"))

# Parse each line and handle potential inconsistencies
json_raw_reviews <- map(raw_json_lines, function(line) {
  tryCatch({
    # Parse the JSON line
    parsed <- fromJSON(line)

    # Convert nested lists to character
    parsed_flat <- map_if(parsed, is.list, ~toString(.x))

    # Convert to a data frame
    as.data.frame(t(unlist(parsed_flat)), stringsAsFactors = FALSE)
  }, error = function(e) {
    # Print any problematic lines and return NULL
    cat("Error parsing line:", line, "\n")
    return(NULL)
  })
}) %>%
  # Remove any NULL entries from parsing errors
  discard(is.null)

# Use bind_rows to combine, which will fill missing columns with NA
json_raw_df_reviews <- bind_rows(json_raw_reviews)

# Additional flattening and cleaning
json_raw_df_flat_reviews <- json_raw_df_reviews %>%
  # Convert any remaining list columns to character
  mutate(across(where(is.list), ~ sapply(., toString)))

# Remove unnecessary 'image' columns
filtered_toys_and_games_reviews <- json_raw_df_flat_reviews %>%
  select(overall, reviewTime, reviewerID, asin, reviewerName, reviewText, summary, unixReviewTime)

# Save the flattened data frame to CSV for further processing and enhanced performance
write.csv(filtered_toys_and_games_reviews,
          "/users/laurencestephan/Programming/Product Market Gap Analysis/Data/Toys_and_Games_Review_Data_Filtered.csv",
          row.names = FALSE)


# Read the file line-by-line
raw_json_lines_meta_data <- readLines(filepath(workingDirectory, "Data", "meta_Toys_and_Games.json"))

# Parse each line and handle potential inconsistencies
json_raw_meta_data <- map(raw_json_lines_meta_data, function(line) {
  tryCatch({
    # Parse the JSON line
    parsed <- fromJSON(line)

    # Convert to a data frame, ensuring consistent structure
    as.data.frame(t(unlist(parsed)), stringsAsFactors = FALSE)
  }, error = function(e) {
    # Print the problematic lines and return NULL
    cat("Error parsing line:", line, "\n")
    return(NULL)
  })
}) %>%
  # Remove any NULL entries from parsing errors
  discard(is.null)

# Use bind_rows to combine, which will fill missing columns with NA
json_raw_df_meta_data <- bind_rows(json_raw)

# Flatten the data frame (convert nested lists to character columns)
json_raw_df_flat_meta_data <- json_raw_df_meta_data %>%
  mutate(across(where(is.list), ~ sapply(., toString)))

# Retain only the 'title', 'price', and 'asin' columns
filtered_df_meta_data <- json_raw_df_flat_meta_data %>%
  select(title, price, asin)

# Save the flattened data frame to CSV for further processing and enhanced performance
write.csv(filtered_df_meta_data, "/users/laurencestephan/Programming/Product Market Gap Analysis/Data/Toys_and_Games_Meta_Data_Filtered.csv", row.names = FALSE)




# Read in the review data
toys_and_games_reviews <- read.csv("/users/laurencestephan/Programming/Product Market Gap Analysis/Data/Toys_and_Games_Review_Data_Filtered.csv")

# Read in the meta data
toys_and_games_meta_data <- read.csv("/users/laurencestephan/Programming/Product Market Gap Analysis/Data/Toys_and_Games_Meta_Data_Filtered.csv")

# Combine the review data and metadata by asin (Amazon Standard Identification Number)
toys_and_games_combined <- toys_and_games_reviews %>%
  left_join(toys_and_games_meta_data, by = "asin", relationship = "many-to-many")

# Count rows where title is blank
blank_title_count <- toys_and_games_combined %>%
  filter(is.na(title) | title == "") %>%
  nrow()

# Count rows where asin is blank
blank_asin_count <- toys_and_games_combined %>%
  filter(is.na(asin) | asin == "") %>%
  nrow()

# Count rows where reviewerID is blank
blank_reviewerID_count <- toys_and_games_combined %>%
  filter(is.na(reviewerID) | reviewerID == "") %>%
  nrow()

# Count rows where reviewerID is blank
blank_rating_count <- toys_and_games_combined %>%
  filter(is.na(overall) | overall == "") %>%
  nrow()

# Print the counts
print(paste("Number of rows with blank title:", blank_title_count))
print(paste("Number of rows with blank asin:", blank_asin_count))
print(paste("Number of rows with blank reviewerID:", blank_reviewerID_count))
print(paste("Number of rows with blank ratings:", blank_rating_count))

sum(is.na(toys_and_games_combined$overall))  # Count of NA values
sum(is.nan(toys_and_games_combined$overall)) # Count of NaN values

# Remove rows where title is blank
toys_and_games_combined <- toys_and_games_combined %>%
  filter(!(is.na(title) | title == ""))

# Testing set will be 10% of product data.
set.seed(42, sample.kind = "Rounding")

test_index <- createDataPartition(y = toys_and_games_combined$overall, times = 1, p = 0.1, list = FALSE)

# Create the training set using rows where the indices are not in test_index.
training_set <- toys_and_games_combined[-test_index, ]

# Create the test set using rows where the indices are in test_index.
temp_testing_set <- toys_and_games_combined[test_index, ]

# Use semi_join to create a variable with the rows from the original temp dataframe and those that have matching values in both the "asin" and "reviewerId" columns from the training_set dataframe.
testing_set <- temp_testing_set %>%
  semi_join(training_set, by = "asin") %>%
  semi_join(training_set, by = "reviewerID")

# Add rows removed from final hold-out test set back into training_set set.
removed <- anti_join(temp_testing_set, testing_set)
training_set <- rbind(training_set, removed)

'training data'
glimpse(training_set)

'testing data'
glimpse(testing_set)


## 2.Exploring The Data 

# Create a dataframe of ratings directly from the dataset.
ratings_df <- data.frame(rating = training_set$overall)

# Plot a histogram of whole ratings with alternating colors
ggplot(ratings_df, aes(x = factor(rating))) +
  geom_bar(aes(fill = factor(rating)), color = "white", alpha = 0.7) +
  scale_x_discrete(name = "Rating", limits = as.character(1:5)) +
  scale_fill_manual(values = c("#fadf2d", "#ff9859")[1:5 %% 2 + 1]) +
  labs(
    x = "Rating",
    y = "Number of Ratings",
    caption = "Source Data: Amazon Review Data (2018)"
  ) +
  ggtitle("Rating by Number of Ratings (Whole Stars Only)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Split product genres using regular expression and arrange them by their count.
top_product_name <- training_set %>%
  group_by(title) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::arrange(desc(count))

# Create the top_product_name table
datatable(
  top_product_name,
  rownames = FALSE, filter = "top", options = list(pageLength = 5, scrollX = TRUE)
) %>%
  formatRound("count", digits = 0, interval = 3, mark = ",")

# Extract the top 10 products based on the number of ratings
top_product <- training_set %>%
  group_by(title) %>%
  summarize(count = n(), .groups = "drop") %>%
  top_n(10, count) %>%
  arrange(desc(count))

# Create a bar plot using ggplot2
top_product %>%
  ggplot(aes(x = reorder(title, count), y = count)) +
  geom_bar(stat = "identity", fill = "#ff9859") +
  geom_text(aes(label = str_sub(title, start = 1, end = 60)), 
            hjust = 1, size = 2.1, color = "black") +
  coord_flip() +
  labs(
    x = "", 
    y = "Number of Ratings", 
    title = "Top 10 Products on Number of Ratings", 
    caption = "Source Data: Amazon Review Data (2018)"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


# Convert unixReviewTime and extract year_month
training_set_converted_date <- training_set %>%
  mutate(
    review_date = as.Date(as.POSIXct(unixReviewTime, origin = "1970-01-01")),
    year_month = format(review_date, "%Y-%m")
  )

# Compute grouped data (if not already done)
grouped_data <- training_set_converted_date %>%
  group_by(year_month) %>%
  summarise(monthly_count = n(), .groups = "drop")

# Add monthly counts to the original data
training_set_converted_date <- training_set_converted_date %>%
  left_join(grouped_data, by = "year_month")

training_set_converted_date %>%
  group_by(year_month) %>%
  summarise(
    N = n(),
    avg = mean(overall, na.rm = TRUE),
    SE = sd(overall, na.rm = TRUE) / sqrt(N)
  ) %>%
  filter(N >= 5) %>%  # Adjust threshold as needed
  slice_max(avg, n = 30) %>%  # Select the top 20 entries by avg
  mutate(year_month = reorder(year_month, avg)) %>%
  ggplot(aes(x = reorder(year_month, avg), y = avg, ymin = avg - 2 * SE, ymax = avg + 2 * SE)) +
  geom_point() +
  geom_errorbar(color = "grey", linewidth = 0.7) +
  theme_minimal() +
  labs(
    title = "Top 20 Error Bar Plots by Genres",
    caption = "Source Data: Amazon Review Data (2018)",
    x = "Titles",
    y = "Avg"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


# Generate a histogram depicting the distribution of ratings by product.
training_set %>%
  dplyr::count(asin) %>%
  filter(n < 10000) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "#ffa600", fill = "#fab937", alpha = 0.7) +
  ggtitle("Distribution of Ratings by Product") +
  labs(
    subtitle = "Number of Ratings by Product",
    x = "Number of Ratings < 10000",
    y = "Frequency",
    caption = "Source Data: Amazon Review Data (2018)"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA))


# Generate a histogram depicting the distribution of ratings by product.
training_set %>%
  dplyr::count(asin) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "#ffa600", fill = "#fab937", alpha = 0.7) +
  scale_x_log10() +
  ggtitle("Distribution of Ratings by product") +
  labs(
    subtitle = "Number of Ratings by Product",
    x = "Number of Ratings by Product",
    y = "Frequency",
    caption = "Source Data: Amazon Review Data (2018)"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA))


# Generate a histogram depicting the distribution of ratings by reviewer.
training_set %>%
  dplyr::count(reviewerID) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30,color = "#ffa600", fill = "#fab937", alpha = 0.7) +
  ggtitle("Distribution of Ratings by Reviewer") +
  labs(
    subtitle = "Number of Ratings by Reviewer ID",
    x = "Number of Ratings",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA))

# Generate a histogram depicting the distribution of ratings by reviewer.
training_set %>%
  dplyr::count(reviewerID) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "#ffa600", fill = "#fab937", alpha = 0.7) +
  scale_x_log10() +
  ggtitle("Distribution of Ratings by reviewer") +
  labs(
    subtitle = "Number of Ratings by reviewerId",
    x = "Number of Ratings (log scale)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA))


# Analyse average ratings over time by extracting weekly timestamps and calculating the mean rating for each week.
training_set_converted_date %>%
  mutate(date = round_date(as_datetime(review_date), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(overall, na.rm = TRUE)) %>%
  ggplot(aes(date, rating)) +
  geom_point(color = "#fa4e2f", size = 0.5) +
  geom_smooth(formula = y ~ x, method = "loess", se = TRUE, color = "#fcba03", linetype = "solid") +
  ggtitle("Average Ratings Over Time") +
  labs(
    subtitle = "Timestamp, Time Unit: Week",
    x = "Date",
    y = "Average Ratings",
    caption = "Source Data: Amazon Review Data (2018)"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#ffecbf", fill = NA))

## Matrix Transformation 
# Calculate the number of distinct reviewers and products in the training_set dataset.
training_set %>%
  dplyr::summarise(
    n_reviewers = n_distinct(reviewerID),
    n_products = n_distinct(asin)
  )

# Create a copy of the training_set data frame for manipulation without altering the original data.
training_set.copy <- training_set

# Convert the reviewerID and asin columns to factors for categorical representation.
training_set.copy$reviewerID <- as.factor(training_set.copy$reviewerID)
training_set.copy$asin <- as.factor(training_set.copy$asin)

# Convert reviewerID and asin back to numeric representation.
training_set.copy$reviewerID <- as.numeric(training_set.copy$reviewerID)
training_set.copy$asin <- as.numeric(training_set.copy$asin)

# Create a sparse matrix representation of ratings.
sparse_ratings_amazon <- sparseMatrix(
  i = training_set.copy$reviewerID,
  j = training_set.copy$asin,
  x = training_set.copy$overall,
  dims = c(
    length(unique(training_set.copy$reviewerID)),
    length(unique(training_set.copy$asin))
  ),
)

# Remove the copied data frame to free up memory.
rm(training_set.copy)

# Define constants.
num_reviewers <- 50
num_products <- 50
num_display_reviewers <- 10

# Suppress row and column names in sparse matrix display.
options(Matrix.print.rownames = FALSE)
options(Matrix.print.colnames = FALSE)

# Display a subset of the sparse ratings.
sparse_ratings_amazon[1:num_display_reviewers, 1:num_display_reviewers]


# Convert rating matrix into a recommenderlab sparse matrix.
ratingMat <- new("realRatingMatrix", data = sparse_ratings_amazon)
ratingMat

# Compute reviewer similarity using cosine similarity for the first 1000 reviewers.
similarity_reviewers <- similarity(ratingMat[1:200,], 
                               method = "cosine", 
                               which = "users")

# Visualise reviewer similarity using an image plot.
image(as.matrix(similarity_reviewers), main = "Reviewer Similarity")

# Compute product similarity using cosine similarity for the first 50 products.
similarity_products <- similarity(ratingMat[,1:50], 
                                method = "cosine", 
                                which = "items")

# Visualise product similarity using an image plot.
image(as.matrix(similarity_products), main = "Product Similarity")

# Calculate sparsity
sparsity <- sum(ratingMat@data == 0) / prod(dim(ratingMat))

# Save the sparsity value to a file
save(sparsity, file = "sparsity_value.RData")


# Load the saved sparsity value
load("sparsity_value.RData")

# Print sparsity to confirm it loaded correctly
print(paste("Sparsity of the matrix:", round(sparsity * 100, 2), "%"))


## 2. Dimension Reduction
# Set seed for reproducibility.
set.seed(42)

# Perform incremental randomised SVD on the sparse_ratings matrix.
suppressMessages({
  svd_result <- irlba(sparse_ratings_amazon, tol=1e-4, verbose=TRUE, nv = 100, maxit = 1000)
})

# Plot singular values for the reviewer-product Matrix.
plot(svd_result$d, pch=20, col = "blue", cex = 1, xlab='Singular Value', ylab='Magnitude', 
     main = "Singular Values for Reviewer-Product Matrix")

# Calculate the percentage of total sum of squares for the first 25, 50, 75 and 90 singular values.
all_sing_sq <- sum(svd_result$d^2)
all_sing_sq

first_25 <- sum(svd_result$d[1:25]^2)
print(first_6/all_sing_sq)

first_50 <- sum(svd_result$d[1:50]^2)
print(first_12/all_sing_sq)

first_75 <- sum(svd_result$d[1:75]^2)
print(first_20/all_sing_sq)

first_90 <- sum(svd_result$d[1:90]^2)
print(first_100/all_sing_sq)

# Calculate the cumulative percentage of total sum of squares for each singular value.
perc_vec <- NULL
for (i in 1:length(svd_result$d)) {
  perc_vec[i] <- sum(svd_result$d[1:i]^2) / all_sing_sq
}

# Plot the cumulative percentage against singular values and a horizontal line at 90%.
plot(perc_vec, pch=20, col = "blue", cex = 1, xlab='Singular Values %', 
     ylab='% of Sum of Squares of Singular Values', main = "Choosing k for Dimensionality Reduction")
lines(x = c(0,100), y = c(.90, .90))

# Determine the optimal value for k:
# To find k, calculate the length of the vector derived from the cumulative sum of squares.
# The chosen k corresponds to the number of singular values needed to capture 90% of the total sum of squares,
# excluding any values that exceed the 0.90 threshold.

#Find the optimal k value.
k = length(perc_vec[perc_vec <= .90])
cat("Optimal k Value:", k, "\n")

# Decompose Y into matrices U, D, and V.
U_k <- svd_result$u[, 1:k]
D_k <- Diagonal(x = svd_result$d[1:k])
V_k <- t(svd_result$v)[1:k, ]

# Display dimensions.
cat("Dimensions of U_k:", dim(U_k), "\n")
cat("Dimensions of D_k:", dim(D_k), "\n")
cat("Dimensions of V_k:", dim(V_k), "\n")


## Relevant Data
# Determine the minimum number of products and reviewers.
min_n_products <- round(quantile(rowCounts(ratingMat), 0.90))
min_n_reviewers <- round(quantile(colCounts(ratingMat), 0.75))

cat("Minimum number of products (90th percentile):", min_n_products, "\n")
cat("Minimum number of reviewers (90th percentile):", min_n_reviewers, "\n")

# Extract ratings for products and reviewers meeting the criteria.
ratings_products <- ratingMat[
  rowCounts(ratingMat) > min_n_products,
  colCounts(ratingMat) > min_n_reviewers
]

# Display the resulting ratings matrix.
ratings_products

# IV. Models and Results
#Define the RMSE function.
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

## 1. Linear Regression
# Define the RMSE function.
calculate_RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Calculate the global average rating.
mu <- mean(training_set$overall)
cat("Mu:", mu, "\n")

# Calculate training time and predict ratings.
training_time_mu <- system.time({
  predicted_ratings <- testing_set %>%
    mutate(pred = mu) %>%
    pull(pred)
})

# Calculate RMSE.
rmse_mu <- calculate_RMSE(testing_set$overall, predicted_ratings)

# Calculate model size.
model_size_mu <- round(
  sum(
    object.size(mu),
    object.size(predicted_ratings)
  ) / (1024^2),  # Convert to MB
  4
)

# Save the results of Mu model.
saveRDS(list(rmse = rmse_mu, time = training_time_mu["elapsed"], size = model_size_mu), file = "mu_model_amazon.rds")

# Load model results.
mu_model <- readRDS(file.path(workingDirectory, "mu_model_amazon.rds"))

# Print results.
cat("RMSE for Mu:", mu_model$rmse, "\n")
cat("Training Time:", round(mu_model$time["elapsed"], 4), "sec\n")
cat("Model Size:", mu_model$size, "MB")

# product effect.
product_avgs <- training_set %>%
  group_by(asin) %>%
  dplyr::summarize(b_i = mean(overall - mu))

# Calculate training time and predict ratings.
training_time_product <- system.time({
predicted_ratings_bi <- testing_set %>%
  left_join(product_avgs, by = "asin") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
})

# Calculate RMSE for product effect.
rmse_model_product <- calculate_RMSE(testing_set$overall, predicted_ratings_bi)

# Calculate model size.
model_size_product_effect <- round(
  sum(
    object.size(product_avgs),
    object.size(predicted_ratings_bi)
  ) / (1024^2),  # Convert to MB.
  4
)

# Save the results of product effect model.
saveRDS(list(rmse = rmse_model_product, time = training_time_product["elapsed"], size = model_size_product_effect), file = "product_effect_model.rds")

# Load model results.
product_effect_model <- readRDS(file.path(workingDirectory, "product_effect_model.rds"))

# Print results.
cat("RMSE for product Effect:", product_effect_model$rmse, "\n")
cat("Training Time:", round(product_effect_model$time["elapsed"], 4), "sec\n")
cat("Model Size:", product_effect_model$size, "MB")

# product + reviewer effect.
reviewer_avgs <- training_set %>%
  left_join(product_avgs, by = "asin") %>%
  group_by(reviewerID) %>%
  dplyr::summarize(b_u = mean(overall - mu - b_i))

# Calculate training time and predict ratings.
training_time_product_reviewer <- system.time({
predicted_ratings_bu <- testing_set %>%
  left_join(product_avgs, by = "asin") %>%
  left_join(reviewer_avgs, by = "reviewerID") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
})

# Calculate RMSE for reviewer effect.
rmse_model_product_reviewer <- calculate_RMSE(testing_set$overall, predicted_ratings_bu)

# Calculate model size.
model_size_product_reviewer <- round(
  sum(
    object.size(product_avgs),
    object.size(reviewer_avgs),
    object.size(predicted_ratings_bu)
  ) / (1024^2),  # Convert to MB.
  4
)

# Save the results of product + reviewer effect model.
saveRDS(list(rmse = rmse_model_product_reviewer, time = training_time_product_reviewer["elapsed"], size = model_size_product_reviewer), file = "product_reviewer_effect_model.rds")

# Load model results.
product_reviewer_effect_model <- readRDS(file.path(workingDirectory, "product_reviewer_effect_model.rds"))

# Print results.
cat("RMSE for product + reviewer Effect:", product_reviewer_effect_model$rmse, "\n")
cat("Training Time:", round(product_reviewer_effect_model$time["elapsed"], 4), "sec\n")
cat("Model Size:", product_reviewer_effect_model$size, "MB")

# Regularisation.
# Set up lambda values for cross-validation.
lambdas <- seq(0, 10, 0.25)

# Initialise vectors to store RMSE and model sizes
rmses_productID <- numeric(length(lambdas))
model_size_product_reviewer_reg <- numeric(length(lambdas))

# Function to calculate RMSE with regularisation.
calculate_RMSE_reg <- function(training_set, testing_set, lambda) {
  
  mu_reg <- mean(training_set$overall)
  
  b_i_reg <- training_set %>%
    group_by(asin) %>%
    dplyr::summarize(b_i_reg = sum(overall - mu_reg) / (n() + lambda))
  
  b_u_reg <- training_set %>%
    left_join(b_i_reg, by = "asin") %>%
    group_by(reviewerID) %>%
    dplyr::summarize(b_u_reg = sum(overall - b_i_reg - mu_reg) / (n() + lambda))
  
  predicted_ratings_b_i_u <- testing_set %>%
    left_join(b_i_reg, by = "asin") %>%
    left_join(b_u_reg, by = "reviewerID") %>%
    mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
    pull(pred)
  
  model_size_product_reviewer_reg <<- round(  # Using <<- to assign it globally outside of the function. 
    sum(
      object.size(mu_reg),
      object.size(b_i_reg),
      object.size(b_u_reg),
      object.size(predicted_ratings_b_i_u)
    ) / (1024^2),  # Convert to MB.
    4
  )
  
  return(list(RMSE = RMSE(testing_set$overall, predicted_ratings_b_i_u), Model_Size = model_size_product_reviewer_reg))
}

# Calculate training time and RMSE for different lambdas using sapply.
training_time_product_reviewer_reg <- system.time({
  for (i in seq_along(lambdas)) {
    result <- calculate_RMSE_reg(training_set, testing_set, lambdas[i])
    rmses_productID[i] <- result$RMSE
    model_size_product_reviewer_reg[i] <- result$Model_Size
  }
})

# Plot RMSE values for different lambdas.
ggplot(data = data.frame(lambdas, rmses_productID), aes(x = lambdas, y = rmses_productID)) +
  geom_point(color = "blue") +
  geom_line(color = "red") +
  labs(
    title = "RMSE vs. Lambda",
    x = "Lambda",
    y = "RMSE"
  ) +
  theme_minimal()

# Initialise vectors to store RMSE and model sizes
rmses_reviewerID <- numeric(length(lambdas))
model_size_reviewer_product_reg <- numeric(length(lambdas))

# Function to calculate RMSE with regularisation.
calculate_RMSE_reg <- function(training_set, final_holdout_year, lambda) {
    
  mu_reg <- mean(training_set$overall)

    b_i_reg <- training_set %>%
      group_by(reviewerID) %>%
      dplyr::summarize(b_i_reg = sum(overall - mu_reg) / (n() + lambda))

    b_u_reg <- training_set %>%
      left_join(b_i_reg, by = "reviewerID") %>%
      group_by(asin) %>%
      dplyr::summarize(b_u_reg = sum(overall - b_i_reg - mu_reg) / (n() + lambda))

    predicted_ratings_b_i_u <- testing_set %>%
      left_join(b_i_reg, by = "reviewerID") %>%
      left_join(b_u_reg, by = "asin") %>%
      mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
      pull(pred)

   model_size_reviewer_product_reg <<- round(  # Using <<- to assign it globally outside of the function. 
    sum(
      object.size(mu_reg),
      object.size(b_i_reg),
      object.size(b_u_reg),
      object.size(predicted_ratings_b_i_u)
    ) / (1024^2),  # Convert to MB
    4
  )
  
  return(list(RMSE = RMSE(testing_set$overall, predicted_ratings_b_i_u), Model_Size = model_size_reviewer_product_reg))
}

# Calculate training time and RMSE for different lambdas using sapply.
training_time_reviewer_product_reg <- system.time({
  for (i in seq_along(lambdas)) {
    result <- calculate_RMSE_reg(training_set, testing_set, lambdas[i])
    rmses_reviewerID[i] <- result$RMSE
    model_size_reviewer_product_reg[i] <- result$Model_Size
  }
})

# Plot RMSE values for different lambdas.
ggplot(data = data.frame(lambdas, rmses_reviewerID), aes(x = lambdas, y = rmses_reviewerID)) +
  geom_point(color = "blue") +
  geom_line(color = "red") +
  labs(
    title = "RMSE vs. Lambda",
    x = "Lambda",
    y = "RMSE"
  ) +
  theme_minimal()

# Find the optimal lambda for productID.
optimal_lambda_all <- lambdas[which.min(rmses_productID)]
cat("Optimal Lambda: ", optimal_lambda_all, "\n")

# Find the optimal lambda for reviewerID.
optimal_lambda_all <- lambdas[which.min(rmses_reviewerID)]
cat("Optimal Lambda: ", optimal_lambda_all, "\n")

# Calculate RMSE for the full model with the optimal lambda.
rmse_regularised_productID <- min(rmses_productID)

# Save the results of Regularised product + reviewer Effect model
saveRDS(list(rmse = rmse_regularised_productID, time = training_time_product_reviewer_reg["elapsed"], size = model_size_product_reviewer_reg[1]), file = "regularised_product_reviewer_effect_model.rds")

# Calculate RMSE for the full model with the optimal lambda.
rmse_regularised_reviewerID <- min(rmses_reviewerID)

# Save the results of Regularised reviewer + product effect model
saveRDS(list(rmse = rmse_regularised_reviewerID, time = training_time_reviewer_product_reg["elapsed"], size = model_size_reviewer_product_reg[1]), file = "regularised_reviewer_product_effect_model.rds")

# Load model results.
regularised_product_reviewer_effect_model <- readRDS(file.path(workingDirectory, "regularised_product_reviewer_effect_model.rds"))

# Print results.
cat("RMSE for Product ID with Regularisation:", regularised_product_reviewer_effect_model$rmse, "\n")
cat("Training Time:", round(regularised_product_reviewer_effect_model$time["elapsed"], 4), "sec\n")
cat("Model size:", regularised_product_reviewer_effect_model$size, "MB\n\n")

# Load model results.
regularised_reviewer_product_effect_model <- readRDS(file.path(workingDirectory, "regularised_reviewer_product_effect_model.rds"))

# Print results.
cat("RMSE for Reviewer ID with Regularisation: ", regularised_reviewer_product_effect_model$rmse, "\n")
cat("Training Time:", round(regularised_reviewer_product_effect_model$time["elapsed"], 4), "sec\n")
cat("Model size:", regularised_reviewer_product_effect_model$size, "MB", "\n")

# Summarise the RMSE values on the validation set for the linear regression models.
rmse_results <- data.frame(
  Method = c("Mu", "Product Effect", "Product + Reviewer Effects", "Regularised Product + Reviewer Effect", "Regularised Reviewer + Product Effect"),
  
  RMSE = c(mu_model$rmse, product_effect_model$rmse, product_reviewer_effect_model$rmse, regularised_product_reviewer_effect_model$rmse, regularised_reviewer_product_effect_model$rmse),
  
  Time = c(mu_model$time, product_effect_model$time, product_reviewer_effect_model$time,  regularised_product_reviewer_effect_model$time, regularised_reviewer_product_effect_model$time),
  
  Size = c(mu_model$size, product_effect_model$size, product_reviewer_effect_model$size, regularised_product_reviewer_effect_model$size, regularised_reviewer_product_effect_model$size)
)

# Rename the columns to replace full stops with spaces.
colnames(rmse_results) <- gsub("Time", "Time (sec)", colnames(rmse_results))
colnames(rmse_results) <- gsub("Size", "Size (MB)", colnames(rmse_results))

# Display the results in an HTML table using the kable function.
kable(rmse_results, "html") %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover"),
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(1, bold = TRUE, color = "black") %>%
  column_spec(2, bold = TRUE, color = "white", background = "#ff9238") %>%
  column_spec(3, bold = TRUE, color = "black", background = "#ffce6b") %>%
  column_spec(4, bold = TRUE, color = "black", background = "#fcf683") %>%
  row_spec(0, extra_css = "text-align: left;") %>%
  add_header_above(c("Linear Regression" = 4)) 

## 2. Recommender Engines
# Set the seed for reproducibility.
set.seed(42)

# Create an evaluation scheme with a 60-40 train-test split.
evaluation_scheme <- evaluationScheme(ratings_products, method = "split", train = 0.6, given = -2)

# Create a POPULAR recommender model.
model_popular <- Recommender(getData(evaluation_scheme, "train"), 
                             method = "POPULAR")

# Make predictions on the test set.
predictions_popular <- predict(model_popular, getData(evaluation_scheme, "known"), type = "ratings")

# Calculate RMSE for the POPULAR algorithm.
rmse_popular <- calcPredictionAccuracy(predictions_popular, getData(evaluation_scheme, "unknown"))[1]

# Save the POPULAR RMSE
saveRDS(rmse_popular, file = "rmse_popular.rds")


# Load the POPULAR RMSE.
rmse_popular <- readRDS(file.path(workingDirectory, "rmse_popular.rds"))

# Print results.
rmse_popular

# Create a user-based collaborative filtering (UBCF) recommender model using Cosine similarity and 50 neighbors based on cross-validation.
set.seed(42)

user_based_collaborative_filtering_model <- Recommender(getData(evaluation_scheme, "train"),
                          method = "UBCF",
                          param = list(normalize = "center", method = "Cosine", nn = 10, shrink = 10, lambda = 0.01)

)

ubcf_prediction <- predict(user_based_collaborative_filtering_model, getData(evaluation_scheme, "known"), type = "ratings")

rmse_ubcf <- calcPredictionAccuracy(ubcf_prediction, getData(evaluation_scheme, "unknown"))[1]

# Save the UBCF RMSE
saveRDS(rmse_ubcf, file = "rmse_ubcf.rds")


# Load the UBCF RMSE
rmse_ubcf <- readRDS(file.path(workingDirectory, "rmse_ubcf.rds"))

# Print results.
rmse_ubcf

# Create an evaluation scheme with a 10-90 train-test split.
evaluation_scheme <- evaluationScheme(ratings_products, method = "split", train = 0.6, given = -2)

# Create an item-based collaborative filtering (IBCF) recommender model using Cosine similarity and 10 neighbors based on cross-validation.
item_based_collaborative_filtering_model <- Recommender(getData(evaluation_scheme, "train"),
                          method = "IBCF",
                          param = list(normalize = "center", method = "Cosine", k = 200)
)

ibcf_prediction <- predict(item_based_collaborative_filtering_model, getData(evaluation_scheme, "known"), type = "ratings")

rmse_ibcf <- calcPredictionAccuracy(ibcf_prediction, getData(evaluation_scheme, "unknown"))[1]

saveRDS(rmse_ibcf, file = "rmse_ibcf.rds")



# Load the IBCF RMSE.
rmse_ibcf <- readRDS(file.path(workingDirectory, "rmse_ibcf.rds"))

# Print results.
rmse_ibcf

# Create the data frame for RMSE results
rmse_results_reco <- data.frame(
  Method = c("Popular", "UBCF", "IBCF"),
  Size = c(rmse_popular, rmse_ubcf, rmse_ibcf)
)

# Display the results in an HTML table using the kable function
kable(rmse_results_reco, "html") %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover"),
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(1, bold = TRUE, color = "black") %>%
  column_spec(2, bold = TRUE, color = "white", background = "#ff9238") %>%
  row_spec(0, extra_css = "text-align: left;") %>%
  add_header_above(c("Recommenderlab" = 2))

## Matrix Factorisation
## Before performing Matrix Factorisation (MF) method, clear unused memory.
invisible(gc())

# Create mapping for user and item IDs
create_id_mapping <- function(data) {
  user_map <- data.frame(
    original_id = unique(as.character(data$reviewerID)),
    numeric_id = seq_along(unique(as.character(data$reviewerID)))
  )
  
  item_map <- data.frame(
    original_id = unique(as.character(data$asin)),
    numeric_id = seq_along(unique(as.character(data$asin)))
  )
  
  list(user_map = user_map, item_map = item_map)
}

# Apply mapping to dataset
map_ids <- function(data, user_map, item_map) {
  # Create mappings
  user_lookup <- setNames(user_map$numeric_id, user_map$original_id)
  item_lookup <- setNames(item_map$numeric_id, item_map$original_id)
  
  # Replace IDs
  data$reviewerID <- as.factor(user_lookup[as.character(data$reviewerID)])
  data$asin <- as.factor(item_lookup[as.character(data$asin)])
  
  data
}

training_set.copy <- training_set %>%
  select(c("reviewerID", "asin", "overall", "title"))

testing_set.copy <- testing_set %>%
  select(c("reviewerID", "asin", "overall", "title"))

# Create ID mappings
id_mappings <- create_id_mapping(rbind(training_set.copy, testing_set.copy))

# Apply ID mapping
training_set.copy <- map_ids(training_set.copy, 
                              id_mappings$user_map, 
                              id_mappings$item_map)

testing_set.copy <- map_ids(testing_set.copy, 
                             id_mappings$user_map, 
                             id_mappings$item_map)

# Prepare matrices for file writing
training_set.copy_matrix <- as.matrix(training_set.copy)
testing_set.copy_matrix <- as.matrix(testing_set.copy)

# Write to files
write.table(training_set.copy_matrix, 
            file = "trainset.txt", 
            sep = " ", 
            row.names = FALSE, 
            col.names = FALSE, 
            quote = FALSE)

write.table(testing_set.copy_matrix, 
            file = "validset.txt", 
            sep = " ", 
            row.names = FALSE, 
            col.names = FALSE, 
            quote = FALSE)


# Specify data sets from files on the hard disk using data_file().
train_set <- file.path(workingDirectory, "trainset.txt")
valid_set <- file.path(workingDirectory, "validset.txt")

# Build a Recommender object for Matrix Factorisation
recommender <- Reco()

# Optimise/tune the recommender model
opts <- recommender$tune(train_set, opts = list(
  dim = c(10, 15, 20),  # Broader dimension search
  lrate = c(0.01, 0.05, 0.1),
  nthread = 4, 
  costp_l1 = 0,
  costq_l1 = 0,
  niter = 50, 
  nfold = 5,  # Reduced folds
  verbose = FALSE
))

# Train the recommender model
recommender$train(train_set, opts = c(opts$min, nthread = 4, niter = 100, verbose = FALSE))

# Make predictions on the validation set
pred_file <- tempfile()
recommender$predict(valid_set, out_file(pred_file))

# Read actual and predicted ratings
scores_real <- read.table("validset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(pred_file)

# Remove NA predictions
valid_indices <- !is.na(scores_pred)
scores_real_filtered <- scores_real[valid_indices]
scores_pred_filtered <- scores_pred[valid_indices]

# Calculate RMSE
rmse_mf_opt <- sqrt(mean((scores_real_filtered - scores_pred_filtered)^2))
rmse_mf_opt

# Train the recommender model with verbose output for the first 30 iterations.
output <- capture.output(recommender$train(train_set, opts = c(opts$min, nthread = 4, niter = 30, verbose = TRUE)))

output <- output[-1]
output <- trimws(output)

# Extract data using regular expressions.
output_df <- do.call(rbind, strsplit(output, "\\s+"))
colnames(output_df) <- c("iter", "tr_rmse", "obj")

# Convert columns to appropriate types.
output_df <- as.data.frame(output_df, stringsAsFactors = FALSE)
output_df$iter <- as.integer(output_df$iter)
output_df$tr_rmse <- as.numeric(output_df$tr_rmse)
output_df$obj <- as.numeric(output_df$obj)

# Separate files for training output and model
saveRDS(recommender, "product_recommender_model.rds")
save(output_df, file = "trainRmse_product_MF.RData")

# Load the model.
load("trainRmse_product_MF.RData")

# Specify the iteration number for analysis.
iter.line <- 15

# Extract the training RMSE at the specified iteration.
training_rmse.line <- output_df$tr_rmse[which(output_df$iter == 15)]

# Plot the training RMSE over iterations.
suppressMessages({
  output_df %>%
    ggplot(aes(x = iter, y = tr_rmse)) +
    geom_point(size = 3, shape = 19) +
    geom_smooth(aes(x = iter, y = tr_rmse), formula = y ~ x, method = "loess") +
    geom_segment(x = 0, xend = iter.line, y = training_rmse.line, yend = training_rmse.line, color = "orange", lty = 2) +
    geom_segment(x = iter.line, xend = iter.line, y = 0, yend = training_rmse.line, color = "orange", lty = 2) +
    annotate(
      geom = "label", x = iter.line, y = 0.8350, color = 5,
      label = paste("x =", round(iter.line, 0), "\ny =", round(training_rmse.line, 4))
    ) +
    labs(
      title = "RMSE for different number of latent factors",
      caption = "Based on the output of r$train(train_set, opts = c(opts$min, nthread = 4, niter = 100), \n show just first 30 iterations)"
    ) +
    ylab("RMSE") +
    xlab("Latent factors")
})

# Specify data sets from files on the hard disk using data_file().
train_set <- file.path(workingDirectory, "trainset.txt")
valid_set <- file.path(workingDirectory, "validset.txt")

# Build a Recommender object for Matrix Factorisation
recommender <- Reco()

# Tune the recommender with the optimal number of latent factors
opts <- recommender$tune(train_set, opts = list(
  dim = 15,  # Optimal number of latent factors from the graph
  lrate = c(0.01, 0.05, 0.1),
  nthread = 4, 
  costp_l1 = 0,
  costq_l1 = 0,
  niter = 100,  # Increase the number of iterations
  nfold = 5,
  verbose = FALSE
))
  
# Train the recommender model with the optimal configuration
recommender$train(train_set, opts = c(opts$min, nthread = 4, niter = 100, verbose = FALSE))

# Make predictions on the validation set
pred_file <- tempfile()
recommender$predict(valid_set, out_file(pred_file))

# Read actual and predicted ratings
scores_real <- read.table("validset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(pred_file)

# Remove NA predictions
valid_indices <- !is.na(scores_pred)
scores_real_filtered <- scores_real[valid_indices]
scores_pred_filtered <- scores_pred[valid_indices]

# Calculate RMSE
rmse_mf_opt <- sqrt(mean((scores_real_filtered - scores_pred_filtered)^2))
print(paste("Final RMSE:", rmse_mf_opt))

# Save the trained recommender model
save(recommender, file = "optimised_product_recommender_model.RData")
# Save the trained recommender model to a file
saveRDS(recommender, file = "optmised_product_recommender_model.rds")


# Making Predictions
# Load the saved recommender model from a file (when needed)
recommender <- readRDS("optmised_product_recommender_model.rds")

# Select a random user ID
random_user_id <- sample(id_mappings$user_map$original_id, 1)

# Get numeric ID for the random user
numeric_user_id <- id_mappings$user_map %>%
  filter(original_id == random_user_id) %>%
  pull(numeric_id)

# Prepare a list of all items for predictions
items_to_predict <- id_mappings$item_map$numeric_id

# Create a data frame for predictions
prediction_input <- data.frame(
  reviewerID = numeric_user_id,
  asin = items_to_predict
)

# Convert to matrix and save to a file
prediction_input_matrix <- as.matrix(prediction_input)
write.table(prediction_input_matrix,
            file = "prediction_input.txt",
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE)

# Generate predictions
prediction_output_file <- tempfile()
recommender$predict("prediction_input.txt", out_file(prediction_output_file))

# Read predicted scores
predicted_scores <- scan(prediction_output_file)

# Combine items and predictions
predictions <- data.frame(
  item_id = items_to_predict,
  predicted_rating = predicted_scores
)

# Map numeric item IDs back to original IDs
predictions$item_original_id <- id_mappings$item_map$original_id[
  match(predictions$item_id, id_mappings$item_map$numeric_id)
]

# Sort predictions by rating (highest to lowest)
predictions <- predictions[order(-predictions$predicted_rating), ]

# Now, perform the left join with toys_and_games_meta_data to include metadata (title, price)
predictions_with_metadata <- predictions %>%
  left_join(toys_and_games_meta_data, by = c("item_original_id" = "asin"), relationship = "many-to-many") %>%
  select(title, predicted_rating)  # Select only the relevant columns


# Cap titles at 60 characters and add a number column
top_10_recommendations_title <- predictions_with_metadata %>%
  head(10) %>%
  mutate(
    title = str_sub(title, 1, 60),  # Cap titles at 60 characters
    Number = seq_along(title)      # Add a sequential number column
  ) 

# Display the table with the number column
top_10_recommendations_title %>%
  select(Number, title, predicted_rating) %>%  # Select columns in the desired order
  kable("html", col.names = c("No.", "Title", "Predicted Rating")) %>%
  add_header_above(setNames(3, paste("Top Picks for reviewer", random_user_id))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)


# Load the saved recommender model from a file (when needed)
recommender <- readRDS("optmised_product_recommender_model.rds")

# Prepare a list of all items for predictions (same for all users)
items_to_predict <- id_mappings$item_map$numeric_id

# Get the total number of users
total_users <- length(id_mappings$user_map$original_id)
cat("Total number of users: ", total_users, "\n")

# Limit to 10% of users
num_users_to_process <- min(20818, total_users)
cat("Number of users being processed: ", num_users_to_process, "\n")

# Initialize an empty data frame to store all predictions
all_predictions <- data.frame()

# Create a single output file for all users' predictions
prediction_output_file <- tempfile()  # Use only one temporary file

# Loop through the first 40 users (or total_users, whichever is smaller)
for (user_index in seq_along(id_mappings$user_map$original_id)[1:num_users_to_process]) {
  # Get the current user ID
  random_user_id <- id_mappings$user_map$original_id[user_index]
  
  # Print the progress for each user
  cat("Processing user ", user_index, " of ", num_users_to_process, " (User ID: ", random_user_id, ")\n")
  
  # Get numeric ID for the current user
  numeric_user_id <- id_mappings$user_map %>%
    filter(original_id == random_user_id) %>%
    pull(numeric_id)
  
  # Create a data frame for the current user's predictions
  prediction_input <- data.frame(
    reviewerID = numeric_user_id,
    asin = items_to_predict
  )
  
  # Convert to matrix and save to a file
  prediction_input_matrix <- as.matrix(prediction_input)
  write.table(prediction_input_matrix,
              file = "prediction_input.txt",
              sep = " ",
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  
  # Generate predictions (suppress warnings and messages)
  suppressMessages({
    suppressWarnings({
      recommender$predict("prediction_input.txt", out_file(prediction_output_file))
    })
  })
  
  # Read predicted scores (suppress any potential output from scan)
  suppressMessages({
    suppressWarnings({
      predicted_scores <- scan(prediction_output_file)
    })
  })
  
  # Combine items and predictions for this user
  predictions <- data.frame(
    user_id = numeric_user_id,
    item_id = items_to_predict,
    predicted_rating = predicted_scores
  )
  
  # Sort predictions for the current user by rating (highest to lowest) and pick top 50
  inclusion_number <- 50
  
  top_predictions_for_user <- predictions[order(-predictions$predicted_rating), ][1:inclusion_number, ]
  
  # Assign weighted scores (50 for top item, 49 for second, ..., 1 for 50th)
  top_predictions_for_user$weighted_score <- (inclusion_number + 1) - seq_along(top_predictions_for_user$predicted_rating)
  
  # Append only the top 50 predictions with weighted scores to the overall predictions data frame
  all_predictions <- rbind(all_predictions, top_predictions_for_user)
}

# Now, calculate the total weighted score, the number of recommendations, and average rank for each item
item_weighted_scores <- all_predictions %>%
  group_by(item_id) %>%
  summarise(
    total_weighted_score = sum(weighted_score),  # Sum of weighted scores for the item
    num_recommendations = n(),  # Number of times this item appeared in top 50
    avg_ranking_position = mean(seq_along(weighted_score)),  # Average ranking position (from 1 to 50)
    top_position = min((inclusion_number + 1) - weighted_score),  # Best rank (lowest numerical position)
    lowest_position = max((inclusion_number + 1) - weighted_score),  # Worst rank (highest numerical position)
    .groups = "drop"
  )

# Calculate the average weighted score by dividing the total weighted score by the number of loops
item_weighted_scores$avg_weighted_score <- item_weighted_scores$total_weighted_score / num_users_to_process

# Map numeric item IDs back to original IDs
item_weighted_scores$item_original_id <- id_mappings$item_map$original_id[
  match(item_weighted_scores$item_id, id_mappings$item_map$numeric_id)
]

# Calculate the occurrences of each item in the original training set
item_occurrences <- training_set %>%
  group_by(asin) %>%
  summarise(occurrences = n(), .groups = "drop")

# Join the occurrences with the item_weighted_scores
final_weighted_predictions <- item_weighted_scores %>%
  left_join(toys_and_games_meta_data, by = c("item_original_id" = "asin"), relationship = "many-to-many") %>%
  left_join(item_occurrences, by = c("item_original_id" = "asin")) %>%
  arrange(-avg_weighted_score) %>%  # Order by average weighted score (highest to lowest)
  select(title, total_weighted_score, avg_weighted_score, avg_ranking_position, top_position, lowest_position, num_recommendations, occurrences)

# Number of users processed
num_users_to_process

# View the top 50 recommended products based on the weighted score
head(final_weighted_predictions)

# Save the final weighted predictions to a CSV file
write.csv(final_weighted_predictions, 
          file = paste0("final_weighted_predictions_", num_users_to_process, "_users.csv"), 
          row.names = FALSE)


# Read the CSV file
grouped_predictions <- read.csv(file.path(workingDirectory, "final_weighted_predictions 20818 users.csv"))

# Compute new columns
grouped_predictions <- grouped_predictions %>%
  mutate(
    avg_position = round(50 - avg_weighted_score, 2),
    percentage_recommended = round((num_recommendations / num_users_to_process) * 100, 2),
    opportunity = round(((num_recommendations - occurrences) / num_users_to_process) * 100, 2)
  )

# Remove specified columns
grouped_predictions <- grouped_predictions %>%
  select(-total_weighted_score, -avg_weighted_score, -avg_ranking_position)

# Reorder columns
grouped_predictions <- grouped_predictions %>%
  select(
    title,
    avg_position,
    opportunity,
    top_position,
    lowest_position,
    percentage_recommended,
    num_recommendations,
    occurrences,
    everything() # Ensures other columns (if any) are retained at the end
  )

# Write updated data to CSV
write.csv(grouped_predictions, 
          file = file.path(workingDirectory, "final_weighted_predictions_users.csv"), 
          row.names = FALSE)




grouped_recommendations <- read.csv(file.path(workingDirectory, "final_weighted_predictions_users.csv"))

# Create the kable table
grouped_recommendations %>%
  arrange(desc(opportunity)) %>% 
  head(10) %>% 
  kable("html", col.names = c("Title", "Average Position", "Percentage Opportunity", "Top Position", 
                              "Lowest Position", "Percentage Recommended", 
                              "Num Recommendations", "Occurrences"), align = "c") %>% 
  add_header_above(c("Grouped Recommendations" = ncol(grouped_recommendations))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) 


# Set seed for reproducability
set.seed(42)

# Load the saved recommender model from a file (when needed)
recommender <- readRDS("optmised_product_recommender_model.rds")

# Extract the Q matrix (latent factors)
latent_factors <- recommender$model$matrices$Q@Data

# Apply t-SNE with lower perplexity
tsne_result <- Rtsne(latent_factors, dims = 2, perplexity = 4, verbose = T)


# Number of latent factors (columns in Q matrix) - the total number of latent factors in the model
num_latent_factors_total <- ncol(latent_factors)

cat("Total number of latent factors in the model:", num_latent_factors_total, "\n")

# Plot the t-SNE result
plot(tsne_result$Y, 
     main = "t-SNE of Latent Factor Matrix", 
     xlab = "t-SNE 1", 
     ylab = "t-SNE 2", 
     pch = 19, 
     col = "orange")

# Extract the item mapping (original IDs to numeric IDs)
item_map <- id_mappings$item_map

# Access Q_matrix (latent factor matrix)
Q_matrix <- recommender$model$matrices$Q

# Remove the first column of Q_matrix if it contains only NaN values
if (all(is.na(Q_matrix[, 1]))) {
  cat("Removing the first column of Q_matrix as it contains only NaN values...\n")
  Q_matrix <- Q_matrix[, -1]
}

# Loop through all 15 latent factors
for (latent_factor_id in 1:15) {
  
  # Select the latent factor values for the current factor (e.g., latent factor 1)
  latent_factor_values <- Q_matrix[latent_factor_id, ]  # Values for the current latent factor
  
  # Convert the latent factor values to a numeric vector (in case they are not numeric)
  latent_factor_values_numeric <- as.numeric(latent_factor_values)
  
  # Create a data.table with product IDs and corresponding latent factor values
  dt <- data.table(
    product_id = item_map$original_id,  # Product IDs (ASINs)
    latent_value = latent_factor_values_numeric  # Numeric latent factor values for this factor
  )
  
  # Remove rows with NaN values from the data.table (if any)
  dt <- dt[!is.na(latent_value), ]
  
  # Add an index column to preserve the original positions
  dt[, original_index := .I]  # The original index of each product before sorting
  
  # Sort the data.table by latent factor values (in descending order)
  setorder(dt, -latent_value)  # Sort by latent value in decreasing order
  
  # Get the top 10 items (instead of just 5)
  top_indices <- dt[1:50, .(product_id, latent_value, original_index)]  # Top 10 items
  
  # Perform a left join with the metadata table (toys_and_games_meta_data)
  top_indices_with_meta <- left_join(top_indices, toys_and_games_meta_data, by = c("product_id" = "asin"))
  
  # Remove duplicates after the left join, if any
  top_indices_with_meta <- top_indices_with_meta[!duplicated(top_indices_with_meta$product_id), ]
  
  # Remove the 'price' column, if it exists
  top_indices_with_meta <- top_indices_with_meta[, !("price"), with = FALSE]
  
  # Reorder columns: title, product_id, latent_value, original_index
  top_indices_with_meta <- top_indices_with_meta[, .(title, product_id, latent_value, original_index)]
  
  # Save the result to a CSV file for the current latent factor
  file_name <- paste0(workingDirectory, "/top_indices/top_indices_latent_factor_", latent_factor_id, ".csv")
  
  write.csv(top_indices_with_meta, file_name, row.names = FALSE)
  
  # Print the result for the current latent factor
  cat("Top 50 products for Latent Factor", latent_factor_id, "\n")
  print(top_indices_with_meta)
  cat("\n")
}

# Initialize a data frame to store cluster names
cluster_names <- data.frame(Cluster = integer(), Name = character(), stringsAsFactors = FALSE)

# Loop through all 15 latent factor files
for (latent_factor_id in 1:15) {
  # Read the CSV file for the current latent factor
  file_path <- file.path(workingDirectory, paste0("top_indices/top_indices_latent_factor_", latent_factor_id, ".csv"))
  top_10_latent_factor_products <- read.csv(file_path)
  
  # Prepare the prompt for LLaMA
  prompt <- paste(
    "Here is a cluster of items grouped by their similarity in features.",
    paste("Cluster", latent_factor_id, "items:", sep = " "),
    paste(top_10_latent_factor_products$title, collapse = "\n"),
    "\n\nPlease analyse the product names and return with a detailed category name. Keep your answer under 20 words, and don’t include any of the product names. Do try and be as specific as possible, as we want category names that really capture the products meaning. Do not write any other comments, we only want the category name."
  )
  
  # Query LLaMA and extract the response
  response <- query(prompt)  # Replace this with the appropriate LLaMA API call
  cluster_name <- paste(response[[1]]$message$content, collapse = "\n")
  
  # Append the cluster name to the data frame
  cluster_names <- rbind(cluster_names, data.frame(Cluster = latent_factor_id, Name = cluster_name, stringsAsFactors = FALSE))
}

# Save all cluster names to a CSV file
output_file <- file.path(workingDirectory, "cluster_name/cluster_names_longer.csv")
write.csv(cluster_names, output_file, row.names = FALSE)

# Load category names
category_names <- read.csv(file.path(workingDirectory, paste0("cluster_name/cluster_names_longer.csv")))
category_names <- category_names$Name

# Create a data frame with an index and the category names (for 1 to 15)
category_names_with_index <- data.frame(Index = 1:15, Name = category_names[1:15])

# Create the kable table
category_names_with_index %>%
  kable("html", col.names = c("Index", "Category Name")) %>%
  add_header_above(setNames(2, "Category Names From Llama3")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)