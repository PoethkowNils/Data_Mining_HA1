library(plotly)

# probability_entropy <- function(p) {
#     # Handle cases where p = 0 or 1 to avoid log(0)
#     p <- p[p > 0 & p < 1]
#     return(-sum(p * log(p) + (1 - p) * log(1 - p)))
# }

# poethkow_updated_entropy <- function(feature, num_bins) {
#   # create sectors
#   bins <- cut(feature, breaks = num_bins, include.lowest = TRUE) # 

#   # fraction of points in each bin/sector
#   p_i <- table(bins) / length(feature)
#   print(p_i)

#   # Compute the entropy for this feature
#   return(probability_entropy(p_i))
# }

poethkow_entropy <- function(labels) {

    n_data_points <- length(labels)

    n_occ <- table(labels) # number of occurances for every label
    n_labels <- length(n_occ)

    entropy <- 0
    for (label in n_occ) {
        entropy <- entropy - ((label/n_data_points)*log2(label/n_data_points))
    }

    return (entropy)
}

poethkow_fisher_score <- function(dataset, feature) {

    labels <- dataset$label          
    unique_labels <- unique(labels)  
    n_occ <- table(labels)  # occurances for each label
    
    mean_total <- mean(dataset[[feature]])  # Calculate mean for the specified feature
    
    # numerator and denominator of the fisher score formula
    numerator <- 0
    denominator <- 0

    for (label in unique_labels) {

        subset <- dataset[dataset$label == label, feature]
        
        mean_class <- mean(subset)
        var_class <- var(subset)
        
        n_class <- n_occ[as.character(label)]
        
        # numerator (inter-class variance)
        numerator <- numerator + n_class * (mean_class - mean_total)^2
        # denominator (intra-class variance)
        denominator <- denominator + n_class * var_class
    }

    return(numerator / denominator) # return fisher score
}

# Test with dataset: separable by one and non separable by the other feature

set.seed(120) # for Reproducibility
n <- 100

# Generate separable data for 'x' and non-separable data for 'y'
ds <- data.frame(
  x = c(rnorm(n/2, mean = -5, sd = 1), rnorm(n/2, mean = 5, sd = 1)),  # Separable by x (distinct means)
  y = c(rnorm(n, mean = 0, sd = 1.5)),                                # Non-separable by y (same mean for both classes)
  label = rep(0:1, each = n/2)
)

ones = matrix(c(rep(1,n/2)),ncol=1)
twos = matrix(c(rep(2,n/2)),ncol=1)

labels = rbind(ones,twos)  # Combine the labels into one vector
labels_factor = as.factor(labels)  # Convert labels to a factor for color coding in the plot

# Plotting the dataset
plot_ly(x = ~ds[,1], y = ~ds[,2], type = 'scatter', mode = 'markers',
        marker = list(size = 10), color = ~labels_factor, colors=c('blue', 'red')) %>%
  layout(title = "2D Scatter Plot",
         xaxis = list(title = "X-axis"),
         yaxis = list(title = "Y-axis"))


cat("Fisher score x:", poethkow_fisher_score(ds, "x"), "\n")
cat("Fisher score y:", poethkow_fisher_score(ds, "y"))



#----- Updated Version for Entropy -----#

entropy <- function(p) {
  p <- p[p > 0]  # Avoid log(0)
  return(-sum(p * log(p)))
}
# Compute the pairwise distances between all points
distances <- as.vector(dist(ds))  # Flatten distances

# Define the number of bins for discretization (e.g., 10 bins)
num_bins <- 10

# Discretize the distances into bins
distance_bins <- cut(distances, breaks = num_bins, include.lowest = TRUE)

# Calculate the fraction of distances in each bin (probability distribution)
p_i <- table(distance_bins) / length(distances)

distance_entropy <- entropy(p_i)

cat("Distance-based entropy:", distance_entropy, "\n")



# Test with non clustered data

#set.seed(300) # for Reproducibility
n <- 100

ds2 <- data.frame(
  x = c(rnorm(n, mean = -5, sd = 1)),
  y = c(rnorm(n, mean = -5, sd = 1)),
  label = rep(0:1, each = n/2)
)

ones = matrix(c(rep(1,n/2)),ncol=1)
twos = matrix(c(rep(2,n/2)),ncol=1)

labels = rbind(ones,twos)  # Combine the labels into one vector
labels_factor = as.factor(labels)  # Convert labels to a factor for color coding in the plot

# Plotting the dataset
plot_ly(x = ~ds2[,1], y = ~ds2[,2], type = 'scatter', mode = 'markers',
        marker = list(size = 10), color = ~labels_factor, colors=c('blue', 'red')) %>%
  layout(title = "2D Scatter Plot",
         xaxis = list(title = "X-axis"),
         yaxis = list(title = "Y-axis"))

distances <- as.vector(dist(ds2))  
num_bins <- 10
distance_bins <- cut(distances, breaks = num_bins, include.lowest = TRUE)
p_i <- table(distance_bins) / length(distances)
distance_entropy <- entropy(p_i)

cat("Distance-based entropy:", distance_entropy, "\n")
