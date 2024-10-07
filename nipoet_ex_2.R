library(plotly)

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

    labels <- dataset$label          # Extract the label column
    unique_labels <- unique(labels)  # Get unique class labels
    n_occ <- table(labels)           # Occurrences of each label
    
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
k <- 3
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
  layout(title = "2D Scatter Plot: Separable by Feature x, Non-separable by Feature y",
         xaxis = list(title = "X-axis (Feature x: Separable)"),
         yaxis = list(title = "Y-axis (Feature y: Non-separable)"))


print(poethkow_fisher_score(ds, "x"))
print(poethkow_fisher_score(ds, "y"))
