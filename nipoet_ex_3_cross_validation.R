source("nipoet_ex_3_tree.R")
source("nipoet_ex_3_metrics.R")
source("nipoet_ex_3_gaus.R")
library(ggplot2)

# cross validation
poethkow_cross_validate <- function(dataset, labels, k = 4, max_depth = 5) {
    # Initialize accuracy storage
    accuracies <- numeric(k)
    
    # Shuffle dataset
    #set.seed(42)  # ADJUSTMENT: no seed
    shuffled_indices <- sample(1:nrow(dataset))
    dataset <- dataset[shuffled_indices, ]
    labels <- labels[shuffled_indices]

    # Create folds
    folds <- split(1:nrow(dataset), cut(1:nrow(dataset), breaks = k, labels = FALSE))
    
    for (i in 1:k) {
        # Separate test indices for this fold
        test_indices <- folds[[i]]
        train_indices <- unlist(folds[-i])  # all the remaining indices
        
        #ADJUSTMENT: Previous train test split was nonsense
        train_data <- dataset[train_indices, ]
        test_data <- dataset[test_indices, ]  # Use the entire test set from the fold
        train_labels <- labels[train_indices]
        test_labels <- labels[test_indices]
        
        # Train the decision tree
        tree <- poethkow_train_tree(train_data, train_labels, max_depth = max_depth)
        
        # Make predictions on the test set
        predictions <- poethkow_tree_predict(tree, test_data)
        
        # Metric, here accuracy
        accuracies[i] <- poethkow_metrics(predictions, test_labels)$accuracy
    }
    
    # Return mean accuracy and standard deviation
    return(list(mean_accuracy = mean(accuracies), sd_accuracy = sd(accuracies)))
}

# R2 dataset with overlapping
# seed = 150
# set.seed(seed)
n = 200
ds_1 = poethkow_gen_gaus_2d(n,c(0,0), c(2,2), alpha=90) # from functions.R
ds_2 = poethkow_gen_gaus_2d(n,c(3,3), c(1,4), alpha=180)

ds = rbind(ds_1, ds_2) #dataset
# Generating labels = {1,2}
ones = matrix(c(rep(1,n)),ncol=1)
twos = matrix(c(rep(2,n)),ncol=1)
#rep -> class labels

labels = rbind(ones,twos)# what does this do??
labels_factor = as.factor(labels)

ds = data.frame(cbind(ds, labels))

# Depth range to evaluate
depths <- 1:10
accuracy_results <- numeric(length(depths))  # Store accuracy for each depth

# Loop over different tree depths and calculate accuracy using cross-validation
for (depth in depths) {
    cv_result <- poethkow_cross_validate(ds[,1:(ncol(ds)-1)], ds[,ncol(ds)], k = 5, max_depth = depth)
    accuracy_results[depth] <- cv_result$mean_accuracy  # Store mean accuracy
}

# Create a data frame for plotting
accuracy_df <- data.frame(
    Depth = depths,
    Accuracy = accuracy_results
)

# Plot the results
p <- ggplot(accuracy_df, aes(x = Depth, y = Accuracy)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = "Decision Tree Accuracy vs. Depth",
         x = "Tree Depth",
         y = "Accuracy") +
    theme_minimal() +
    ylim(0.8, 1.0)

# Save the plot as a smaller PNG
ggsave("accuracy_plot.png", plot = p, width = 4, height = 3, dpi = 150)