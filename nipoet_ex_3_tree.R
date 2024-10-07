library(plotly)
source("nipoet_2_ex.R")

# train test split 
train_test_split <- function(dataset, labels, ratio = 0.8) {
    # Shuffle the dataset and labels simultaneously
    set.seed(42)  # Set a seed for reproducibility
    shuffled_indices <- sample(1:nrow(dataset))
    dataset <- dataset[shuffled_indices, ]
    labels <- labels[shuffled_indices]
    
    # Calculate the split index
    split <- as.integer(ratio * nrow(dataset))
    
    # Split the dataset into training and testing sets
    train_data <- dataset[1:split, ]
    test_data <- dataset[(split + 1):nrow(dataset), ]
    
    # Split the labels accordingly
    train_labels <- labels[1:split]
    test_labels <- labels[(split + 1):length(labels)]
    
    return(list(train_data = train_data, test_data = test_data,
                train_labels = train_labels, test_labels = test_labels))
}

source("nipoet_ex_3_gaus.R")

# R2 dataset with overlapping
seed = 123
set.seed(seed)
# Generating Gaussians
n = 200
ds_1 = poethkow_gen_gaus(n,c(0,0), c(2,2), alpha=90) # from functions.R
ds_2 = poethkow_gen_gaus(n,c(3,3), c(1,4), alpha=180)

ds = rbind(ds_1, ds_2) #dataset
# Generating labels = {1,2}
ones = matrix(c(rep(1,n)),ncol=1)
twos = matrix(c(rep(2,n)),ncol=1)
#rep -> class labels

labels = rbind(ones,twos)# what does this do??
labels_factor = as.factor(labels)

# Create 2D scatter plot
# plot_ly(x = ~ds[,1], y = ~ds[,2], type = 'scatter', mode = 'markers',
#         marker = list(size = 10),  color = ~labels_factor, colors=c('blue', 'red')) %>%
#   layout(title = "2D Scatter Plot",
#          xaxis = list(title = "X-axis"),
#          yaxis = list(title = "Y-axis"))

ds = data.frame(cbind(ds, labels))
print(ds)


#------visualization------#
poethkow_plot_tree <- function(node, depth = 0, pos = 0, x_offset = 0.5, y_offset = 0.5) {
    # Set the position for drawing
    x_pos <- pos

    if (!is.null(node$label)) {
        # leaf node
        text(x_pos, -depth - 0.1, node$label, adj = c(0.5, 0.5), col = "blue", cex = 1.2)  # Lower the text slightly
    } else {
        # split condition
        text(x_pos, -depth - 0.1, paste(node$feature, "<=", round(node$split, 2)), adj = c(0.5, 0.5), cex = 1.2)  # Lower the text slightly

        # left branch
        segments(x_pos, -depth, x_pos - x_offset, -(depth + y_offset), lwd = 2)  # Line to the left child node
        poethkow_plot_tree(node$left_node, depth + y_offset, x_pos - x_offset, x_offset / 2, y_offset) # half of x_offset so the tree fits, y_offset stays the same

        # right branch
        segments(x_pos, -depth, x_pos + x_offset, -(depth + y_offset), lwd = 2)  # Line to the right child
        poethkow_plot_tree(node$right_node, depth + y_offset, x_pos + x_offset, x_offset / 2, y_offset)
    }
}
#------ ------#

# determine the information gain of a split (to select best split)
poethkow_information_gain <- function(data, labels) { # maybe rename to feature_column or something

    best_entropy <- 1
    best_split <- NULL #the value which the dataset should be split at

    # sort feature_column
    sorted_indices <- order(data)
    data_sorted <- data[sorted_indices]  # Explain how this works!!!!!
    labels_sorted <- labels[sorted_indices]
    

    # loop through dataset
    for (i in 2:(length(data)-1)) { # first and last index as splits wouldn't make any sense

        subset <- data_sorted[1:i]
        subset_labels <- labels_sorted[1:i]

        # weigthed entropy
        entropy <- ((i/length(labels))*poethkow_entropy(subset_labels))+(1-(i/length(labels)))*poethkow_entropy(labels_sorted[i+1:length(labels)])
        # maybe define some variables here
        
        if (entropy<best_entropy) {
            best_entropy <- entropy
            best_split <- data_sorted[i]
        }

    }
    return (list(best_entropy=best_entropy, best_split=best_split)) # return the gain of the best split along with the best split (threshhold)
}

# split label column from feature columns first
poethkow_train_tree <- function(dataset, labels, current_depth=1, max_depth = 4) { # call function recursivly

    # Stopping conditions
    if (current_depth > max_depth || length(unique(labels)) == 1 || nrow(dataset) == 0) { # when there is only one feature in split left
        # Return majority label or the single label if all labels are the same
        majority_label <- names(which.max(table(labels)))
        return(list(label = majority_label))  # Return as a leaf node with the majority label
    }

    # find feature with best gain
    best_gain <- 1   # determine best gain, can't be NULL  
    best_feature <- NULL   # store best feature
    best_split <- NULL  # store best split (threshold were one side is greater and the other is smaller) -> a simple split

    for (feature in names(dataset)) {
        gain <- poethkow_information_gain(dataset[[feature]], labels)

        if (is.null(best_feature)) {
            best_feature <- feature
            best_gain <- gain$best_entropy
            best_split <- gain$best_split

        } else if (gain$best_entropy<best_gain) {
            best_feature <- feature
            best_gain <- gain$best_entropy # rename
            best_split <- gain$best_split

        }
    }

    # create subset of dataset
    # split dataset in two part in respect to the before calculated best split (threshold)
    left_subdata <- dataset[dataset[[best_feature]] <= best_split, ]
    right_subdata <- dataset[dataset[[best_feature]] > best_split, ]
    left_labels <- labels[dataset[[best_feature]] <= best_split]
    right_labels <- labels[dataset[[best_feature]] > best_split]

    # call "train_tree" recursivly
    left_subtree <- poethkow_train_tree(left_subdata, left_labels, current_depth+1) # max depth is default argument
    right_subtree <- poethkow_train_tree(right_subdata, right_labels, current_depth+1)

    # construct the tree like a linked list
    return (list(
        left_node = left_subtree,
        right_node = right_subtree,
        feature = best_feature,
        split = best_split
    ))
}

poethkow_tree_predict <- function(tree, dataset) {    # dataset here does not include labels
    
    # iterate through tree using the splits
    predictions <- numeric(nrow(dataset))

    # not recursive, so it's more efficient
    for (i in 1:nrow(dataset)) {

        node <- tree

        while (is.null(node$label)) {
            feature <- node$feature

            if (dataset[[feature]][i] <= node$split) { # not sure if this works
                node <- node$left_node
            } else {
                node <- node$right_node
            }
        }
        predictions[i] <- node$label
    }

    return (predictions)
}

#----- Plot Tree -----#
tree <- poethkow_train_tree(ds[,1:ncol(ds)-1], ds[,ncol(ds)])
result <- poethkow_tree_predict(tree, ds)

# Set up the plot
plot.new()
max_depth <- 5  # Adjust this based on the expected maximum depth of your tree
plot.window(xlim = c(-1, 1), ylim = c(-max_depth, 1))  # Adjust limits based on your tree size
title(main = "Decision Tree Visualization")

poethkow_plot_tree(tree)


