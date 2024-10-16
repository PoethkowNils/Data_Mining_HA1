source("nipoet_ex_2.ex")
source("nipoet_ex_3_tree_functions.R")
source("nipoet_ex_3_metrics.R")
source("nipoet_ex_3_gaus.R")

# R3 dataset with overlapping
seed = 123
set.seed(seed)
# Generating Gaussians
n = 200
ds_1 = poethkow_gen_gaus_3d(n, c(0,0,0), c(2,2,2), alpha=90) # from functions.R
ds_2 = poethkow_gen_gaus_3d(n, c(3,3,3), c(1,4,2), alpha=180)

ds = rbind(ds_1, ds_2) #dataset
# Generating labels = {1,2}
ones = matrix(c(rep(1,n)),ncol=1)
twos = matrix(c(rep(2,n)),ncol=1)
#rep -> class labels

labels = rbind(ones,twos)# what does this do??
labels_factor = as.factor(labels)

#Create 3D scatter plot
plot_ly(x = ~ds[,1], y = ~ds[,2], z = ~ds[,3], type = 'scatter3d', mode = 'markers',
        marker = list(size = 10),  color = ~labels_factor, colors=c('blue', 'red')) %>%
  layout(title = "3D Scatter Plot",
         xaxis = list(title = "X-axis"),
         yaxis = list(title = "Y-axis"),
         zaxis = list(title = "Z-axis"))

ds = data.frame(cbind(ds, labels))
print(ds)

#----- Plot Tree -----#
tree <- poethkow_train_tree(ds[,1:ncol(ds)-1], ds[,ncol(ds)])
result <- poethkow_tree_predict(tree, ds)

# Set up the plot
#par(pty = "s", mar = c(2, 2, 2, 2), pin = c(5, 5))
dev.off()
plot.new()
max_depth <- 4  # Adjust this based on the expected maximum depth of your tree
plot.window(xlim = c(-1, 1), ylim = c(-max_depth, 1))  # Adjust limits based on your tree size
title(main = "Decision Tree Visualization")

poethkow_plot_tree(tree)


