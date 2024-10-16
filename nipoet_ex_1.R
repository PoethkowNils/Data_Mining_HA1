calc_distance <- function(method=NaN, p1, p2) { #just the distance between two points

  if (length(p1)!=length(p2)) {
    stop("Points have different dimenions")
  }

  if (identical(method, "manhattan")) {
    sum <- 0
    for (i in 1:length(p1)) {
        sum <- sum + abs(p1[i] - p2[i])
    }
    return (sum)
  }
  else if (identical(method, "euclidean")) {
    sum <- 0
    for (i in 1:length(p1)) {
      sum <- sum + (p1[i] - p2[i])^2
    }
    return (sum^0.5)
  }
  else if (identical(method, "chebyshev")) {
    # maybe do this differently (can also be done without storing all values, just compare current largest with new calculated one)

    distances<-abs(p1[1]-p2[1])
    for (i in 2:length(p1)) {
        distances[i]<-abs(p1[i]-p2[i])
    }
    sort(distances)
    return (distances[length(p1)])
  }
  
  stop("Invalid method specified.")
}

# Define two points in 2D space
p1 <- c(1, 2)  # Point 1
p2 <- c(4, 6)  # Point 2

# Calculate distances
manhattan_distance <- calc_distance("manhattan", p1, p2)
euclidean_distance <- calc_distance("euclidean", p1, p2)
chebyshev_distance <- calc_distance("chebyshev", p1, p2)
print(manhattan_distance)
print(euclidean_distance)
print(chebyshev_distance)