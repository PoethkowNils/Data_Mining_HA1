#distance function

calc_distance <- function(method=NaN, p1, p2) { #just the distance between two points
  
  # maybe implement some checks
    # checks if points have same dimensions
  
  if (identical(method, "manhatten")) {
    sum <- 0
    for (i in 1:length(p1)) {
        sum <- sum + (p1[i] + p2[i])
    }
    return (sum)
  }
  else if (identical(method, "euclidean")) {
    sum <- 0
    for (i in 1:length(p1)) {
      sum <- sum + abs(p1[i] - p2[i])
    }
    return (sum)
  }
  else if (identical(method, "russe")) {
    #maybe do this differently (can also be done without storing all values, just compare current largest with new calculated one)

    distances<-abs(p1[1]-p2[1])
    for (i in 2:length(p1)) {
        distances[i]<-abs(p1[i]-p2[i])
    }
    sort(distances)
    return (distances[length(p1)])
  }
  
  return (Nan)
}

p1<-c(1,3,2)

calc_distance("russe", p1, c(2,3,4))

