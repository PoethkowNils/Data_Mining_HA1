# function for calculating true_pos, ... etc.
poethkow_calculations <- function(predictions, observations) { 
    total_true_pos <- 0
    total_false_pos <- 0
    total_true_neg <- 0
    total_false_neg <- 0

    labels <- unique(predictions)

    for (label in labels) {
        tp <- sum(predictions == label & observations == label)
        
        fp <- sum(predictions == label & observations != label)
    
        tn <- sum(predictions != label & observations != label)
        
        fn <- sum(predictions != label & observations == label)

        total_true_pos <- total_true_pos + tp
        total_false_pos <- total_false_pos + fp
        total_true_neg <- total_true_neg + tn
        total_false_neg <- total_false_neg + fn
    }

    return(list(
        true_pos = total_true_pos, 
        false_pos = total_false_pos, 
        true_neg = total_true_neg, 
        false_neg = total_false_neg
    ))
}

poethkow_metrics <- function(predictions, observations) {

    results <- poethkow_calculations(predictions, observations)

    precision <- results$true_pos / (results$true_pos+results$false_pos)
    recall <- results$true_pos / (results$true_pos+results$false_neg)
    accuracy <- (results$true_pos+results$true_neg) / (results$true_pos+results$true_neg + results$false_pos+results$false_neg)
    f1_score <- (recall*precision) / (recall+precision)

    return (list(accuracy=accuracy , precision=precision , recall=recall, f1_score=f1_score))
}
