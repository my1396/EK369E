quick_summary <- function(x, full=FALSE) {
    # Function to compute basic descriptive statistics
    if (!full){
        # Basic summary
        data.frame(
          n = length(x),
          min = min(x),
          mean = mean(x),
          median = median(x),
          max = max(x),
          sd = sd(x),
          skewness = moments::skewness(x),
          kurtosis = moments::kurtosis(x),
          row.names = NULL
        )
    } else {
        # Full summary with quartiles and IQR
        data.frame(
          n = length(x),
          min = min(x),
          Q1 = quantile(x, 0.25),
          mean = mean(x),
          median = median(x),
          Q3 = quantile(x, 0.75),
          max = max(x),
          IQR = IQR(x),
          sd = sd(x),
          skewness = moments::skewness(x),
          kurtosis = moments::kurtosis(x),
          row.names = NULL
        )
    }
}