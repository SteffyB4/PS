read_sample_from_file <- function(filename) {
  sample <- as.numeric(readLines(filename))
  return(sample[!is.na(sample)])
}

calculate_mean <- function(sample) {
  return(mean(sample))
}

calculate_confidence_interval <- function(sample, sigma, confidence_level = 0.95) {
  n <- length(sample)
  mean_sample <- calculate_mean(sample)
  
  z <- qnorm((1 + confidence_level) / 2)
  
  margin_of_error <- z * (sigma / sqrt(n))
  lower_bound <- mean_sample - margin_of_error
  upper_bound <- mean_sample + margin_of_error
  
  return(list(mean = mean_sample, sample_size = n, lower_bound = lower_bound, upper_bound = upper_bound))
}


sample <- read_sample_from_file('history.txt')
sigma <- 5

confidence_interval <- calculate_confidence_interval(sample, sigma)

cat("Sample Size:", confidence_interval$sample_size, "\n")
cat("Sample Mean:", round(confidence_interval$mean, 2), "\n")
cat("95% Confidence Interval: (", round(confidence_interval$lower_bound, 2), ", ", round(confidence_interval$upper_bound, 2), ")\n", sep = "")
