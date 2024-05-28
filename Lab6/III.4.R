read_sample_from_file <- function(filename) {
  sample <- as.numeric(readLines(filename))
  return(sample[!is.na(sample)])
}


calculate_mean <- function(sample) {
  return(mean(sample))
}


calculate_sd <- function(sample) {
  return(sd(sample))
}


calculate_confidence_interval <- function(sample, confidence_level) {
  n <- length(sample)
  mean_sample <- calculate_mean(sample)
  sd_sample <- calculate_sd(sample)
  

  z <- qnorm((1 + confidence_level) / 2)
  
  margin_of_error <- z * (sd_sample / sqrt(n))
  lower_bound <- mean_sample - margin_of_error
  upper_bound <- mean_sample + margin_of_error
  
  return(list(mean = mean_sample, sd = sd_sample, sample_size = n, lower_bound = lower_bound, upper_bound = upper_bound))
}


sample <- read_sample_from_file('history.txt')

confidence_interval_95 <- calculate_confidence_interval(sample, 0.95)
cat("95% Confidence Interval:\n")
cat("Sample Size:", confidence_interval_95$sample_size, "\n")
cat("Sample Mean:", round(confidence_interval_95$mean, 2), "\n")
cat("Sample Standard Deviation:", round(confidence_interval_95$sd, 2), "\n")
cat("95% Confidence Interval: (", round(confidence_interval_95$lower_bound, 2), ", ", round(confidence_interval_95$upper_bound, 2), ")\n", sep = "")

confidence_interval_99 <- calculate_confidence_interval(sample, 0.99)
cat("99% Confidence Interval:\n")
cat("Sample Size:", confidence_interval_99$sample_size, "\n")
cat("Sample Mean:", round(confidence_interval_99$mean, 2), "\n")
cat("Sample Standard Deviation:", round(confidence_interval_99$sd, 2), "\n")
cat("99% Confidence Interval: (", round(confidence_interval_99$lower_bound, 2), ", ", round(confidence_interval_99$upper_bound, 2), ")\n", sep = "")
