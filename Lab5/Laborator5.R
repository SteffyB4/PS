simulate_random_variable <- function(values, probabilities) {
  if (sum(probabilities) != 1) {
    stop("Suma probabilităților trebuie să fie egală cu 1.")
  }
  sample(values, size = 1, prob = probabilities)
}

values <- c("x1", "x2", "x3")
probabilities <- c(0.2, 0.5, 0.3)
set.seed(123)
simulate_random_variable(values, probabilities)
