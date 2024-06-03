# D1
calculate_confidence_intervals = function(file_name) {
  data <- read.csv(file_name)
  
  # Calculăm media și dispersia eșantionului
  sample_mean <- mean(data$Punctaj)
  sample_variance <- var(data$Punctaj)
  
  # Specificăm nivelurile de încredere
  confidence_levels <- c(0.95, 0.99)
  # Calculăm valorile critice Z pentru nivelurile de încredere specificate
  z_values <- qnorm(1 - (1 - confidence_levels) / 2)
  # Calculăm limitele superioare și inferioare ale intervalelor de încredere
  intervals <- matrix(NA, nrow = length(confidence_levels), ncol = 2)
  for (i in 1:length(confidence_levels)) {
    margin_of_error <- z_values[i] * sqrt(sample_variance / length(data$Punctaj))
    intervals[i, ] <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)
  }
  
  for (i in 1:length(confidence_levels)) {
    cat("Intervalul de încredere de", confidence_levels[i]*100, "%:", intervals[i, 1], "–", intervals[i, 2], "\n")
  }
}

calculate_confidence_intervals("probabilitati.csv")


# D2
calculate_confidence_intervals_statistics = function(file_name) {
  data <- read.csv(file_name)
  # Calculăm media și deviația standard a eșantionului
  sample_mean <- mean(data$Punctaj)
  sample_sd <- sd(data$Punctaj)
  confidence_levels <- c(0.95, 0.99)

  z_values <- qnorm(1 - (1 - confidence_levels) / 2)
  
  intervals <- matrix(NA, nrow = length(confidence_levels), ncol = 2)
  for (i in 1:length(confidence_levels)) {
    margin_of_error <- z_values[i] * (sample_sd / sqrt(length(data$Punctaj)))
    intervals[i, ] <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)
  }
  
  for (i in 1:length(confidence_levels)) {
    cat("Intervalul de încredere de", confidence_levels[i]*100, "%:", intervals[i, 1], "–", intervals[i, 2], "\n")
  }
}

calculate_confidence_intervals_statistics("statistica.csv")

# D3
test_prop = function(sample_size, successes, alpha) {
  # Proporția de studenți care nu pot rezolva temele înainte de schimbare
  p_before = 0.85
  
  # Proporția de studenți care nu pot rezolva temele după schimbare
  p_after = successes / sample_size
  
  # Calculăm statisticul testului Z
  z_statistic = (p_after - p_before) / sqrt(p_before * (1 - p_before) / sample_size)
  
  # Calculăm valoarea critică Z corespunzătoare nivelului de semnificație dat
  z_critical = qnorm(1 - alpha)
  
  # Afișăm rezultatele testului
  cat("Statisticul testului Z:", z_statistic, "\n")
  cat("Valoarea critică Z la nivelul de semnificație de", alpha * 100, "%:", z_critical, "\n")
  
  # Tragem concluzia
  if (z_statistic > z_critical) {
    cat("Concluzie: Se respinge ipoteza nulă. Schimbarea în structura temelor a fost utilă.\n")
  } else {
    cat("Concluzie: Nu există suficiente dovezi pentru a respinge ipoteza nulă. Nu se poate trage concluzia că schimbarea a fost utilă.\n")
  }
}

# Apelăm funcția pentru testul la nivelul de semnificație de 1%
test_prop(100, 14, 0.01)

# Apelăm funcția pentru testul la nivelul de semnificație de 5%
test_prop(100, 14, 0.05)



