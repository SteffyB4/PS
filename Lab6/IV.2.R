
p0 <- 0.10  
n <- 150  
x <- 20 

phat <- x / n

z <- (phat - p0) / sqrt(p0 * (1 - p0) / n)

p_value <- 1 - pnorm(z)

alpha <- 0.05

cat("Proporția observată:", round(phat, 4), "\n")
cat("Statistica z:", round(z, 4), "\n")
cat("Valoarea p:", round(p_value, 4), "\n")
cat("Nivel de semnificație:", alpha, "\n")

if (p_value < alpha) {
  cat("Respingem ipoteza nulă: Procentul componentelor defecte este mai mare de 10%.\n")
} else {
  cat("Nu putem respinge ipoteza nulă: Nu există suficiente dovezi că procentul componentelor defecte este mai mare de 10%.\n")
}
