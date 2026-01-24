wyniki <- c(1, 2, 3, 4, 5, 6)
frekwencje <- c(171, 200, 168, 213, 226, 222)

n <- sum(frekwencje)

# a) Oczekiwane frekwencje
oczekiwane <- rep(n / 6, 6)

tabela <- data.frame(
  Wynik = wyniki,
  Obserwowane = frekwencje,
  Oczekiwane = oczekiwane
)
print(tabela)

# b) Statystyka testowa chi-kwadrat
chi2 <- sum((frekwencje - oczekiwane)^2 / oczekiwane)
cat("\nStatystyka chi-kwadrat =", chi2, "\n")

# c) Wartość p
df <- length(frekwencje) - 1
p_value <- 1 - pchisq(chi2, df)
cat("p-value =", p_value, "\n")

# d) Wniosek testu (alpha = 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Wniosek: Odrzucamy hipotezę zerową – kostka nie jest symetryczna.\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia hipotezy zerowej – kostka może być symetryczna.\n")
}

# e) Test chi-kwadrat
test <- chisq.test(frekwencje, p = rep(1/6, 6))
cat("\n\n")
print(test)