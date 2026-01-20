# Funkcja do wnioskowania na podstawie p-value

wniosek <- function(p){
  if(p > 0.05){
    return("Nie odrzucamy H0 (p > 0.05): brak dowodów przeciwko H0")
  } else if(p > 0.01){
    return("Odrzucamy H0 na poziomie 5% (0.01 < p < 0.05)")
  } else if(p > 0.001){
    return("Odrzucamy H0 na poziomie 1% (0.001 < p < 0.01)")
  } else {
    return("Odrzucamy H0 na poziomie 0.1% (p < 0.001)")
  }
}

# 1a) Test równości proporcji – Test Z

# Dane
n_k <- 520
n_m <- 480
x_k <- 220
x_m <- 165

# Hipotezy:
# H0: p_k = p_m (ukończenie studiów nie zależy od płci)
# H1: p_k ≠ p_m

p_k <- x_k / n_k
p_m <- x_m / n_m

p_hat <- (x_k + x_m) / (n_k + n_m)
SE <- sqrt(p_hat * (1 - p_hat) * (1/n_k + 1/n_m))

Z <- (p_k - p_m) / SE
p_value_Z <- 2 * (1 - pnorm(abs(Z)))

Z
p_value_Z
# Wniosek:
wniosek(p_value_Z)

# 1a) Test równości proporcji – prop.test

# H0: p_k = p_m
# H1: p_k ≠ p_m

prop_test <- prop.test(c(x_k, x_m), c(n_k, n_m), correct = FALSE)

prop_test
# Wniosek:
wniosek(prop_test$p.value)

# 1b) Tablica rozdzielcza 2x2

# 0 – brak wyższego wykształcenia
# 1 – wyższe wykształcenie

tab <- matrix(
  c(
    x_k, n_k - x_k,
    x_m, n_m - x_m
  ),
  nrow = 2,
  byrow = TRUE
)

rownames(tab) <- c("Kobiety", "Mezczyzni")
colnames(tab) <- c("Wykszt_1", "Wykszt_0")

tab

# 1c) Test niezależności chi-kwadrat

# H0: Wykształcenie jest niezależne od płci
# H1: Wykształcenie zależy od płci

chi <- chisq.test(tab, correct = FALSE)

chi
# Wniosek:
wniosek(chi$p.value)

# 1c) Test dokładny Fishera

# H0: Wykształcenie jest niezależne od płci
# H1: Wykształcenie zależy od płci

fisher <- fisher.test(tab)

fisher
# Wniosek:
wniosek(fisher$p.value)

# 1d) Test Z dla średnich (znane wariancje)

# Dane
mean_k <- 166
mean_m <- 174
var_k <- 100
var_m <- 121

# Hipotezy:
# H0: μ_k = μ_m (średni wzrost nie zależy od płci)
# H1: μ_k ≠ μ_m

SE_mean <- sqrt(var_k/n_k + var_m/n_m)
Z_mean <- (mean_k - mean_m) / SE_mean
p_value_mean <- 2 * (1 - pnorm(abs(Z_mean)))

Z_mean
p_value_mean
# Wniosek:
wniosek(p_value_mean)
