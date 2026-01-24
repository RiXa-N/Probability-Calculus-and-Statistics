set.seed(123)  # dla powtarzalności wyników
alpha <- 0.05

# a) Próba z rozkładu wykładniczego (E(X)=1, lambda=1)
proba_exp <- rexp(1000, rate = 1)


# b) Test KS dla próby wykładniczej

# i) Test KS: normalny
ks_exp_norm <- ks.test(proba_exp, "pnorm", mean = 1, sd = 1)

# ii) Test KS: wykładniczy
ks_exp_exp <- ks.test(proba_exp, "pexp", rate = 1)

cat("Próba z rozkładu wykładniczego\n")
cat("p-value =", ks_exp_norm$p.value, "\n")
if (ks_exp_norm$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0 (to nie jest rozkład normalny).\n\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0.\n\n")
}

cat("p-value =", ks_exp_exp$p.value, "\n")
if (ks_exp_exp$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0.\n\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0 (zgodne z rozkładem wykładniczym).\n\n")
}

# c) Próba z rozkładu Gamma (kształt=100, skala=1)
proba_gamma <- rgamma(1000, shape = 100, scale = 1)

# d) Test KS dla próby Gamma

# i) Test KS: normalny
ks_gamma_norm <- ks.test(proba_gamma, "pnorm", mean = 100, sd = 10)

# ii) Test KS: gamma
ks_gamma_gamma <- ks.test(proba_gamma, "pgamma", shape = 100, scale = 1)

cat("Próba z rozkładu Gamma\n")
cat("p-value =", ks_gamma_norm$p.value, "\n")
if (ks_gamma_norm$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0.\n\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0 (efekt CTG).\n\n")
}

cat("p-value =", ks_gamma_gamma$p.value, "\n")
if (ks_gamma_gamma$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0.\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0 (zgodne z rozkładem Gamma).\n")
}