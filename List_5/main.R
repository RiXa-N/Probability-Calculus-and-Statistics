n  <- 100
xbar <- 109          # średnia IQ
s2 <- 225            # wariancja
s  <- sqrt(s2)       # odchylenie standardowe
x_success <- 30      # liczba osób z IQ>115
p_hat <- x_success/n # estymator proporcji

cat("Zad.1\n")
#a) Test hipotezy, że p = 0.35 (proporcja IQ>115)
cat("a)\n")
# Hipotezy:
# H0: p = 0.35
# H1: p ≠ 0.35
cat("i)\n")

# i) test Z
p0 <- 0.35
SE_p <- sqrt(p0*(1-p0)/n)
Z_stat <- (p_hat - p0) / SE_p
p_val_Z <- 2*(1 - pnorm(abs(Z_stat)))

Z_stat
p_val_Z

# Wniosek:
# Jeśli p_val_Z > 0.05 → brak podstaw do odrzucenia H0
# Tutaj p ≈ 0.29, więc nie odrzucamy H0
# Nie mamy dowodów, że prawdziwa proporcja różni się od 35%
cat("ii)\n")

#ii)
prop_test_result <- prop.test(x_success, n, p = 0.35)

prop_test_result

# Wniosek:
# prop.test daje p około 0.29, czyli podobnie jak test Z
# p > 0.05 → nie odrzucamy H0
# Brak dowodów, że p ≠ 0.35
cat("b)\n")

# b) 99% przedział ufności dla proporcji
cat("i)\n")

# i)
z_crit <- qnorm(1 - 0.01/2)
CI_lower_norm <- p_hat - z_crit*sqrt(p_hat*(1-p_hat)/n)
CI_upper_norm <- p_hat + z_crit*sqrt(p_hat*(1-p_hat)/n)

c(CI_lower_norm, CI_upper_norm)

# Wniosek:
# To podstawowy przedział Wald'a
# Wynik zbliżony do (0.18, 0.42)
cat("ii)\n")

# ii)
prop_ci_result <- prop.test(x_success, n, conf.level = 0.99)

prop_ci_result$conf.int

# Wniosek:
# Wilson CI jest dokładniejszy
# zwykle nieco szerszy i bardziej stabilny
# Otrzymujemy około (0.20, 0.43)
cat("c)\n")

# c) 90% CI dla średniego IQ — test Z

# Hipotezy nie dotyczą ci, wiec obliczamy CI
SE_mean <- s / sqrt(n)
z_crit_90 <- qnorm(1 - 0.10/2)

CI_lower_Z <- xbar - z_crit_90 * SE_mean
CI_upper_Z <- xbar + z_crit_90 * SE_mean
c(CI_lower_Z, CI_upper_Z)

# Wniosek:
# Przedział Z → ok. (106.53 , 111.47)
cat("d)\n")

#d) 90% CI dla średniego IQ — test t (df = 99)

t_crit_90 <- qt(1 - 0.10/2, df = n-1)

CI_lower_t <- xbar - t_crit_90 * SE_mean
CI_upper_t <- xbar + t_crit_90 * SE_mean
c(CI_lower_t, CI_upper_t)

# Wniosek:
# Przedział t jest minimalnie szerszy niż Z
# Wynik ok. (106.51 , 111.49)
cat("e)\n")

#e) Test hipotezy: średni IQ = 115

#Hipotezy:
# H0: μ = 115   (średni IQ wynosi 115)
# H1: μ ≠ 115  (średni IQ różni się od 115)

mu0 <- 115
cat("i)\n")

# i)
Z_stat_mu <- (xbar - mu0) / SE_mean
p_val_mu_Z <- 2*(1 - pnorm(abs(Z_stat_mu)))

Z_stat_mu
p_val_mu_Z

# Wniosek:
# p < 0.001 → bardzo silne dowody przeciw H0
# Odrzucamy H0
# Średni IQ jest istotnie różny (i niższy) niż 115
cat("ii)\n")

# ii)
t_stat_mu <- (xbar - mu0) / SE_mean
p_val_mu_t <- 2*(1 - pt(abs(t_stat_mu), df = n-1))

t_stat_mu
p_val_mu_t

# Wniosek:
# Wyniki t-testu bardzo podobne do testu Z
# p < 0.001 → odrzucamy H0 i przyjmujemy H1
# Średni IQ w próbie jest istotnie mniejszy niż 115
cat("\n")



# Wczytanie danych

dane <- read.csv("waga1.csv", sep = ";")

# 2) Test hipotezy: średni wzrost = 170 cm

# H0: μ = 170  (średni wzrost studentów = 170 cm)
# H1: μ ≠ 170  (średni wzrost różny od 170 cm)
cat("Zad.2")

test_wzrost_170 <- t.test(dane$Wzrost, mu = 170)
test_wzrost_170

cat("\n")
# Wniosek:
# Jeżeli p-value > 0.05 → nie odrzucamy H0
# Jeżeli p-value < 0.05 → odrzucamy H0 i przyjmujemy, że średni wzrost ≠ 170

# 3) 90% przedział ufności dla średniego wzrostu
cat("Zad.3\n")

ci90 <- t.test(dane$Wzrost, conf.level = 0.90)
ci90$conf.int

cat("\n")
# Wniosek:
# Otrzymujemy 90% CI, który określa możliwy przedział wartości średniego wzrostu

# 4) Test hipotezy: średni wzrost studentek = 160 cm
cat("Zad.4")

studentki <- dane$Wzrost[dane$plec == 1]

# H0: μ = 160  (średni wzrost studentek = 160 cm)
# H1: μ ≠ 160

test_studentki_160 <- t.test(studentki, mu = 160)
test_studentki_160

cat("\n")
# Wniosek:
# Jeżeli p < 0.05 → studentki mają średni wzrost różny od 160 cm

# 5) 98% CI dla średniego wzrostu studentek
cat("Zad.5\n")

ci98_studentki <- t.test(studentki, conf.level = 0.98)
ci98_studentki$conf.int

cat("\n")
# Wniosek:
# 98% CI daje szeroki przedział — większa pewność oznacza większą szerokość

# 6) Test proporcji studentów (mężczyzn) wyższych niż 180 cm
cat("Zad.6")

mezczyzni <- dane[dane$plec == 0, ]

# liczba sukcesów = >180 cm
x_sukces <- sum(mezczyzni$Wzrost > 180)
n_m <- nrow(mezczyzni)

# H0: p = 0.25
# H1: p ≠ 0.25

test_prop_25 <- prop.test(x_sukces, n_m, p = 0.25)
test_prop_25

cat("\n")
# Wniosek:
# p-value < 0.05 → odrzucamy H0 i proporcja różni się od 25%
# p-value > 0.05 → brak podstaw do odrzucenia H0


# 7) 96% CI dla proporcji mężczyzn >180 cm
cat("Zad.7\n")

ci96_prop <- prop.test(x_sukces, n_m, conf.level = 0.96)
ci96_prop$conf.int

# Wniosek:
# Otrzymujemy Wilson CI, który jest stabilniejszy niż przybliżenie normalne