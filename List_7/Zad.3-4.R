alpha <- 0.05

dane <- read.csv("mieszkania.csv", header = TRUE, sep = ";")

metraz <- dane$Metraz
cena <- dane$Cena

# Zadanie 3 – testy normalności

# 3a) Test normalności ceny (Shapiro–Wilk)
shapiro_cena <- shapiro.test(cena)

cat("Test Shapiro-Wilka: Cena \n")
print(shapiro_cena)
if (shapiro_cena$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0 – cena nie ma rozkładu normalnego.\n\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0 – cena ma rozkład normalny.\n\n")
}

# 3b) Test normalności metrażu (Shapiro–Wilk)
shapiro_metraz <- shapiro.test(metraz)

cat("Test Shapiro-Wilka: Metraż\n")
print(shapiro_metraz)
if (shapiro_metraz$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0 – metraż nie ma rozkładu normalnego.\n\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0 – metraż ma rozkład normalny.\n\n")
}

# Zadanie 4 – regresja liniowa

# 4a) Regresja liniowa: Cena ~ Metraz
model <- lm(Cena ~ Metraz, data = dane)

cat("Podsumowanie regresji liniowej\n")
print(summary(model))

# 4b) Wykres rozrzutu + linia regresji (PNG)

png("wykres_cena_metraz.png", width = 800, height = 600)

plot(metraz, cena,
     main = "Cena mieszkania względem metrażu",
     xlab = "Metraż [m2]",
     ylab = "Cena",
     pch = 19,
     col = "blue")

abline(model, lwd = 2, col = "red")

legend("topleft", legend = c("Dane", "Regresja liniowa"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))


# 4c) Test normalności reszt (Shapiro–Wilk)

reszty <- residuals(model)
shapiro_reszty <- shapiro.test(reszty)

cat("\nTest Shapiro-Wilka: reszty modelu\n")
print(shapiro_reszty)
if (shapiro_reszty$p.value < alpha) {
  cat("Wniosek: Odrzucamy H0 – reszty nie mają rozkładu normalnego.\n\n")
} else {
  cat("Wniosek: Brak podstaw do odrzucenia H0 – reszty mają rozkład normalny.\n\n")
}

# 4d) Estymacja średniej ceny mieszkania o metrażu 80 m2

nowe_dane <- data.frame(Metraz = 80)
predykcja <- predict(model, newdata = nowe_dane)

cat("Estymowana średnia cena mieszkania (80 m2)\n")
cat("Cena =", predykcja, "\n")
