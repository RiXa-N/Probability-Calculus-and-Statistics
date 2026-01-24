dane <- read.csv("bakteria.csv", header = TRUE, sep = ";",)

dane$masa <- as.numeric(gsub(",", ".", dane$masa))

dane <- na.omit(dane)

czas <- dane$czas
masa <- dane$masa

# Model liniowy log(masa) ~ czas

model_log <- lm(log(masa) ~ czas)

a <- coef(model_log)[1]
b <- coef(model_log)[2]

cat("=== Model liniowy log(masy) ===\n")
print(summary(model_log))

cat("\nPostać modelu wykładniczego:\n")
cat("masa(czas) =", exp(a), "* exp(", b, "* czas)\n")

# Wykres zapisany do PNG

png("wykres_bakterie.png", width = 800, height = 600)

plot(czas, masa,
     main = "Masa bakterii w funkcji czasu",
     xlab = "Czas",
     ylab = "Masa bakterii",
     pch = 19,
     col = "blue")

# Krzywa regresji wykładniczej
czas_teor <- seq(min(czas), max(czas), length.out = 200)
masa_teor <- exp(a) * exp(b * czas_teor)

lines(czas_teor, masa_teor, lwd = 2, col = "red")

legend("topleft", legend = c("Dane", "Regresja wykładnicza"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

