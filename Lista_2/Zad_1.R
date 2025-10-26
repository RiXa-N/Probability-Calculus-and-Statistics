# a
oceny <- data.frame(
  Imie = c("Krzysztof", "Maria", "Henryk", "Anna"),
  Plec = c("m", "k", "m", "k"),
  Analiza = c(3.5, 4.5, 5.0, 4.5),
  Algebra = c(4.0, 5.0, 4.0, 3.5)
)
print(oceny)
cat("\n")

# b
head(oceny, 2)
cat("\n")

# c
str(oceny)
cat("\n")

# d
srednia_analiza <- mean(oceny$Analiza)
print(srednia_analiza)
cat("\n")

# e
oceny$srednia <- (oceny$Analiza + oceny$Algebra)/2
print(oceny)
cat("\n")

# f
oceny_kobiety <- subset(oceny, Plec == "k")
print(oceny_kobiety)
cat("\n")

# g
oceny_4.5 <- subset(oceny, Analiza >= 4.5 | Algebra >= 4.5)
print(oceny_4.5)
cat("\n")

# h
liczba_osob_analiza_4.5 <- sum(oceny$Analiza >= 4.5)
cat(liczba_osob_analiza_4.5, "\n")