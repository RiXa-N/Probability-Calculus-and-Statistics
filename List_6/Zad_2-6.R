# Wczytanie danych
dane <- read.csv("waga1.csv",sep = ";", header = TRUE)


# Zad.2

# Utworzenie zmiennej: przyrost masy ciała
dane$przyrost <- dane$Waga_po - dane$Waga_przed

head(dane)

# Hipotezy:
# H0: μ = 2  (średni przyrost masy ciała = 2 kg)
# H1: μ ≠ 2  (średni przyrost masy ciała ≠ 2 kg)

# Test t-Studenta dla jednej próby
test_t <- t.test(dane$przyrost, mu = 2)

test_t

# Wniosek (na podstawie p-value):

p_value <- test_t$p.value

# p-value = 0.01889
# 0.01 < p < 0.05
# Odrzucamy hipotezę zerową H0 na poziomie istotności 5%
# Mamy dowody przeciwko H0
# Średni przyrost masy ciała studentów różni się istotnie od 2 kg
# i wynosi około 2.68 kg


# Zad.3

# zmienna binarna (>70 kg po studiach)

dane$waga70 <- ifelse(dane$Waga_po > 70, 1, 0)

# tablica rozdzielcza 2x2

tab70 <- table(dane$plec, dane$waga70)

# Opisanie wierszy i kolumn
rownames(tab70) <- c("Mezczyzni", "Kobiety")
colnames(tab70) <- c("<=70kg", ">70kg")

tab70

# Hipotezy:
# H0: p_k = p_m
#     (proporcja osób ważących >70 kg po studiach
#      jest taka sama u kobiet i mężczyzn)
# H1: p_k ≠ p_m

# test chi-kwadrat niezależności

chi70 <- chisq.test(tab70, correct = FALSE)

chi70

# Wniosek:
p_value <- chi70$p.value

# test proporcji – prop.test

# Liczba osób >70 kg w każdej grupie
x <- c(
  tab70["Kobiety", ">70kg"],
  tab70["Mezczyzni", ">70kg"]
)

# Liczebności grup
n <- c(
  sum(tab70["Kobiety", ]),
  sum(tab70["Mezczyzni", ])
)

prop70 <- prop.test(x, n, correct = FALSE)

prop70

# Wniosek:
p_value <- prop70$p.value

# p-value = 0.01801
# 0.01 < p < 0.05
# Odrzucamy hipotezę zerową H0 na poziomie istotności 5%
# Mamy dowody przeciwko H0
# Proporcja osób ważących więcej niż 70 kg po studiach
# istotnie różni się między kobietami 0.17 i mężczyznami 0.38


# Zad.4

# Podział na grupy

wzrost_m <- dane$Wzrost[dane$plec == 0]  # mężczyźni
wzrost_k <- dane$Wzrost[dane$plec == 1]  # kobiety

# Hipotezy:
# H0: μ_m − μ_k = 5
#     (mężczyźni są średnio o 5 cm wyżsi niż kobiety)
# H1: μ_m − μ_k ≠ 5

# Test t-Studenta dla dwóch prób niezależnych
# (bez założenia równości wariancji – test Welcha)

test_t_wzrost <- t.test(
  wzrost_m,
  wzrost_k,
  mu = 5,
  alternative = "two.sided"
)

test_t_wzrost

# Wniosek na podstawie p-value:

p_value <- test_t_wzrost$p.value

# p-value = 0.0030
# 0.001 < p < 0.01
# Odrzucamy hipotezę zerową H0 na poziomie istotności 1%
# Mamy mocne dowody przeciwko H0
# Średnia różnica wzrostu między mężczyznami i kobietami
# istotnie różni się od 5 cm i wynosi około 12.5 cm


# Zad.5

# Test hipotezy: czy 80% studentów przybrało na wadze

# Utworzenie zmiennej binarnej: 1 jeśli przyrost > 0, 0 w przeciwnym wypadku
dane$przybral <- ifelse(dane$Waga_po - dane$Waga_przed > 0, 1, 0)

# Sprawdzenie liczby studentów, którzy przybrali
table(dane$przybral)

# Hipotezy:
# H0: p = 0.8  (80% studentów przybrało na wadze)
# H1: p ≠ 0.8  (proporcja różni się od 80%)

# Liczba osób, które przybrały
x <- sum(dane$przybral)

# Liczebność próby
n <- nrow(dane)

# Test proporcji jednej próby
test_prop <- prop.test(x, n, p = 0.8, correct = FALSE)

test_prop

# Wniosek na podstawie p-value

p_value <- test_prop$p.value

# p-value = 0.03937
# 0.01 < p < 0.05
# Odrzucamy hipotezę zerową H0 na poziomie istotności 5%
# Mamy dowody przeciwko H0
# Proporcja studentów przybierających na wadze
# istotnie różni się od 80% i wynosi około 71.7%


# Zad.6

# Test hipotezy: średni przyrost masy mężczyzn = 4 kg

# Wyodrębnienie grupy mężczyzn
waga_m <- dane$Waga_po[dane$plec == 0] - dane$Waga_przed[dane$plec == 0]

# Hipotezy:
# H0: μ = 4   (średni przyrost masy mężczyzn = 4 kg)
# H1: μ ≠ 4   (średni przyrost masy mężczyzn ≠ 4 kg)

# Test t-Studenta dla jednej próby
test_m <- t.test(waga_m, mu = 4)

test_m

# Wniosek na podstawie p-value

p_value <- test_m$p.value

# p-value = 0.00405
# 0.001 < p < 0.01
# Odrzucamy hipotezę zerową H0 na poziomie istotności 1%
# Mamy mocne dowody przeciwko H0
# Średni przyrost masy mężczyzn jest istotnie mniejszy niż 4 kg
# i wynosi około 2.81 kg