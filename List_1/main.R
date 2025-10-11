#Zadanie 1
# Tworzenie wektorów
# a
a <- c(1, 4, 6, 13, -10, 8)

# b sekwencja od 1 do 101 co 2
b <- seq(1,101, by=2)

# c - 4x3, 7x3, 9x3
c <- c(rep(4,3), rep(7,3), rep(9,3))

# d
d <-c("czy", "to", "jest", "wektor", NA)

# e
e <- c("czy", "to", "jest", "wektor", "NA")

# f (4,7,9)x6
f <- rep(c(4,7,9), times=6)

# Funkcja pomocnicza dla infa
info <- function(x, name) {
  cat("\nWektor", name, "\n")
  print(x)
  cat("Długość:", length(x), "\n")
  cat("Typ danych:", typeof(x), "\n")

  if (is.numeric(x)) {
    cat("Min:", min(x), "\n")
    cat("Max:", max(x), "\n")
    cat("Sum:", sum(x), "\n")
  } else {
    cat("Nie można obliczyć min, max, sum — dla wektorow nienumerycznych\n")
  }
}

# podpunkt a
info(a, "a")
info(b, "b")
info(c, "c")
info(d, "d")
info(e, "e")
info(f, "f")

# sortowanie decreasing = false - rosnąco na.last = true - na jest ostatnie
sort_d <- sort(d, decreasing = FALSE, na.last = TRUE)
sort_e <- sort(e, decreasing = FALSE, na.last = TRUE)

# podpunkt b
cat("\nPodpunkt b\n")
cat("Wektor d przed sortem:", d,"\n")
cat("Po sortie:", sort_d, "\n\n")

cat("Wektor e przed sortem:", e, "\n")
cat("Po sortie:", sort_e, "\n")

# podpunkt c.1
a_sum_f <- a+f
cat("\na+f=", a_sum_f, "\n")

#podpunkt c.2
a_mult_f <- a*f
cat("\na*f=", a_mult_f, "\n")

#podpunkt c.3
a_sum_c <- a+c
cat("\na+c=", a_sum_c, "\n")

#podpunkt c.4
a_sum_10 <- a+10
cat("\na+10=", a_sum_10, "\n")

#podpunkt c.5
a_mult_15 <- a*15
cat("\n15a=", a_mult_15, "\n")

#podpunkt c.6
cat("\n26 element wektora b:", b[26], "\n")

#podpunkt c.7
cat("\nelementy 6-10 wektora f:", f[6:10], "\n")

#podpunkt d
b_more_50 <- b[b>50]
cat("\nelementy w wektorze b >50:", b_more_50, "\n")
cat("jest ich:", length(b_more_50), "\n")