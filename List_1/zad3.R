# Tworzenie wektorów
# a
a <- seq(300, 0, by=-3)

# b 5 jest zamieniona na "5" bo w R wektor ma miec jednolity typ danych
b <- c("one", "two", "three", "four", 5)

# c
c <- c("one", "two", "three", "four", "5")

# d
d <- rep(c(3,1,6), times=4)

# e
e <- c(rep(3,4), rep(1,4), rep(6,4))

# f
f <- c(5, 1, 4, 7)

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

# podpunkt b
b_sorted <- sort(b)
e_sorted <- sort(e)

cat("\nPodpunkt b\n")
cat("Wektor b przed sortem:", b,"\n")
cat("Po sortie:", b_sorted, "\n\n")

cat("Wektor e przed sortem:", e, "\n")
cat("Po sortie:", e_sorted, "\n")

# podpunkt c.1
cat("\nPodpunkt c")
d_sum_f <- d+f
cat("\nd+f=", d_sum_f, "\n")

# podpunkt c.2
d_mult_skal_e <- sum(d*e)
cat("\nd∘e=", d_mult_skal_e, "\n")

# podpunkt c.3
cat("\n35 element wektora a to:", a[35], "\n")

# podpunkt c.4
cat("\nelementy od 67 do 85 wektora a to:", a[67:85], "\n")

# podpunkt c.5


# podpunkt c.6
a_less_100 <- a[a<100]
cat("\nelementy w wektorze a <100:", a_less_100, "\n")
cat("jest ich:", length(a_less_100), "\n")
