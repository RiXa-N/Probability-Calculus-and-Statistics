# Zad.1

# a) 5000 z U[0,1]
set.seed(12345)

u <- runif(5000, min=0, max=1)

png("hist_uniform.png", width = 800, height = 600)     # zapis histogramu do pliku PNG
hist(u, breaks=30, col="red", prob=TRUE,main="Histogram - U[0,1], n=5000", xlab="Wartość") # prob=TRUE → histogram gęstości

dens_u <- density(u) # estymator jądrowy gęstości

png("kde_uniform.png", width = 800, height = 600)
plot(dens_u, col="red", main="Estymator jądrowy - U[0,1], n=5000", xlab="Wartość")

# b) 3000 z N(100,15)
set.seed(12345)

n <- rnorm(3000, mean=100, sd=15)

png("hist_normal.png", width = 800, height = 600)
hist(n, breaks=30, col="orange", prob=TRUE, main="Histogram - N(100,15), n=3000", xlab="Wartość")

dens_n <- density(n)

png("kde_normal.png", width = 800, height = 600)
plot(dens_n, col="orange", main="Estymator jądrowy - N(100,15), n=3000", xlab="Wartość")

# Zad.2
cat("Zad.2 \n")

# a) i)
set.seed(123)

u <- runif(600)                     # 600 realizacji U[0,1]
rzuty <- ceiling(6 * u)            # X = ceil(6u): generowanie liczb 1–6

head(rzuty, 5)
cat("\n")

# a) ii)
srednia <- mean(rzuty)  # średnia z próby
wariancja <- var(rzuty) # wariancja z próby

srednia
wariancja
cat("\n")

# a) iii)
czestosci <- table(rzuty)
czestosci
cat("\n")

# a) iv)
ramka <- as.data.frame(czestosci)
ramka
var_czestosci <- var(ramka$Freq)   # wariancja liczności kategorii
var_czestosci
cat("\n")

# b)
set.seed(123)

rzuty_sample <- sample(1:6, size = 600, replace = TRUE)

head(rzuty_sample)
table(rzuty_sample)
mean(rzuty_sample)
var(rzuty_sample)

# Zad.3
cat("\nZad.3 \n")
set.seed(123)

# wartości i ich prawdopodobieństwa
k <- c(0, 1, 2, 3)
p <- c(0.15, 0.25, 0.50, 0.10)

# sort mal prawdopodobieństwa dla optymalki
ord <- order(p, decreasing = TRUE)
k_sorted <- k[ord]
p_sorted <- p[ord]

# prawdopodobieństwa skumulowane
s <- cumsum(p_sorted)

u <- runif(1000)                   # 1000 realizacji U[0,1]
X <- numeric(1000)                 # wektor wyników

# generujemy X według reguły:
#    X = k_i   gdy   s_(i-1) <= u < s_i
X <- numeric(1000)

for (i in 1:1000) {
  ui <- u[i]
  j <- 1
  while (ui > s[j]) {
    j <- j + 1
  }
  X[i] <- k_sorted[j]
}

head(X)
table(X)

# Zad.4
cat("\nZad.4\n")

# i)
set.seed(123)

n <- 10
p <- 0.3

binom_sample <- numeric(100)

for (i in 1:100) {
  u <- runif(n)                    # 10 prób jednorazowych
  sukcesy <- sum(u < p)            # sukces gdy U < p
  binom_sample[i] <- sukcesy
}

head(binom_sample)
table(binom_sample)
cat("\n")

# ii)
set.seed(123)
p <- 0.4
geom_sample <- numeric(50)

for (i in 1:50) {
  licznik <- 1
  u <- runif(1)
  while (u >= p) {                 # powtarzamy aż trafimy sukces
    licznik <- licznik + 1
    u <- runif(1)
  }
  geom_sample[i] <- licznik
}

head(geom_sample)
table(geom_sample)

# Zad.5
cat("\nZad.5\n")

# a) i)
losuj_odwrotnie <- function(n) {
  u <- runif(n)
  x <- 2 * sqrt(u)                 # odwrotna dystrybuanta F^{-1}(u)
  return(x)
}

# a) ii)
set.seed(123)

realizacje_odwrotnie <- losuj_odwrotnie(200)

png("hist_wisualise.png", width = 800, height = 600)
hist(realizacje_odwrotnie, breaks=20, col="blue", main="Histogram - metoda odwracania dystrybuanty", xlab="x")

# b) i)
losuj_przyjecie_odrzucenie <- function(n) {
  x <- numeric(n)
  i <- 1
  while(i <= n) {
    y <- runif(1, 0, 2)            # kandydat z rozkładu pomocniczego
    u <- runif(1)
    if(u <= (0.5*y)) {             # reguła przyjęcia: g(y)/(M*h(y)) = 0.5*y
      x[i] <- y
      i <- i + 1
    }
  }
  return(x)
}

# b) ii)
set.seed(as.numeric(Sys.time()))

realizacje_przyjecie <- losuj_przyjecie_odrzucenie(200)

png("hist_wisualise_2.png", width = 800, height = 600)
hist(realizacje_przyjecie, breaks=20, col="green", main="Histogram - metoda przyjęcia i odrzucenia", xlab="x")
