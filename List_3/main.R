# Zad.1
dbinom(5, size = 6, prob = 0.5) #rozklad dwumianowy prawdopodobienstwo punktowe

1 - pbinom(2, size = 6, prob = 0.5) # prawdopodobienstwo skumulowane (dystrybuanta)

pbinom(4, size = 6, prob = 0.5) - pbinom(1, size = 6, prob = 0.5)
cat("\n")

png("rozkład_reszek.png", width = 600, height = 400) # ustawienie typu nazwy i rozmiaru wykresu
x <- 0:6                     # możliwe X
p <- dbinom(x, size = 6, prob = 0.5)
plot(x, p, type = "h", lwd = 3, col = "red",
     main = "Rozkład liczby reszek przy 6 rzutach monetą",
     xlab = "Liczba reszek (X)", ylab = "P(X = x)")

# Zad.2
lambda <- 2*3

p_eq5 <- dpois(5, lambda = lambda) # rozklad poisona prawdopodobienstwo punkctowe

p_ge4  <- 1 - ppois(3, lambda = lambda) # prawdopodobieństwo skumulowane

p_3to5 <- ppois(5, lambda = lambda) - ppois(2, lambda = lambda)

cat("P(X = 5)        =", p_eq5, "\n")
cat("P(X >= 4)       =", p_ge4, "\n")
cat("P(3 <= X <= 5)  =", p_3to5, "\n\n")

png("rozkład_poisson_lambda6.png", width = 800, height = 500)
x <- 0:30
p <- dpois(x, lambda = lambda)
plot(x, p, type = "h", lwd = 3,
     main = "Rozkład Poissona: λ = 6 (liczba sprzedanych aut w 2 tyg.)",
     xlab = "Liczba sprzedanych samochodów (X)",
     ylab = "P(X = x)")
points(x, p, pch = 16)        # kropki na końcach słupków
text(x, p, labels = round(p, 3), pos = 3, cex = 0.7) # wartosci nad slupkami

# Zad.3
a <- 4
b <- 12

punif(7, min=a, max=b) #rozklad jednostajny

punif(11, min=a, max=b) - punif(5, min=a, max=b)

1 - punif(10, min=a, max=b)

qnorm_ <- function(p) a + p*(b-a)  # kwantyl rozkładu jednostajnego
x <- a + 0.4*(b-a)
x
cat("\n")

# Zad.4
lambda <- 4

sec_to_min <- function(s) s/60

p_i <- exp(-lambda * sec_to_min(30))

p_ii <- 1 - exp(-lambda * sec_to_min(20))

p_iii <- exp(-lambda * sec_to_min(40)) - exp(-lambda * sec_to_min(80))

t_iv <- -log(0.2)/lambda

cat("P(T > 30s) =", round(p_i, 4), "\n")
cat("P(T < 20s) =", round(p_ii, 4), "\n")
cat("P(40s < T < 80s) =", round(p_iii, 4), "\n")
cat("t takie, że P(T>t)=0.2:", round(t_iv, 3), "min (≈", round(t_iv*60,1), "s )\n\n")

t <- seq(0, 3, by=0.01)
g <- dexp(t, rate=lambda)

png("wykres_wykładniczy.png", width=800, height=500)
plot(t, g, type="l", lwd=2, col="blue",
     main="Gęstość rozkładu wykładniczego (λ = 4/min)",
     xlab="Czas między telefonami (minuty)",
     ylab="g(t)")

# Zad.5

1 - pnorm(180, mean=170, sd=12) # Funkcja dystrybuanty rozkładu normalnego

pnorm(165, mean=170, sd=12)

pnorm(190, 170, 12) - pnorm(155, 170, 12)

qnorm(0.9, mean=170, sd=12) # Funkcja kwantylowa rozkładu normalnego

x <- seq(130, 210, by=0.1)
y <- dnorm(x, mean=170, sd=12)
plot(x, y, type="l", lwd=2, col="blue",
     main="Rozkład normalny wzrostu studentów",
     xlab="Wzrost (cm)", ylab="gęstość")

x_fill <- seq(180, 210, by=0.1)
polygon(c(x_fill, rev(x_fill)), c(dnorm(x_fill,170,12), rep(0,length(x_fill))),
        col=rgb(1,0,0,0.3), border=NA)
cat("\n")

# Zad.6

n <- 180
p <- 1/6

p_i <- dbinom(27, n, p) # prawdopodobieństwo punktowe

p_ii <- 1 - pbinom(31, n, p) # dystrybuanta

p_iii <- pbinom(28, n, p)

p_iv <- pbinom(33, n, p) - pbinom(24, n, p)

cat("P(X=27) =", round(p_i,4), "\n")
cat("P(X>=32) =", round(p_ii,4), "\n")
cat("P(X<29) =", round(p_iii,4), "\n")
cat("P(25<=X<=33) =", round(p_iv,4), "\n")