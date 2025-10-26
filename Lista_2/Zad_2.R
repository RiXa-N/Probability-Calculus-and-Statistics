# a
waga_1 <- read.csv("waga1.csv", header=TRUE,sep = ";")

# b
head(waga_1, 5)
cat("\n")

# c
str(waga_1)
cat("\n")

# d
sredni_wzrost <- mean(waga_1$Wzrost)
srednia_waga_przed <- mean(waga_1$Waga_przed)
cat(sredni_wzrost,"\n", srednia_waga_przed, "\n", "\n")

# e
waga_1$wskaznik_wagi <- (waga_1$Waga_przed/((waga_1$Wzrost/100)^2))
head(waga_1, 10)
cat("\n")

# ev2
grube_kobiety <- subset(waga_1, plec == 1 & wskaznik_wagi > 25)
print(grube_kobiety)
cat("\n")

# f
menzczyzni <- subset(waga_1, plec == 0)
print(menzczyzni)
cat("\n")

# g
ile_wysokich <- sum(waga_1$Wzrost > 175)
cat(ile_wysokich, "\n")