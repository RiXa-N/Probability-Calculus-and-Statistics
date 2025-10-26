library(dplyr)

# a
mieszkania <- read.csv("mieszkania.csv", header=TRUE,sep = ";")

# b
head(mieszkania, 6)
cat("\n")

# c
str(mieszkania)
cat("\n")

# d
sredni_metraz <- mean(mieszkania$Metraz)
srednia_cena <- mean(mieszkania$Cena)
cat(sredni_metraz, "\n", srednia_cena, "\n\n")

# e
mieszkania$Cena_za_m2 <- (mieszkania$Cena/mieszkania$Metraz)
head(mieszkania, 10)
cat("\n")

# f
Nie_drogie_Psie_Pole <- subset(mieszkania, Dzielnica == "Psie Pole" & Cena < 400000)
print(Nie_drogie_Psie_Pole)
cat("\n")

# g
Srodmiescie_duze <- subset(mieszkania, Dzielnica == "Srodmiescie" & Metraz > 60)
print(Srodmiescie_duze)
cat("\n")

# h
ile_duzych_i_tanich <- sum(mieszkania$Metraz > 60 & mieszkania$Cena < 350000)
cat(ile_duzych_i_tanich, "\n","\n")

# i
mieszkania$Sr_metr_pokoju <- (mieszkania$Metraz / mieszkania$Pokoje)
mieszkania$Stosunek <- (mieszkania$Sr_metr_pokoju / mieszkania$Cena)
najlepsze_mieszkanie <- mieszkania[which.max(mieszkania$Stosunek),]
najlepsze_mieszkanie[, c("Dzielnica", "Metraz", "Pokoje", "Cena", "Cena_za_m2","Stosunek")]
cat("\n")

# j
# %>% - przekazuje wynik jednej funkcji jako pierwszy argument kolejnej funkcji.
# group_by - grupuje dane według jednej lub kilku kolumn
# summarise - tworzy podsumowanie danych np. średnia, suma, liczba elementów itp. po group_by robi to dla każdej grupy osobno
# filter - wybiera tylko te wiersze, które spełniają określone warunki.
# nie mozna uzyc subset bo nie dziala z group_by
# sd - oblicza odchylenie standardowe
ceny_analiza <- mieszkania %>%
  group_by(Dzielnica) %>%
  summarise(
    srednia_cena_m2 = mean(Cena_za_m2),
    odchylenie_standardowe_cena_m2 = sd(Cena_za_m2)
  )
stabilne <- ceny_analiza %>% filter(odchylenie_standardowe_cena_m2 == min(odchylenie_standardowe_cena_m2))
niestabilne <- ceny_analiza %>% filter(odchylenie_standardowe_cena_m2== max(odchylenie_standardowe_cena_m2))

cat("Najbardziej stabilna dzielnica to: \n")
stabilne
cat("\nNajbardziej niestabilna dzielnica to: \n")
niestabilne