# Tworzenie macierzy
# A nrow-liczba wierszy byrow = True - zapełniać wierszami a nie kolumnami
A <- matrix(c(3,1,2,4,5,3), nrow=2, byrow = TRUE)

# B rbind - laczenie wierszy
B <- rbind(c(-1,2), c(3,-4), c(-5,6))

# C cbind - laczenie kolumn
C <-cbind(c(7,2), c(3,1))

# D
D <- rbind(c(1,2,4), c(3,5,7), c(5,7,11))

# podpunkt a
# A + B nie możliwe bo rózne wymiary macierzy
A_sum_Bt <- A + t(B) # t()- zamiana kolumn i wierszy miejscami
AB <- A%*%B # %*% - mnożenie macierzowe
AA <- A*A # nie można robić mnożenie macierzowe dla tej samej macierzy
D_inv <- solve(D) # solve() - inwersja macierzy
DD_inv <- D %*% D_inv
cat("\nA+B nie istnieje bo różne wymiary macierzy\n", "\nA+Bt=")
A_sum_Bt
cat("\nA*B=")
AB
cat("\nA*A=")
AA
cat("\nD^-1=")
D_inv
cat("\nD*D^-1=")
round(DD_inv,14)

# podpunkt b
X_1 <- solve(C) %*% A
X_2 <- A %*% solve(D) #możemy tak zrobić bo C i D są kwadratowe i odwracalne
cat("\nX dla równania CX=A to:\n")
X_1
cat("\nX dla równania XD=A to:\n")
X_2