# Tworzenie macierzy
# A
A <- rbind(c(-3,1,-2), c(4,-5,3))

# B
B <- cbind(c(1,3,5), c(2,4,6))

# C
C <- matrix(c(7,-3,-2,1),2,byrow = TRUE)

# D
D <- rbind(c(1,2,4), c(3,5,7), c(2,3,2))

# podpunkt a
# A + B nie możliwe bo rózne wymiary macierzy
At_sum_B <- t(A)+B
BA <- B%*%A
BB <- B*B
C_inv <- solve(C)
CC_inv <- C%*%solve(C)

cat("\nA+B nie istnieje bo różne wymiary macierzy\n", "\nA+Bt=")
At_sum_B
cat("\nB*A=")
BA
cat("\nB*B=")
BB
cat("\nC^-1=")
C_inv
cat("\nC*C^-1=")
round(CC_inv,14)

#podpunkt b
X_1 = B %*% solve(C)
X_2 = solve(D) %*% B
cat("\nX dla równania XC=B to:\n")
X_1
cat("\nX dla równania DX=B to:\n")
X_2