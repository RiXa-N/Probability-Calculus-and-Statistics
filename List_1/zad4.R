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

# test
