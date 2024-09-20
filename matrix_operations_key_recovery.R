# Example plaintext and ciphertext bitstrings
plaintext_bits <- c(0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
                    0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 
                    0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0)

ciphertext_bits <- c(1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 
                     0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 
                     1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 
                     0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1)

# Convert the bitstrings into 8x8 matrices (blocks)
P_matrix <- matrix(plaintext_bits, nrow = 8, byrow = FALSE)
C_matrix <- matrix(ciphertext_bits, nrow = 8, byrow = FALSE)

# Display the plaintext and ciphertext matrices
cat("Plaintext Matrix:
")
print(P_matrix)

cat("Ciphertext Matrix:
")
print(C_matrix)

# Function to check if two columns differ by exactly one bit
differs_by_one_bit <- function(col1, col2) {
    return(sum(col1 != col2) == 1)
}

# Initialize the matrix to store the recovered key
G_recovered <- matrix(0, nrow = 8, ncol = 8)

# Recover each column of the key matrix by comparing the columns of plaintext and ciphertext
for (col in 1:ncol(P_matrix)) {
    for (col2 in 1:ncol(P_matrix)) {
        if (col != col2 && differs_by_one_bit(P_matrix[, col], P_matrix[, col2])) {
            # Add the columns mod 2 to find the row with a single differing bit
            diff_vec <- (P_matrix[, col] + P_matrix[, col2]) %% 2
            if (sum(diff_vec) == 1) {
                row_j <- which(diff_vec == 1)
                # Recover the jth column of the key
                G_recovered[, row_j] <- (C_matrix[, col] + C_matrix[, col2]) %% 2
            }
        }
    }
}

# Display the recovered key matrix
cat("Recovered Key Matrix G:
")
print(G_recovered)
