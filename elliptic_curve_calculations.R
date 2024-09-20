library(numbers)
library(polynom)

# Student Number Calculation (Simplified)
S1 <- 3
S2 <- 9
S3 <- 7
S4 <- 3
S5 <- 7
S6 <- 1
S7 <- 5

# Calculating A and B using the given formula
A <- (((S1 + S2 + S3) * 7) + ((S4 + S5 + S6) * 6) + (S7 * 5)) %% 23
B <- (((S1 + S3 + S5 + S7) * 3) + ((S2 + S4 + S6) * 9)) %% 23

# Display the values of A and B
cat(paste("A = ", A, "\nB = ", B, "\n"))

# Defining modulus
p <- 23

# Function to check non-singularity of an elliptic curve
check_ns <- function(A, B, p) {
    discriminant <- ((4 * A^3 + 27 * B^2) %% p)
    return(discriminant != 0)
}

# Check non-singularity for calculated A and B
non_singular <- check_ns(A, B, p)
cat(paste("The curve is non-singular: ", non_singular, "\n"))

# Function to compute points on the elliptic curve
elliptic_curve_points <- function(A, B, p) {
    points <- list()
    for (x in 0:(p-1)) {
        rhs <- (x^3 + A*x + B) %% p
        for (y in 0:(p-1)) {
            lhs <- (y^2) %% p
            if (lhs == rhs) {
                points <- append(points, list(c(x, y)))
            }
        }
    }
    return(points)
}

# Calculate points on the elliptic curve
points <- elliptic_curve_points(A, B, p)

# Convert points to data frame for display
points_df <- do.call(rbind, points)
colnames(points_df) <- c("x", "y")
print(points_df)

# Calculate the order of the group (number of points + point at infinity)
order_of_group <- length(points) + 1
cat(paste("Order of the group: ", order_of_group, "\n"))
