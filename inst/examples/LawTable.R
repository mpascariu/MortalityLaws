# Example 1 --- Makeham --- multiple life tables from a matrix of parameters

x1 <- 45:100
L1 <- "makeham"
C1 <- matrix(
  c(0.00717, 0.07789, 0.00363,
    0.01018, 0.07229, 0.00001,
    0.00298, 0.09585, 0.00002,
    0.00067, 0.11572, 0.00078),
  nrow = 4,
  dimnames = list(1:4, c("A", "B", "C"))
)

LawTable(x = x1, par = C1, law = L1)

# ---- Important note on age scaling ----

# The Makeham model applies internal age scaling during fitting.
# If the coefficients above were estimated over ages 45-100, the life
# table produced by LawTable is valid only from age 45 onward.

# ---- Example 1B: correct usage ----
LawTable(x = x1, par = c(0.00717, 0.07789, 0.00363), law = L1)

# ---- Example 1C: incorrect usage ----
# The code below uses the same coefficients but starts at age 25.
# Because the model was fitted on scaled ages (starting at 45),
# the life table at age 25 will be meaningless (e.g., e25 equals e45).
LawTable(x = 25:100, par = c(0.00717, 0.07789, 0.00363), law = L1)


# ---- How to check which laws apply scaling ----
A <- availableLaws()$table
A[, c("CODE", "SCALE_X")]

# Example 2 --- Heligman-Pollard (no scaling) ---

x2 <- 0:110
L2 <- "HP"
C2 <- c(0.00223, 0.01461, 0.12292, 0.00091,
        2.75201, 29.01877, 0.00002, 1.11411)

LawTable(x = x2, par = C2, law = L2)

# Because "HP" does NOT scale the age vector, the output is valid for
# any starting age. Compare:
LawTable(x = 3:110, par = C2, law = L2)
# Note that e3 = 70.31 in both tables, confirming consistency.

