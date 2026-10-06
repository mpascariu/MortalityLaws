# Example 1 --- Full life tables with different inputs ------------

y  <- 1900
x  <- as.numeric(rownames(ahmd$mx))
Dx <- ahmd$Dx[, paste(y)]
Ex <- ahmd$Ex[, paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
LT2 <- LifeTable(x, mx = LT1$lt$mx)
LT3 <- LifeTable(x, qx = LT1$lt$qx)
LT4 <- LifeTable(x, lx = LT1$lt$lx)
LT5 <- LifeTable(x, dx = LT1$lt$dx)
LT6 <- LifeTable(x, ex = LT1$lt$ex)

LT1
LT5
LT6
ls(LT5)

# Example 2 --- Compute multiple life tables at once ------------

LTs <- LifeTable(x, mx = ahmd$mx)
LTs
# A warning is printed if the input contains missing values.
# Some of the missing values can be handled automatically.

# Example 3 --- Abridged life table -----------------------------

x  <- c(0, 1, seq(5, 110, by = 5))
mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
        .004, .005, .006, .0093, .0129, .019, .031, .049,
        .084, .129, .180, .2354, .3085, .390, .478, .551)
LT7 <- LifeTable(x, mx = mx, sex = "female")
LT7

# Example 4 --- Abridged life table using a custom 'ax' --------
# This example reuses the ages (x) and death rates (mx) from Example 3.
# Note that 'ax' must have the same length as 'x', otherwise an error
# will be returned.

my_ax <- c(0.1, 1.5, rep(2, 19), 1, 1, 1)

LT8 <- LifeTable(x = x, mx = mx, ax = my_ax)

# Example 5 --- The ax methods ------------------------------
# The default 'andreev_kingkade' follows the HMD Methods Protocol v6
# (Andreev-Kingkade a0, half-interval elsewhere). 'cfm' is the plain
# constant-force identity; 'preston' and 'coale_demeny' are the two
# Coale-Demeny conventions for the first two intervals (identical above
# m0 = 0.107).

LT9  <- LifeTable(x, mx = mx, sex = "female", ax = "andreev_kingkade")
LT10 <- LifeTable(x, mx = mx, sex = "female", ax = "cfm")
LT11 <- LifeTable(x, mx = mx, sex = "female", ax = "preston")
LT12 <- LifeTable(x, mx = mx, sex = "female", ax = "coale_demeny")

rbind(
 andreev_kingkade = LT9$lt$ax[1:2], 
 cfm = LT10$lt$ax[1:2],
 preston = LT11$lt$ax[1:2], 
 coale_demeny = LT12$lt$ax[1:2]
)

# Example 6 --- Closing the open interval accurately -----------
# The data stop at 75+; closing there assumes a constant hazard above 75.
# 'close' argument corrects the open-interval rate with a fitted law, on the same
# age grid; 
# 'omega' argument offers the option to extend the table to 110 before closing it.

x5  <- c(0, 1, seq(5, 75, by = 5))
mx5 <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
         .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
LT13 <- LifeTable(x5, mx = mx5)
LT14 <- LifeTable(x5, mx = mx5, close = "kannisto")
LT15 <- LifeTable(x5, mx = mx5, close = "kannisto", omega = 110)

c(default = LT13$lt$ex[1], close = LT14$lt$ex[1], omega = LT15$lt$ex[1])


