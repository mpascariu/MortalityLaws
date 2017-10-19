rm(list = ls())

x  <- 0:100
mx <- ahmd$mx[paste(x), ] # select data

qx1 <- convertFx(mx, x = x, type = 'mx', output = 'qx') # transform mx into qx
fx1 <- convertFx(mx, x = x, type = 'mx', output = 'fx') # transform mx into qx
mx1 <- convertFx(qx1, x = x, type = 'qx', output = 'mx') # transform mx into qx
# qx2 <- convertFx(fx1, x = x, type = 'fx', output = 'qx') # transform mx into qx


expect_true(all(qx1 > 0))
expect_true(all(fx1 > 0))
expect_true(all(mx1 > 0))
expect_true(all(round((mx - mx1)/mx * 100, 2)[-101,] == 0))

