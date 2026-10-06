x  <- 45:75
M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
                   Ex = ahmd$Ex[as.character(x), "1950"], law = "makeham")
plot(M1, which = "fit")
plot(M1, which = "diagnostics")
plot(M1, which = "diagnostics", split = c(1, 4))
plot(M1, which = "both")
