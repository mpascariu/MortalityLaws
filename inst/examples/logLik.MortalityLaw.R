x  <- 45:75
M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
                   Ex = ahmd$Ex[as.character(x), "1950"],
                   law = "makeham", opt.method = "poissonL")
logLik(M1)
