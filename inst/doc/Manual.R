## ----setup, include=FALSE------------------------------------------------
library(MortalityLaws)
library(knitr)
opts_chunk$set(collapse = TRUE)

## ----diagram, echo=FALSE, message=FALSE, results='hide', fig.align='center', out.width='90%', fig.width=9, fig.cap="MortalityLaws R Package Structure", fig.pos='h'----
library(diagram)

MortalityLaws_diagram <- function(){
  par(mar = c(.1, .5, .1, .5))
  names <- c(
            "availableHMD()", "availableLaws()", "availableLF()",
            "MortalityLaws\n R package", 
            "ahmd\n(Test data)", "ReadHMD()\n(Download data...)", 
            "MortalityLaw()\n(Fit/Predict models)",
            "LifeTable()\n(Construct life tables)"
            )
  
  M <- matrix(nrow = 8, ncol = 8, byrow = TRUE, 
             data = c(
                       0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 1, 0, 0, 0, 0
                      ))
  pp <- plotmat(M, pos = c(3, 1, 4), curve = 0, name = names,
              lwd = 1, box.lwd = 2, cex.txt = 0,
              box.type = "square", box.prop = 0.4, arr.type = "triangle",
              arr.pos = 0.6, shadow.size = 0.01, prefix = "f",
              main = "")
}

MortalityLaws_diagram()

## ----LoadPackages, message=FALSE-----------------------------------------
library(MortalityLaws)

## ----ReadHMD, eval=FALSE-------------------------------------------------
#  
#  # Download HMD data - death counts
#  HMD_Dx <- ReadHMD(what = "Dx",
#                    countries = "SWE",            # HMD country code for Sweden
#                    interval  = "1x1",            # specify data format
#                    username  = "user@email.com", # here add your HMD username
#                    password  = "password",       # here add your password account
#                    save = FALSE)                 # save data outside R

## ------------------------------------------------------------------------
year     <- 1950
ages     <- 0:100
deaths   <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Ex[paste(ages), paste(year)]

fit <- MortalityLaw(x   = ages,
                    Dx  = deaths,   # vector with death counts
                    Ex  = exposure, # vector containing exposures
                    law = "HP",
                    opt.method = "LF2")


## ------------------------------------------------------------------------
# inspect the output object
ls(fit) 

## ------------------------------------------------------------------------
summary(fit)

## ---- fig.align='center', out.width='80%', fig.width=9-------------------
plot(fit)

## ---- fig.align='center', out.width='80%', fig.width=9-------------------
fit.subset <- MortalityLaw(x   = ages,
                           Dx  = deaths,   
                           Ex  = exposure,
                           law = "HP",
                           opt.method = "LF2",
                           fit.this.x = 0:65) 
plot(fit.subset)

## ---- eval=FALSE, warning=FALSE------------------------------------------
#  availableLaws()

## ---- message=FALSE, warning=FALSE---------------------------------------
availableLF()

## ------------------------------------------------------------------------

# Here we define a function for our new model and provide start parameters
my_gompertz <- function(x, par = c(b = 0.13, M = 45)){
  hx  <- with(as.list(par), b*exp(b*(x - M)) )
  # return everything inside this function
  return(as.list(environment())) 
}

## ------------------------------------------------------------------------
# Select data
year     <- 1950
ages     <- 45:85
deaths   <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Ex[paste(ages), paste(year)]

## ---- warning=FALSE, results='hide'--------------------------------------
# Use 'custom.law' argument to instruct the MortalityLaw function how to behave
my_model <- MortalityLaw(x = ages,
                         Dx = deaths, 
                         Ex = exposure, 
                         custom.law = my_gompertz)

## ------------------------------------------------------------------------
summary(my_model)

## ------------------------------------------------------------------------
plot(my_model)

## ------------------------------------------------------------------------
# Life table for year of 1900
y  <- 1900
x  <- as.numeric(rownames(ahmd$mx))
Dx <- ahmd$Dx[, paste(y)]
Ex <- ahmd$Ex[, paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
LT2 <- LifeTable(x, mx = LT1$lt$mx)
LT3 <- LifeTable(x, qx = LT1$lt$qx)
LT4 <- LifeTable(x, lx = LT1$lt$lx)
LT5 <- LifeTable(x, dx = LT1$lt$dx)

LT1

## ------------------------------------------------------------------------
ls(LT1)

## ------------------------------------------------------------------------
# Example
x  <- c(0, 1, seq(5, 110, by = 5))
mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004, 
       .004, .005, .006, .0093, .0129, .019, .031, .049, 
       .084, .129, .180, .2354, .3085, .390, .478, .551)

lt <- LifeTable(x, mx = mx, sex = "female")

## ------------------------------------------------------------------------
lt

