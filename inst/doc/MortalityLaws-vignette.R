## ----echo = FALSE, message = FALSE, fig.height = 4.5, fig.width = 7, fig.pos = 'H'----
library(diagram)

names <- c('MortalityLaws\n R package', 
           'ReadHMD()', 'MortalityLaw()', 'LifeTable()', 'convertFx()',
          'summary()', 'coef()','fitted.values()', 'plot()')

M <- matrix(nrow = 9, ncol = 9, byrow = TRUE, 
           data = c(# M hmd, r  m   l  p  s  p
                      0, 0, 0, 0,  0, 0, 0, 0, 0, #H
                      1, 0, 0, 0,  0, 0, 0, 0, 0, #hmd
                      2, 0, 0, 0,  0, 0, 0, 0, 0, #read
                      2, 0, 0, 0,  0, 0, 0, 0, 0, #Ml
                      2, 0, 0, 0,  0, 0, 0, 0, 0, #life
                      0, 0, 3, 0,  0, 0, 0, 0, 0, #summary
                      0, 0, 3, 0,  0, 0, 0, 0, 0, #coef
                      0, 0, 3, 0,  0, 0, 0, 0, 0, #fitted
                      0, 0, 3, 0,  0, 0, 0, 0, 0  #plot
                    ))

par(mar = c(0.1, 0.1, 0.1, 0.1))
plotmat(M, pos = c(1, 4, 4), curve = 0, name = names,
        lwd = 1, box.lwd = 2, cex.txt = 0,
        box.type = "square", box.prop = 0.4, arr.type = "triangle",
        arr.pos = 0.6, shadow.size = 0.01, prefix = "f",
        main = "")

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # Make sure you have the most recent version of R and devtools package already installed.
#  # install.packages("devtools")
#  devtools::install_github("mpascariu/MortalityLaws")

## ----echo=TRUE, eval=TRUE------------------------------------------------
library(MortalityLaws)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # Download HMD data - death counts
#  HMD_Dx <- ReadHMD(what = "Dx",
#                    countries = "SWE",  # HMD country code for Sweden
#                    interval = "1x1",            # 1 year - 1 age data
#                    username = "user@email.com", # here add your HMD username
#                    password = "password",       # here add your password account
#                    save = FALSE)                # Do you what to save the data outside R

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  HMD_Ex <- ReadHMD(what = "LT_f",
#                    countries = "SWE",
#                    interval = "1x1",
#                    username = "user@email.com",
#                    password = "password",
#                    save = TRUE)

## ----fitmodel, echo=TRUE, eval=TRUE--------------------------------------
# Select 1 year of data, an age-range and sex
year = 1950
ages = 0:100
deaths <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Nx[paste(ages), paste(year)]

fit <- MortalityLaw(x = ages,
                    Dx = deaths, # vector with death counts
                    Ex = exposure, # vector containing exposures
                    law = "HP",
                    how = "poissonL",
                    fit.this.x = 0:65)

ls(fit) # inspect the output object

## ----echo=TRUE, eval=TRUE------------------------------------------------
summary(fit)

## ----echo = TRUE, eval = TRUE, fig.cap='Heligman-Pollard model fitted using the MortalityLaw function', fig.height = 3, fig.width = 7, fig.pos='H'----
plot(fit)

## ----echo=TRUE, eval=TRUE------------------------------------------------
lt <- LifeTable(x = ages, mx = fitted.values(fit))
ls(lt)
head(lt$lt)

## ----gompertz2, echo=TRUE, eval=TRUE, fig.cap='Gompertz model fitted using the MortalityLaw function', fig.height = 3, fig.width = 7, fig.pos='htb'----
# Select the data that we want to fit
year = 2010
ages = 35:75
deaths <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Nx[paste(ages), paste(year)]

# Here we define a function for our new model and provide start parameters
my_gompertz <- function(x, par = c(b = 0.13, M = 45)){
  hx  <- with(as.list(par), b*exp(b*(x - M)) )
  return(as.list(environment())) # return everything inside this function
}

# Use 'custom.law' argument to instruct the MortalityLaw function how to behave
my_model <- MortalityLaw(x = ages,
                         Dx = deaths, 
                         Ex = exposure, 
                         custom.law = my_gompertz)
summary(my_model) # these are the results
plot(my_model)

