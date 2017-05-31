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

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  library(MortalityLaws)

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

