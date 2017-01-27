# ---- LAWS -------------

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz <- function(x, par = NULL){
  model_info <- 'Gompertz (1825): h(x) = a*exp(b*x)'
  # default parameters
  a = 0.0002; b = 0.13
  if (!is.null(par)) { a = par[1]; b = par[2] }
  # Compute distribution functions
  hx <- a*exp(b*x)
  Hx <- a/b * (exp(b*x) - 1)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
         par = c(a = a, b = b)))
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  model_info <- 'Makeham (1860):  h(x) = a*exp(b*x) + c'
  # default parameters
  a = 0.0002; b = 0.13; c = 0.001
  if (!is.null(par)) { a = par[1]; b = par[2]; c = par[3] }
  # Compute distribution functions
  hx <- a*exp(b*x) + c
  Hx <- a/b * (exp(b*x) - 1) + x*c
  return(list(model_info = model_info, hx = hx, Hx = Hx,
         par = c(a = a, b = b, c = c)))
}

#' Kannisto mortality law
#' @keywords internal
#' 
kannisto <- function(x, par = NULL){
  model_info <- 'Kannisto (1992): h(x) = a*exp(b*x) / [1 + a*exp(b*x)]'
  # default parameters
  a = 0.5; b = 0.13
  if (!is.null(par)) { a = par[1]; b = par[2] }
  # Compute distribution functions
  hx <- a*exp(b*x) / (1 + a*exp(b*x))
  Hx <- 1/a * log( (1 + a*exp(b*x)) / (1 + a) )
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a, b = b)))
}


#' DeMoivre mortality law
#' @keywords internal
#' 
demoivre <- function(x, par = NULL){
  model_info <- 'DeMoivre (1725): h(x) = 1/(a-x)'
  # default parameters
  a = 100
  if (!is.null(par)) { a = par[1] }
  # Compute distribution functions
  vsmall = 1e-10 # very small number
  hx <- 1/(a-x) + vsmall
  Hx <- cumsum(hx)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a)))
}


#' Opperman mortality law
#' @keywords internal
#' 
opperman <- function(x, par = NULL){
  model_info <- 'Opperman (1870): h(x) = a*x^(-1/2) + b + c*x^(1/3)'
  # default parameters
  a = 0.004; b = -0.0004; c = 0.001
  if (!is.null(par)) { a = par[1]; b = par[2]; c = par[3] }
  # Compute distribution functions
  hx = a/sqrt(x) + b + c*(x^(1/3))
  Hx = cumsum(hx)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a, b = b, c = c)))
}

#' Heligman-Pollard mortality law
#' @keywords internal
#' 
heligman_pollard <- function(x, par = NULL){
  model_info <- 'Heligman-Pollard (1980): q(x)/p(x) = a^((x+b)^c) + d*exp(-e*(log(x/f))^2) + g*h^x)'
  # default parameters
  a = 0.0005; b = 0.004; c = 0.08; d = 0.001 
  e = 10; f = 17; g = 0.00005; h = 1.1
  if (!is.null(par)) {a = par[1]; b = par[2]; c = par[3]; d = par[4]; 
                      e = par[5]; f = par[6]; g = par[7]; h = par[8]}
  # Compute distribution functions
  hx = ifelse(x==0, 
              a^((x+b)^c) + g*h^x, 
              a^((x+b)^c) + d*exp(-e*(log(x/f))^2) + g*h^x)
  Hx = cumsum(hx)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a, b = b, c = c, d = d, 
                      e = e, f = f, g = g, h = h)))
}



#' Thiele mortality law
#' @keywords internal
#' 
thiele <- function(x, par = NULL){
  model_info <- 'Thiele (1871): h(x) = a*exp(-b*x) + c*exp[-.5d*(x-e)^2] + f*exp(g*x)'
  # default parameters
  a = 0.02474; b = 0.3; c = 0.004; d = 0.5 
  e = 25; f = 0.0001; g = 0.13
  if (!is.null(par)) {a = par[1]; b = par[2]; c = par[3]; d = par[4]; 
                      e = par[5]; f = par[6]; g = par[7]}
  # Compute distribution functions
  hx = ifelse(x == 0, 
              a*exp(-b*x) + f*exp(g*x), 
              a*exp(-b*x) + c*exp(-.5*d*(x-e)^2) + f*exp(g*x))
  Hx = cumsum(hx)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a, b = b, c = c, d = d, 
                      e = e, f = f, g = g)))
}


#' Wittstein mortality law
#' @keywords internal
#' 
wittstein <- function(x, par = NULL){
  model_info <- 'Wittstein (1883): q(x) = (1/m)*a^-[(m*x)^n] + a^-[(M-x)^n]'
  # default parameters
  a = 1.5; m = 1.0; n = 0.5; M = 100
  if (!is.null(par)) {a = par[1]; m = par[2]; n = par[3]; M = par[4]}
  # Compute distribution functions
  hx = (1/m)*a^-((m*x)^n) + a^-((M-x)^n)
  Hx = cumsum(hx)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a, m = m, n = n, M = M)))
}













