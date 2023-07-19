# -------------------------------------------------------------- #
# Title: Test HP model for Indian data
# Author: Marius D. PASCARIU
# Last Update: Sun Mar 26 08:18:03 2023
# -------------------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(MortalityLaws)

# Load data file
load("region_India.RData")

# Filter data
d <- region_India %>% 
  filter(
    state  == 'South',
    sec    == 'Rural',
    gender == 'Female',
    ) 

# Process data by applying a smoothing method and convert the qx values to
# hazard rates (mx) using life table methods

x         <- d$age
qx        <- d$qx_g1_spss
qx_smooth <- loess(d$qx ~ x, span = 0.3)$fitted
mx        <- convertFx(x, data = qx, from = 'qx', to = 'mx')
mx_smooth <- convertFx(x, data = qx_smooth, from = 'qx', to = 'mx')

# Calibrate HP model for qx-values (death probabilities)
# Optimization method. Feel free to test various options. 
# In this case LF1 seems to work well.
mth = 'LF1' 
hp_qx_model  <- MortalityLaw(x, qx = qx, opt.method = mth, law = 'HP', show = F)
hp_qxs_model <- MortalityLaw(x, qx = qx_smooth, opt.method = mth, law = 'HP', show = F)

# Calibrate HP model for mx-values (mortality rates/hazard)
hp_mx_model  <- MortalityLaw(x, mx = mx, opt.method = mth, law = 'HP', show = F)
hp_mxs_model <- MortalityLaw(x, mx = mx_smooth, opt.method = mth, law = 'HP', show = F)
# note: we are using the mx argument not qx!

# Create a table with all the results
dt <- d %>% 
  select(state, sec, gender) %>% 
  mutate(
    x = x,
    qx = qx,
    qx_smooth = qx_smooth,
    qx_fit = fitted(hp_qx_model),
    qx_smooth_fit = fitted(hp_qxs_model),
    mx = mx,
    mx_smooth = mx_smooth,
    mx_fit = fitted(hp_mx_model),
    mx_smooth_fit = fitted(hp_mxs_model),
         )

# We create a plot function to be used for qx and mx
ggplot_fun <- function(vars_to_plot) {
  p <- dt %>% 
    pivot_longer(cols = -c(1:4), names_to = 'index', values_to = 'value') %>% 
    filter(index  %in% vars_to_plot) %>% 
    ggplot() + 
    geom_line(aes(x = x, y = value, linetype = index, color = index)) +
    geom_point(aes(x = x, y = value, size = index, color = index)) +
    scale_color_manual(values = c(1,1,2,2)) +
    scale_linetype_manual(values = c(0, 2, 0, 2)) +
    scale_size_manual(values = c(4, 0.01, 2, 0.01)) + 
    coord_trans(y = "log") +
    theme_bw()
  p
}


# Plot qx fitted data
ggplot_fun(c('qx', 'qx_smooth', 'qx_fit', 'qx_smooth_fit')) 
# Plot mx fitted data
ggplot_fun(c('mx', 'mx_smooth', 'mx_fit', 'mx_smooth_fit')) 

# The calibration of the HP model over the smoothed data reduces variability
# and seems reasonable

# Table of coefficients for various calibrations
coef_table <- bind_rows(
  coef(hp_qx_model),
 coef(hp_qxs_model),
 coef(hp_mx_model),
 coef(hp_mxs_model)
 ) %>% 
  mutate(
    optimization_method = mth,
    fitted_index = c('qx', 'qx_smooth', 'mx', 'mx_smooth'),
    .before = 1
    )

coef_table

# note the difference in the A, B, C parameters when are calibrated over the 
# mx values in comparison with qx values


# we can also look at the standard plots

plot(hp_qxs_model)
plot(hp_qx_model)
plot(hp_mx_model)
plot(hp_mxs_model)

