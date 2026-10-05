\dontrun{
# Download demographic data for Fukushima and Tokyo regions in 1x1 format

# Death counts. We don't want to export data outside R.
JMD_Dx <- ReadJMD(what = "Dx",
                  regions = c('Fukushima', 'Tokyo'),
                  interval  = "1x1",
                  save = FALSE)
JMD_Dx

# Download life tables for female population in all the states and export data.
LTF <- ReadJMD(what = "LT_f", interval  = "5x5", save = FALSE)
LTF
}
