\dontrun{
# Download demographic data for Australian Capital Territory and
# Tasmania regions in 5x1 format

# Death counts. We don't want to export data outside R.
AHMD_Dx <- ReadAHMD(what = "Dx",
                    regions = c('ACT', 'TAS'),
                    interval  = "5x1",
                    save = FALSE)
AHMD_Dx

# Download life tables for female population in all the states and export data.
LTF <- ReadAHMD(what = "LT_f", interval  = "5x1", save = FALSE)
LTF
}
