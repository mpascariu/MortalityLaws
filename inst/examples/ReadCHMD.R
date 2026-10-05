\dontrun{
# Download demographic data for Quebec and Saskatchewan regions in 1x1 format

# Death counts. We don't want to export data outside R.
CHMD_Dx <- ReadCHMD(what = "Dx",
                    regions = c('QUE', 'SAS'),
                    interval  = "1x1",
                    save = FALSE)

# Download life tables for female population. To export data use save = TRUE.
LTF <- ReadCHMD(what = "LT_f",
                regions = c('QUE', 'SAS'),
                interval  = "1x1",
                save = FALSE)
}
