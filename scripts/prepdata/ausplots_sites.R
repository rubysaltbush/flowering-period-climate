# using ausplotsR package access site data from AusPlots and cache csv
# requires internet connection

ausplots_sites <- cache_csv("data_cache/ausplots_sites.csv", function() {
  get_ausplots(site_info = TRUE, veg.vouchers = FALSE, veg.PI = FALSE)$site.info
})