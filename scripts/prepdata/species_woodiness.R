species_woodiness <- cache_csv("data_cache/species_woodiness.csv", function() {
  #read in woodiness data - from AusTraits but reconciled manually
  species_woodiness <- read_csv("data_input/ausplots_woody_RS.csv")
  #add column scoring woody/non-woody (include semi-woody as woody)
  species_woodiness$woody <- species_woodiness$RS_value
  species_woodiness$woody <- gsub("herbaceous", "0", species_woodiness$woody, perl = TRUE)
  species_woodiness$woody <- gsub("woody|semi_woody", "1", species_woodiness$woody, perl = TRUE)
  species_woodiness <- rename(species_woodiness, woodiness = RS_value)
  species_woodiness
})