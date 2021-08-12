# combine flowering period data from all sources for all available species

species_flowering_time <- cache_csv("data_cache/species_flowering_time.csv", function() {
  # load in flowering time data from AusTraits v2.1.0
  austraits <- readRDS("data_input/austraits-2.1.0.rds")
  flowering1 <- dplyr::filter(austraits$traits, austraits$traits$trait_name == "flowering_time")
  flowering1 <- dplyr::select(flowering1, dataset_id, canonicalName = taxon_name,
                              observation_id, value)
  
  # read in additional data collated from original sources
  flowering2 <- read_csv("data_input/flowering_time_data_RS.csv")
  flowering2 <- dplyr::select(flowering2, dataset_id, canonicalName = taxon_name,
                              observation_id, value)
  
  # combine flowering time data
  flowering <- rbind(flowering1, flowering2)
  rm(flowering1)
  rm(flowering2)
  rm(austraits)
  
  # clean any non-standard characters from flowering data species names
  flowering$canonicalName <- str_trim(flowering$canonicalName)
  
  #remove any subspecies or variants so that only species level IDs remain
  flowering$canonicalName <-
    gsub("\\s(?:subsp|var|f|sect)\\.\\s.+$", "", flowering$canonicalName, perl = TRUE)
  
  # define functions - convert YN in value column to binary values
  # this function turns YN values into unique integers (binary string of 0s and 1s)
  stringYNtoInt <- function(valueYN) {
    strtoi(gsub("N", "0", gsub("Y", "1", valueYN)), 2)
  }
  
  # this function takes unique integers and combines them e.g. 0101 + 1100 = 1101
  bitwiseOrList <- function(listOfIntegers) {
    Reduce("bitwOr", listOfIntegers)
  }
  
  # this function sums all the 1s in the binary together to give a total number 
  # of months in the year flowering e.g. 011000101 = 4
  sumBitsInInt <- function(num) {
    result <- 0
    for (i in 0:11) {
      x <- bitwAnd(bitwShiftR(num, i), 1)
      result <- result + x
    }
    result
  }
  
  # convert flowering time YN value to unique integer
  flowering$monthsInt <- stringYNtoInt(flowering$value)
  # add together each positive value for flowering month e.g. 0011 + 0101 = 0111
  species_flowering_time <- aggregate(flowering$monthsInt, by = list(flowering$canonicalName), FUN = bitwiseOrList)
  colnames(species_flowering_time) <- c("canonicalName", "monthsInt")
  # create column displaying aggregated flowering time values in binary strings
  species_flowering_time$monthsBinary <- R.utils::as.character.binmode(species_flowering_time$monthsInt)
  # convert aggregated binary flowering time value into Length of flowering period
  species_flowering_time$monthsCount <- sumBitsInInt(species_flowering_time$monthsInt)
  
  rm(stringYNtoInt)
  rm(bitwiseOrList)
  rm(sumBitsInInt)
  rm(flowering)
  
  #remove any records with "0" monthsCount as these are useless data
  species_flowering_time <- dplyr::filter(species_flowering_time, monthsCount != 0)
  
  #select useful variables from species_flowering_time (remove integer, don't need this)
  species_flowering_time <- dplyr::select(species_flowering_time, canonicalName, monthsBinary:monthsCount)
  
  species_flowering_time
})
