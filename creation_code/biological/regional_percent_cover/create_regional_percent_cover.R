##
##  Name:       create_coral_percent_cover.R
##
##  Objective:  Create coral and other benthic cover
##                data object from gcrmn sources
##
##  Approach:   Call to clean data compilation from gcrmn
##              sources, format and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Benthic cover data are primary source for Criterion A.
##                 Data should include a column assigning each
##                 record to the respective geographic unit of assessment.

##
## 1. Set up
##
 ## -- call to percent cover data data -- ##
  # point to data locale
    data_locale <- "data_raw/biological/percent_cover/"

  # set data file name
    data_file <- "regional_percent_cover.xlsx"

  # import data
    regional_percent_cover <-
      paste0(data_locale, data_file) %>%
      read_excel()


##
## 2. Groom data
##
  # have a look
    regional_percent_cover
# # A tibble: 17,560 × 7
#    Ecoregion       Year Reef.zone Latitude Longitude level1_code
#    <chr>          <dbl> <chr>        <dbl>     <dbl> <chr>
#  1 Cargados Cara…  2010 <NA>         -16.6      59.6 HC
#  2 Cargados Cara…  2010 <NA>         -16.6      59.6 SC
#  3 Cargados Cara…  2010 <NA>         -16.6      59.6 INV
#  4 Cargados Cara…  2010 <NA>         -16.6      59.6 AMAC
#  5 Cargados Cara…  2010 <NA>         -16.6      59.6 AHAL
#  6 Cargados Cara…  2010 <NA>         -16.6      59.6 ACOR
#  7 Cargados Cara…  2010 <NA>         -16.6      59.6 ATRF
#  8 Cargados Cara…  2010 <NA>         -16.6      59.6 BS
#  9 Cargados Cara…  2010 <NA>         -16.6      59.6 RUB
# 10 Cargados Cara…  2010 <NA>         -16.6      59.6 SND
# # ℹ 17,550 more rows
# # ℹ 1 more variable: percent_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # compressed view
    regional_percent_cover %>% quickview()
#                          Ecoregion Year Reef.zone  Latitude
# 1 Cargados Carajos/Tromelin Island 2010      <NA> -16.63324
# 2 Cargados Carajos/Tromelin Island 2010      <NA> -16.63324
# 3 Cargados Carajos/Tromelin Island 2010      <NA> -16.63324
#   Longitude level1_code percent_cover
# 1  59.62795          HC      4.176123
# 2  59.62795          SC      2.258318
# 3  59.62795         INV      3.401470

 ## -- create identifiers for sites and country-ecoregions -- ##
  # # rename ecoregion
  #   regional_percent_cover %<>%
  #     rename(Ecoregion = eco_rgn)

  # # set station id
  #   regional_percent_cover %<>%
  #     mutate(site_id = paste(Sector, Site, Station, Reef.zone, sep = "_"))
  #
  # # create country ecoregion label
  #   regional_percent_cover %<>%
  #     mutate(ecoregion_country = paste0(Ecoregion, "_", Country))


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/percent_cover/"

  # save to file
    save(regional_percent_cover,
      file = paste0(save_locale, "regional_percent_cover.rda"))


##
## 4. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove core objects
    rm(regional_percent_cover)

