##
##  Name:       create_regional_coastline.R
##
##  Objective:  Create coastline for ecoregion of study
##
##  Approach:   Identify ecoregions of interest for
##              region and use rnaturalearth to extract
##              coastline and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-11
##

##
## 1. Set up
##
  # call to additional functionality
    # library(rnatualearth)

 ## -- call to global ecoregions -- ##
  # point to data locale
    data_locale <- "data_raw/spatial/shp/marine_ecoregions/"

  # point to data file
    data_file <- "meow_ecos.shp"

  # import ecoregions
    meow_ecos <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- call to ecoregion list -- ##
  # point to data locale
    data_locale <-  "data_intermediate/spatial/ecoregions/"

  # call to ecoregions
    load(paste0(data_locale, "ecoregion_list.rda"))


##
## 2. Groom data
##
  # have a look
    ecoregion_list
 # [1] "Central Somali Coast"
 # [2] "Northern Monsoon Current Coast"
 # [3] "East African Coral Coast"
 # [4] "Seychelles"
 # [5] "Cargados Carajos/Tromelin Island"
 # [6] "Mascarene Islands"
 # [7] "Southeast Madagascar"
 # [8] "Western and Northern Madagascar"
 # [9] "Bight of Sofala/Swamp Coast"
# [10] "Delagoa"

  # extract relevant ecoregions
    regional_ecoregions <-
      meow_ecos %>%
        dplyr::filter(ECOREGION %in% ecoregion_list)


 ## -- extract coastline -- ##
  # get coastline
    global_coastline <-
      rnaturalearth::ne_countries(scale = 10)

 ## -- clip to coastline -- ##
  # set bounding box
    regional_bbox <-
      regional_ecoregions %>%
      st_bbox()

  # set ecoregional boundary
    regional_coastline <-
      global_coastline %>%
        st_make_valid() %>%
        st_crop(regional_bbox) %>%
        st_geometry() %>%
        st_union()


##
## 3. Generate outputs
##
 ## -- save coastline -- ##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coastline/"

  # save regional coastline to file
    save(regional_coastline,
      file = paste0(save_locale, "regional_coastline.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(meow_ecos,
       ecoregion_list,
       global_coastline,
       regional_ecoregions,
       regional_bbox)

  # remove core objects
    rm(regional_coastline)

