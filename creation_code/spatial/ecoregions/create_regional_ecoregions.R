##
##  Name:       create_wio_ecoregions.R
##
##  Objective:  Extract relevant ecoregions and coral reefs for
##              regional analysis
##
##  Approach:   Point to raw ecoregions and global reef
##              spatial layers, filter for relevant ecoregions,
##              extract and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-02-26
##

##
## 1. Set up
##
 ## -- call to global ecoregions -- ##
  # point to data locale
    data_locale <- "data_raw/spatial/shp/marine_ecoregions/"

  # point to data file
    data_file <- "meow_ecos.shp"

  # import ecoregions
    meow_ecos <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- call to africa continent -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # load coastline
    load(paste0(data_locale, data_file))

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

 ## -- set column names to title -- ##
  # rename
    regional_ecoregions %<>%
      dplyr::select(Eco_code   = ECO_CODE,
                    Ecoregion  = ECOREGION,
                    Prov_code  = PROV_CODE,
                    Province   = PROVINCE,
                    Rlm_code   = RLM_CODE,
                    Realm      = REALM,
                    Alt_code   = ALT_CODE,
                    Eco_code_x = ECO_CODE_X,
                    Lat_zone   = Lat_Zone,
                    geometry)


 ## -- clip to african coastline -- ##
  # set s2 to false
    sf_use_s2(FALSE)

  # set ecoregions to utm
    regional_ecoregions %<>%
      st_transform(32737)

  # set coastline to utm
    regional_coastline %<>%
      st_transform(32737)

  # mask
    regional_ecoregions %<>%
      st_difference(regional_coastline)

 ## -- create quick visual -- ##
  # visualise
    # regional_ecoregions %>%
    #   dplyr::select(Ecoregion) %>% plot()

##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/ecoregions/"

  # save ecoregions to file
    save(regional_ecoregions,
      file = paste0(save_locale, "regional_ecoregions.rda"))


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
       regional_coastline)

  # remove core objects
    rm(regional_ecoregions)

