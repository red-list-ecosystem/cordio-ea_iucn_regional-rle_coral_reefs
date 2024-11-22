##
##  Name:       create_ecoregion_coral_reefs.R
##
##  Objective:  Extract coral reefs from wio ecoregions for
##              regional analysis
##
##  Approach:   Point to wio ecoregions and global reef
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
 ## -- call to ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to wio ecoregions
    load(paste0(data_locale, data_file))


 ## -- call to reef layer -- ##
  # point to https locale
    https_locale <-
      paste0("https://datadownload-production.s3.us-east-1.amazonaws.com/",
             "WCMC008_CoralReefs2021_v4_1.zip")

  # create temporary file for downloading
    temp_file <- tempfile()

  # set temporary file directory
    # temp_directory <- tempfile()
    temp_directory <- "data_raw/spatial/coral_reefs/"

  # download coral reefs
    https_locale %>% download.file(temp_file)

  # extract files
    temp_file %>%
        unzip(exdir = temp_directory)

  # point to data locale
    data_locale <-
      paste0(temp_directory,
             "14_001_WCMC008_CoralReefs2021_v4_1/01_Data/") %>%  
      str_replace("\\//", "\\/")


  # point to data file
    data_file <- "WCMC008_CoralReef2021_Py_v4_1.shp"

  # import ecoregions
    coral_reefs <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- remove reef downloads -- ##
  # remove temp directory
    paste0(temp_directory, "14_001_WCMC008_CoralReefs2021_v4_1") %>%
      fs::dir_delete()


##
## 2. Groom data
##
 ## -- extract coral reefs -- ##
  # set s2 to false
    sf_use_s2(FALSE)

  # extract
    regional_coral_reefs <-
      coral_reefs %>%
        st_intersection(regional_ecoregions %>% st_transform(4326))
# Simple feature collection with 1349 features and 27 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.86711 ymin: -27.01737 xmax: 63.51225 ymax: 2.755531
# Geodetic CRS:  WGS 84
# # A tibble: 1,349 × 28
   # LAYER_NAME METADATA_I ORIG_NAME    FAMILY  GENUS SPECIES DATA_TYPE
 # * <chr>           <dbl> <chr>        <chr>   <chr> <chr>   <chr>    
 # 1 CRR                57 Not Reported Not Re… Not … Not Re… Field su…
 # 2 CRR                57 Not Reported Not Re… Not … Not Re… Field su…
 # 3 CRR                80 Not Reported Not Re… Not … Not Re… Not Repo…
 # 4 CRR                80 Not Reported Not Re… Not … Not Re… Not Repo…
 # 5 CRR                80 Not Reported Not Re… Not … Not Re… Not Repo…
 # 6 CRR                80 Not Reported Not Re… Not … Not Re… Not Repo…
 # 7 CRR                80 Not Reported Not Re… Not … Not Re… Not Repo…
 # 8 CRR                81 Not Reported Not Re… Not … Not Re… Not Repo…
 # 9 CRR                81 Not Reported Not Re… Not … Not Re… Not Repo…
# 10 CRR                81 Not Reported Not Re… Not … Not Re… Not Repo…
# # ℹ 1,339 more rows
# # ℹ 21 more variables: START_DATE <chr>, END_DATE <chr>,
# #   DATE_TYPE <chr>, VERIF <chr>, NAME <chr>, LOC_DEF <chr>,
# #   SURVEY_MET <chr>, GIS_AREA_K <dbl>, Shape_Leng <dbl>,
# #   Shape_Area <dbl>, REP_AREA_K <chr>, Eco_code <dbl>,
# #   Ecoregion <chr>, Prov_code <dbl>, Province <chr>,
# #   Rlm_code <dbl>, Realm <chr>, Alt_code <dbl>, Eco_code_x <dbl>, …
# # ℹ Use `print(n = ...)` to see more rows


   # get column names
     regional_coral_reefs %>% names()
 # [1] "LAYER_NAME" "METADATA_I" "ORIG_NAME"  "FAMILY"     "GENUS"     
 # [6] "SPECIES"    "DATA_TYPE"  "START_DATE" "END_DATE"   "DATE_TYPE" 
# [11] "VERIF"      "NAME"       "LOC_DEF"    "SURVEY_MET" "GIS_AREA_K"
# [16] "Shape_Leng" "Shape_Area" "REP_AREA_K" "Eco_code"   "Ecoregion" 
# [21] "Prov_code"  "Province"   "Rlm_code"   "Realm"      "Alt_code"  
# [26] "Eco_code_x" "Lat_zone"   "geometry" 

  # simplify object
    regional_coral_reefs %<>%
      dplyr::select(Ecoregion,
                    Eco_code,
                    Province,
                    Prov_code,
                    geometry)

 # ## -- visualise -- ##
  # # set palette
    # c_palette <-
      # wesanderson::wes_palette("Cavalcanti1", 15, "continuous")

  # # plot
    # regional_coral_reefs %>%
      # ggplot() +
      # geom_sf(aes(colour = Ecoregion)) +
      # theme_void() +
      # scale_colour_manual(values = c_palette)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/coral_reefs/"

  # save ecoregions to file
    save(regional_coral_reefs,
      file = paste0(save_locale, "regional_coral_reefs.rda"))


# ## -- export to shapefile -- ##
#  # point to save locale
#    save_locale <- "data/spatial/coral_reefs/wcmc_coral_reefs_2021_v4.1/"
#
#  # export geometry only
#    coral_reefs %>%
#      st_geometry() %>%
#      st_write(paste0(save_locale, "wcmc_coral_reefs_2021_v4.1.shp"))
# Writing layer `wcmc_coral_reefs_2021_v4.1' to data source
#   `data/spatial/coral_reefs/wcmc_coral_reefs_2021_v4.1/wcmc_coral_reefs_2021_v4.1.shp'
#    using driver `ESRI Shapefile'
# Writing 17504 features with 0 fields and geometry type Multi Polygon.

##
## 4. Clean up workspace
##
  # clean up paths
    rm(https_locale,
       data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_ecoregions,
       coral_reefs)

  # remove core objects
    rm(regional_coral_reefs)

