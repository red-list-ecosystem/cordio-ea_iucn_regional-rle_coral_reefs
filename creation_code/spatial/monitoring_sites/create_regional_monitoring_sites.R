##
##  Name:       create_wio_sampling_sites.R
##
##  Objective:  Create data object for monitoring site
##              coordinates and other characteristics
##
##  Approach:   Import data table with site coordinates,
##              summarise and generate spatial object.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-13
##

##
## 1. Set up
##
 ## -- call to ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # load data
    load(paste0(data_locale, data_file))


 ## -- call to data sourced from original -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/percent_cover/"

  # set data file name
    data_file <- "regional_percent_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look at percent cover
    regional_percent_cover
# # A tibble: 18,276 × 7
#    Ecoregion  Year Reef.zone Latitude Longitude level1_code
#    <chr>     <dbl> <chr>        <dbl>     <dbl> <chr>
#  1 Comoros    2018 Fore_reef    -11.4      43.3 HC
#  2 Comoros    2018 Fore_reef    -11.4      43.3 SC
#  3 Comoros    2018 Fore_reef    -11.4      43.3 INV
#  4 Comoros    2018 Fore_reef    -11.4      43.3 AMAC
#  5 Comoros    2018 Fore_reef    -11.4      43.3 AHAL
#  6 Comoros    2018 Fore_reef    -11.4      43.3 ACOR
#  7 Comoros    2018 Fore_reef    -11.4      43.3 ATRF
#  8 Comoros    2018 Fore_reef    -11.4      43.3 BS
#  9 Comoros    2018 Fore_reef    -11.4      43.3 RUB
# 10 Comoros    2018 Fore_reef    -11.4      43.3 SND
# # ℹ 18,266 more rows
# # ℹ 1 more variable: percent_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # have a loook at ecoregions
    regional_ecoregions
# Simple feature collection with 10 features and 9 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: -489499 ymin: 6709509 xmax: 3514757 ymax: 11288740
# Projected CRS: WGS 84 / UTM zone 37S
# # A tibble: 10 × 10
   # Eco_code Ecoregion Prov_code Province Rlm_code Realm Alt_code
 # *    <dbl> <chr>         <dbl> <chr>       <dbl> <chr>    <dbl>
 # 1    20097 Cargados…        20 Western…        5 West…       83
 # 2    20093 Central …        19 Somali/…        5 West…       93
 # 3    20102 Delagoa          20 Western…        5 West…       84
 # 4    20098 Mascaren…        20 Western…        5 West…       85
 # 5    20096 Seychell…        20 Western…        5 West…       86
 # 6    20099 Southeas…        20 Western…        5 West…       87
 # 7    20095 East Afr…        20 Western…        5 West…       88
 # 8    20094 Northern…        20 Western…        5 West…       89
 # 9    20101 Bight of…        20 Western…        5 West…       90
# 10    20100 Western …        20 Western…        5 West…       91
# # ℹ 3 more variables: Eco_code_x <dbl>, Lat_zone <chr>,
# #   geometry <GEOMETRY [m]>

 ## -- link ecoregions -- ##
  # set to spatial object
    regional_percent_cover_sf <-
      regional_percent_cover %>%
      mutate(long = Longitude,
             lat  = Latitude) %>%
      dplyr::select(Longitude,
                    Latitude,
                    long,
                    lat,
                    Year,
                    Reef.zone,
                    level1_code,
                    percent_cover) %>%
      st_as_sf(coords = c("long", "lat"),
               crs    = 4326)

  # link with ecoregions
    regional_percent_cover_sf %<>%
      st_transform(32737) %>%
    st_intersection(regional_ecoregions %>%
                      dplyr::select(Ecoregion,
                                    geometry))

  # return to data frame
    regional_percent_cover <-
      regional_percent_cover_sf %>%
        st_drop_geometry() %>%
        dplyr::select(Ecoregion,
                      Year,
                      Reef.zone,
                      Latitude,
                      Longitude,
                      level1_code,
                      percent_cover)


 ## -- generate criteria object -- ##
  # set level 1 code of interest
    level1_of_interest <-
      c("HC")

  # filter & summarise
    regional_percent_cover_summary <-
      regional_percent_cover %>%
      dplyr::filter(level1_code %in% level1_of_interest) %>%
      group_by(# site_id,
               # Country,
               Ecoregion,
               Latitude,
               Longitude) %>%
      summarise(first_year  = Year %>% min(na.rm = TRUE),
                recent_year = Year %>% max(na.rm = TRUE),
                lat         = Latitude  %>% unique(),
                long        = Longitude %>% unique(),
                no_years    = Year %>% unique() %>% length(),                ## -- no. of sampling points           -- ##
                year_gap    = (recent_year - first_year) + 1,                ## -- years between 1st & final record -- ##
                recent_coral_cover   = percent_cover %>% tail(n = 1),        ## -- returns latest coral cover       -- ##
                original_coral_cover = percent_cover %>% head(n = 1),        ## -- returns earliest coral cover     -- ##
                mean_coral_cover     = percent_cover %>% mean(na.rm = TRUE)) ## -- ave across series                -- ##

  # have a look
    regional_percent_cover_summary %>% quickview()
#   Ecoregion  Latitude Longitude first_year recent_year
# 1   Comoros -12.97964  45.21137       2017        2017
# 2   Comoros -12.83212  45.22168       2017        2017
# 3   Comoros -12.78721  45.30792       2017        2017
#         lat     long no_years year_gap recent_coral_cover
# 1 -12.97964 45.21137        1        1          33.083333
# 2 -12.83212 45.22168        1        1          33.083333
# 3 -12.78721 45.30792        1        1           5.751811
#   original_coral_cover mean_coral_cover
# 1            33.083333        33.083333
# 2             5.751811        27.617029
# 3             5.751811         5.751811

 # ## -- create spatial object -- ##
 #  # create Ecoregion
 #    regional_percent_cover_summary %<>%
 #      mutate(Ecoregion = ecoregion_country %>% str_split_i("_", 1))

  # order columns
    regional_percent_cover_summary %<>%
      ungroup() %>%
      dplyr::select(Ecoregion,
                    # Country,
                    # ecoregion_country,   ## -- evaluating if this is redundant -- ##
                    # site_id,
                    lat,
                    long,
                    first_year,
                    recent_year,
                    no_years,
                    year_gap,
                    recent_coral_cover,
                    original_coral_cover,
                    mean_coral_cover)

  # generate spatial object
    regional_monitoring_sites <-
      regional_percent_cover_summary %>%
        st_as_sf(coords = c("long", "lat"),
                 crs    = 4326)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/"

  # save ecoregions to file
    save(regional_monitoring_sites,
      file = paste0(save_locale, "regional_monitoring_sites.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_percent_cover,
       level1_of_interest,
       regional_percent_cover_summary)

  # remove core objects
    rm(regional_monitoring_sites)

