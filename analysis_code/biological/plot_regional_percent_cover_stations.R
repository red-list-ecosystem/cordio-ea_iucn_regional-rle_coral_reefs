##
##  Name:       plot_percent_cover_stations.R
##
##  Objective:  Standardise & format data for analysing criterion A:
##                Reduction in geographic distribution
##
##  Approach:   Call to clean data compilation, filter by time
##                period, geographic region, summarise and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##
## 1. Set up
##
 # -- call to custom wio ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- point to regional coastline -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # import coastline
    load(paste0(data_locale, data_file))


 ## -- point to ecoregions + custom wio coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to coral reefs
    load(paste0(data_locale, data_file))


 ## -- call to original data table -- ##
  # point to data locale
    data_locale <- "data_raw/criteria/"

  # point to data file
    data_file <- "criterion_a_data_table.xlsx"

  # import data table
    criterion_a_data_table <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


##
## 2. Groom data
##
 ## -- review ecoregion object -- ##
  # have a look
    regional_ecoregions
# Simple feature collection with 10 features and 9 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: -489499 ymin: 6709509 xmax: 3514757 ymax: 11288740
# Projected CRS: WGS 84 / UTM zone 37S
# # A tibble: 10 × 10
#    Eco_code Ecoregion Prov_code Province Rlm_code Realm Alt_code
#  *    <dbl> <chr>         <dbl> <chr>       <dbl> <chr>    <dbl>
#  1    20097 Cargados…        20 Western…        5 West…       83
#  2    20093 Central …        19 Somali/…        5 West…       93
#  3    20102 Delagoa          20 Western…        5 West…       84
#  4    20098 Mascaren…        20 Western…        5 West…       85
#  5    20096 Seychell…        20 Western…        5 West…       86
#  6    20099 Southeas…        20 Western…        5 West…       87
#  7    20095 East Afr…        20 Western…        5 West…       88
#  8    20094 Northern…        20 Western…        5 West…       89
#  9    20101 Bight of…        20 Western…        5 West…       90
# 10    20100 Western …        20 Western…        5 West…       91
# # ℹ 3 more variables: Eco_code_x <dbl>, Lat_zone <chr>,

#   geometry <GEOMETRY [m]>
  # # set ecoregions to utm
  #   regional_ecoregions %<>%
  #     st_transform(32737)

  # combine national s segments
    regional_ecoregions %<>%
      group_by(Ecoregion) %>%
      summarise(geometry = geometry %>% st_combine())


 ## -- review coastline object -- ##
  # have a look
    regional_coastline
# Geometry set for 1 feature
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 29.14113 ymin: -29.69891 xmax: 63.49391 ymax: 11.35769
# Geodetic CRS:  WGS 84
# MULTIPOLYGON (((29.14113 -15.85466, 29.14113 -1...

  # set to utm
    regional_coastline %<>%
      st_transform(32737)

  # # combine sectors
    # wio_coastline


 ## -- review criterion a data table object -- ##
  # have a look
    criterion_a_data_table
# # A tibble: 578 × 8
#    Ecoregion      first_year recent_year year_gap no_years   lat
#    <chr>               <dbl>       <dbl>    <dbl>    <dbl> <dbl>
#  1 N Mozambique-…       2015        2015        1        1 -11.1
#  2 N Mozambique-…       2015        2015        1        1 -11.1
#  3 N Mozambique-…       2015        2015        1        1 -11.1
#  4 N Mozambique-…       2015        2015        1        1 -11.3
#  5 N Mozambique-…       2015        2015        1        1 -11.3
#  6 N Mozambique-…       2015        2015        1        1 -10.8
#  7 N Mozambique-…       2015        2015        1        1 -10.8
#  8 N Mozambique-…       2015        2015        1        1 -11.2
#  9 N Mozambique-…       2015        2015        1        1 -10.6
# 10 N Mozambique-…       2015        2015        1        1 -10.6
# # ℹ 568 more rows
# # ℹ 2 more variables: long <dbl>, current_coral_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # generate spatial object
    criterion_a_data_table_sf <-
      criterion_a_data_table %>%
        dplyr::filter(!lat  %>% is.na(),
                      !long %>% is.na()) %>%
        st_as_sf(coords = c("long", "lat"),
                 crs    = 4326)

  # set to utm
    criterion_a_data_table_sf %<>%
      st_transform(32737)

  # # extract ecoregion
  #   criterion_a_data_table_sf %<>%
  #     separate(ecoregion_country,
  #              into  = c("Ecoregion"),
  #              sep   = "_",
  #              extra = "drop")


##
## 3. Visualise
##
 ## -- check ecoregion taxonomy -- ##
  # check ecoregions from criterion a data table
      criterion_a_data_table_sf %>% pull(Ecoregion) %>% unique()
 # [1] "N Mozambique-S Tanzania" "Comoros"
 # [3] "Delagoa"                 "Seychelles north"
 # [5] "Mascarene Isl."          "West Madagascar"
 # [7] "Seychelles Outer"        "N Tanzania-Kenya"
 # [9] "East Madagascar"         "North Madagascar"

  # check reef ecoregions
    regional_coral_reefs %>% pull(Ecoregion) %>% unique()
#  [1] "Cargados Carajos/Tromelin Island"
#  [2] "Central Somali Coast"
#  [3] "Delagoa"
#  [4] "Mascarene Islands"
#  [5] "Seychelles"
#  [6] "Southeast Madagascar"
#  [7] "East African Coral Coast"
#  [8] "Northern Monsoon Current Coast"
#  [9] "Bight of Sofala/Swamp Coast"
# [10] "Western and Northern Madagascar"

  # check list from ecoregions object
    regional_ecoregions %>% pull(Ecoregion) %>% unique()
#  [1] "Bight of Sofala/Swamp Coast"
#  [2] "Cargados Carajos/Tromelin Island"
#  [3] "Central Somali Coast"
#  [4] "Delagoa"
#  [5] "East African Coral Coast"
#  [6] "Mascarene Islands"
#  [7] "Northern Monsoon Current Coast"
#  [8] "Seychelles"
#  [9] "Southeast Madagascar"
# [10] "Western and Northern Madagascar"

 ## -- set colour palette -- ##
  # get list of ecoregions from criterion a data table
    ecoregion_list <-
      criterion_a_data_table_sf %>% pull(Ecoregion) %>% unique()

  # set number of ecoregions
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set criteria b1 eoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

 ## -- plot regional data -- ##
  # set save locale
    figure_locale <- "figures/biological/"

  # create pdf
    CairoPDF(paste0(figure_locale, "gcrmn_percent_cover_stations.pdf"), 7, 7)

  # open window
    # quartz("gcrmn monitoring stations", 7, 7)

  ## -- loop to generate figure -- ##
   # loop            # i=3  ## -- for testing -- ##
     # for(i in 1:length(ecoregion_list)){
     for(i in c(1, 3:length(ecoregion_list))){

       # print progress to screen
         cat(paste0("...processing:  ", ecoregion_list[i],
                    " [ ", i, " of ", length(ecoregion_list), " ]\n"))

       # set ecoregion bounding box
         e_zoom <-
           regional_ecoregions %>%
             dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
             st_bbox()

       # create figure
         p <-
         ggplot() +
           geom_sf(aes(colour = Ecoregion,
                       fill   = Ecoregion),
                   alpha = 0.2,
                   data   = regional_ecoregions %>%
                              st_crop(e_zoom)) +
           geom_sf(fill   = r_colour,
                   colour = r_colour,
                   size   = 0.1,
                   alpha  = 0.5,
                   data   = regional_coral_reefs %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           geom_sf(fill   = "grey75",
                   colour = "grey75",
                   size   = 0.1,
                   data   = regional_coastline %>%
                              st_crop(e_zoom)) +
           geom_sf(# aes(colour = c_colour[i]),
                   # colour = c_colour[i],
                   fill   = NA,
                   size   = 1.5,
                   alpha  = 0.4,
                   data   = criterion_a_data_table_sf %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           theme_void() +
           coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                    ylim = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32737)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = ecoregion_list[i],
                subtitle = "Percent cover sites") +
           theme(legend.position = "none",
                 plot.title      = element_text(hjust = 0.5),
                 plot.subtitle   = element_text(hjust = 0.5,
                                                face  = "italic"))

    # plot image
      p %>% print()


   }

   # close device
     dev.off()


##
## 3. Clean up workspace
##
  # remove paths
    rm(data_locale,
       figure_locale)

  # remove plotting variables
    rm(ecoregion_of_interest,
       # crs_details,
       e_zoom,
       c_palette,
       r_colour,
       c_colour)

  # remove intermediate objects
    rm(regional_coral_reefs,
       regional_coastline,
       regional_ecoregions)

  # remove core objects
    rm(criterion_a_data_table_sf)

