##
##  Name:       plot_criterion_b2_area_of_occupancy_regional.R
##
##  Objective:  Visualise extent of occurrence for wio region
##
##  Approach:   Call to regional benthic taxa, wio coasline and
##              visualise.
##
##              Outputs saved as *.png
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-04-28
##

##  Notes:      1. Changing projection to epsg:32737 instead of wio_crs, but
##                 need to confirm with james                 [ fs: 2024-03-13 ]
##              2. Should add `bquote()` or ēxpression() for superscript
##                 for area labels

##
## 1. Set up
##
 ## -- call to custom wio ecoregions -- ##
  # # point to data locale
    # data_locale <- "data_intermediate/spatial/ecoregions/"

  # # point to data file
    # data_file <- "regional_ecoregions.rda"

  # # call to ecoregions
    # load(paste0(data_locale, data_file))


 ## -- point to regional coastline -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # import coastline
    load(paste0(data_locale, data_file))


 ## -- point to ecoregion combined coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # call to coral reefs
    load(paste0(data_locale, "regional_coral_reefs.rda"))


##
## 2. Groom data
##
  # have a look
    regional_coral_reefs
# Simple feature collection with 21 features and 4 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -131055.3 ymin: 6893684 xmax: 3131589 ymax: 9818958
# Projected CRS: WGS 84 / UTM zone 37S
# # A tibble: 21 × 5
   # Ecoregion          National_s Eco_ID Natnl_ID                  geometry
   # <chr>              <chr>       <int>    <int>        <MULTIPOLYGON [m]>
 # 1 N Tanzania-Kenya   Kilifi          1        3 (((639208.1 9691406, 639…
 # 2 East Madagascar    East Mada…      7        5 (((1715961 8336573, 1715…
 # 3 Mascarene Isl.     Mascarene…     11        6 (((2497484 7695516, 2497…
 # 4 Delagoa            Delagoa A       3        7 (((118306.3 7627246, 118…
 # 5 Seychelles Outer   Seychelle…      9        8 (((1837676 9007115, 1837…
 # 6 South Madagascar   South Mad…      8        9 (((1453261 7566036, 1453…
 # 7 Seychelles north   Seychelle…     10       10 (((2378871 9562749, 2378…
 # 8 N Mozambique-S Ta… N Mozmbiq…      2       11 (((669221.5 8836593, 669…
 # 9 Comoros            Comoros         4       12 (((966013.8 8729730, 966…
# 10 North Madagascar   West Mada…      5       13 (((1353495 8312800, 1353…
# # ℹ 11 more rows
# # ℹ Use `print(n = ...)` to see more rows


 ## -- calculate criterion b2 area of occupancy -- ##
  # get list of ecosystem units
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()


 ## -- set parameters for area analyses -- ##
  # set grid size [ 10 km ]
    grid_size <- 10e3

  # set number of improvements
    n_improvements <- 5

  # set percent threshold
    p_thresh <- 1

  # set minimum percent threshold
    m_thresh <-
      1e-4

  # set buffer distance for linear features
    b_dist <-
     300

 ## -- create empty object to hold results -- ##
  # extent of occurrence
    aoo_grid <- list()

  # label centroids
    area_centroid <- tibble()

  # aoo
    eco_aoo <- tibble()

  # loop to calculate aoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

     # print progress to screen
       cat(paste0("...processing:  ", ecoregion_list[i], " [ ",
                  i, " of ", length(ecoregion_list), " ]\n" ))

     # create area of occurrence grid
       aoo_grid[[i]] <-
          regional_coral_reefs %>%
            dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32737) %>%
          st_union() %>%
          st_buffer(dist = b_dist) %>%
          as_Spatial() %>%
         redlistr::makeAOOGrid(grid.size        = grid_size,
                               min.percent.rule = TRUE,
                               percent          = m_thresh)

     # convert to sf
       aoo_grid[[i]] %<>%
         st_as_sf() %>%
         st_set_crs(32737)

    ## -- get grid uncertainty -- ##
     # calculate
       grid_uncertainty <-
         aoo_grid[[i]] %>%
           as_Spatial() %>%
           redlistr::gridUncertainty(grid.size         = grid_size,
                                     n.AOO.improvement = n_improvements)

     # get min grid uncertainty
       min_aoo_number <-
         grid_uncertainty$min.AOO.grid$AOO.number

     # calculate uncertainty and apply 1% rule
       aoo <-
         regional_coral_reefs %>%
           dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
           st_transform(32737) %>%
           st_union() %>%
           as_Spatial() %>%
         redlistr::getAOO(grid.size        = grid_size,
                          min.percent.rule = TRUE,
                          percent          = p_thresh)

     # set names
       aoo_dat <-
         tribble(       ~Ecoregion,  ~`Grid Uncertainty`, ~`AOO`,
                 ecoregion_list[i],       min_aoo_number,    aoo)

      # harvest results
        eco_aoo %<>%
          bind_rows(aoo_dat)

     ## -- set area annotation -- ##
       # get centroid
         dat <-
           aoo_grid[[i]] %>%
             st_as_sf() %>%
             st_transform(32737) %>%
             st_centroid() %>%
             st_coordinates()

       # harvest results
         area_centroid %<>%
           bind_rows(dat %>%
                       data.frame() %>% tibble() %>%
                       mutate(Ecoregion = ecoregion_list[i]))


     }

 ## -- summarise object -- ##
  # put in order
    area_centroid %<>%
      group_by(Ecoregion) %>%
      summarise(easting  = X %>% mean(na.rm = TRUE),
                northing = Y %>% mean(na.rm = TRUE))

 ## -- combine objects -- ##
  # combine centoids and area
    area_centroid %<>%
      left_join(eco_aoo)

  # set zeros to 1   ## -- manual fix before testing polygon errors -- ##
    area_centroid %<>%
      mutate(AOO = ifelse(AOO == 0, 1, AOO))

  # set ecoregion order
    area_centroid %<>%
      mutate(Ecoregion = Ecoregion %>% factor(levels = ecoregion_list)) %>%
      arrange(Ecoregion)

  # have a look
    area_centroid
# # A tibble: 10 × 5
#    Ecoregion                easting northing `Grid Uncertainty`   AOO
#    <fct>                      <dbl>    <dbl>              <int> <int>
#  1 Cargados Carajos/Tromel…  2.66e6   8.21e6                 25    19
#  2 Central Somali Coast      1.28e6   1.03e7                  6     1
#  3 Delagoa                   8.19e4   7.37e6                 28    12
#  4 Mascarene Islands         2.57e6   7.63e6                 45    36
#  5 Seychelles                1.98e6   9.17e6                128   103
#  6 Southeast Madagascar      1.43e6   7.51e6                 17     4
#  7 East African Coral Coast  6.00e5   9.04e6                421   305
#  8 Northern Monsoon Curren…  9.15e5   9.95e6                 70    40
#  9 Bight of Sofala/Swamp C…  5.43e5   8.13e6                 30    20
# 10 Western and Northern Ma…  1.30e6   8.19e6                490   323


##
## 3. Visualise
##
 ## -- set colour palette -- ##
   # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # get list of ecosystem units
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

  # set regional bounding box
    e_zoom <-
      regional_coral_reefs %>%
        st_transform(32737) %>%
        st_bbox()

 ## -- combine aoo polygons -- ##
  # create empty object to hold results
    criterion_b2_polygons <- st_sfc(crs = 32737) %>% st_sf()

  # loop                 # i=1 ## -- for testing -- ##
    for(i in 1:length(aoo_grid)){

      # subset object and convert to sf
        dat <-
          aoo_grid[[i]]

      # add ecoregion
        dat %<>%
          mutate(Ecoregion = ecoregion_list[i])

      # organise columns
        dat %<>%
          dplyr::select(Ecoregion,
                        cover,
                        geometry)

      # harvest results
        criterion_b2_polygons %<>%
          rbind(dat)


    }


 ## -- plot regional data -- ##
  # set region name
    region_name <- "Western Indian Ocean"

  # open window
    # quartz("criterion b2 aoo", 7, 7)

    # create figure
      ggplot() +
        # geom_sf(aes(colour = Ecoregion,
                  # fill   = Ecoregion),
               # alpha = 0.2,
               # data   = wio_eco_rgns %>%
                         # st_transform(32737) %>%
                         # st_crop(e_zoom)) +
        geom_sf(fill   = r_colour,
                colour = r_colour,
                size   = 0.1,
                alpha  = 0.5,
                data   = regional_coral_reefs %>%
                           filter(Ecoregion %in% ecoregion_list[i]) %>%
                           st_transform(32737))  +
        geom_sf(fill   = "grey75",
                colour = "grey75",
                size   = 0.1,
                data   = regional_coastline %>%
                           st_transform(32737) %>%
                           st_crop(e_zoom)) +
        geom_sf(aes(colour = Ecoregion),
                fill   = NA,
                size   = 0.5,
                alpha  = 0.1,
                data   = criterion_b2_polygons) +
         geom_sf_text(aes(# colour = Ecoregion,
                          label  = paste0(Ecoregion, "\n",
                                          "AOO = ", AOO,
                                          " x10 km^2 units")),
                      position = "jitter",
                      size  = 2.5,
                      data  = area_centroid %>%
                                st_as_sf(coords = c("easting", "northing"),
                                         crs   = 32737)) +
         theme_void() +
         coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                  ylim = c(e_zoom[2], e_zoom[4]),
                  datum = st_crs(32737)) +
         ggspatial::annotation_scale() +
         scale_colour_manual(values = c_palette) +
         scale_fill_manual(values = c_palette) +
         labs(title    = region_name,
              subtitle = "Criterion B2: Area of Occupancy") +
         theme(legend.position = "none",
               plot.title      = element_text(hjust = 0.5),
               plot.subtitle   = element_text(hjust = 0.5,
                                              face  = "italic"))


 ## -- plot regional data -- ##
  # set save locale
    figure_locale <-
      "figures/criteria/criterion_b_restricted_geographic_distribution/"

  # save to file
    ggsave(paste0(figure_locale, "criterion_b2_area_of_occupancy_regional.png"),
      width  = 7,
      height = 7)


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       figure_locale)

  # remove plotting variables
    rm(ecoregion_list,
       e_zoom,
       c_palette,
       r_colour,
       c_colour,
       region_name)

  # remove intermediate objects
    rm(#regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

