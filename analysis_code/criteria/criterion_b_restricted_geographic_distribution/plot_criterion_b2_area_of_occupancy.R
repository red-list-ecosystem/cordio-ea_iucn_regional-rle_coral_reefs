##
##  Name:       plot_criterion_b2_area_of_occupancy.R
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
##  Date:       2024-03-13
##

##
## 1. Set up
##
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
# Simple feature collection with 1349 features and 4 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.86711 ymin: -27.01737 xmax: 63.51225 ymax: 2.755531
# Geodetic CRS:  WGS 84
# # A tibble: 1,349 × 5
   # Ecoregion    Eco_code Province Prov_code                  geometry
   # <chr>           <dbl> <chr>        <dbl>            <GEOMETRY [°]>
 # 1 Cargados Ca…    20097 Western…        20 POLYGON ((56.59693 -10.3…
 # 2 Cargados Ca…    20097 Western…        20 POLYGON ((56.67842 -10.4…
 # 3 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.51909…
 # 4 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.50409…
 # 5 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.50409…
 # 6 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.51485…
 # 7 Cargados Ca…    20097 Western…        20 POLYGON ((54.51764 -15.9…
 # 8 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((59.49749…
 # 9 Cargados Ca…    20097 Western…        20 POLYGON ((59.57061 -16.7…
# 10 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((59.48067…
# # ℹ 1,339 more rows
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

 ## -- combine objects -- ##
  # combine centoids and area
    area_centroid %<>%
      left_join(eco_aoo)

  # put in order
    area_centroid %<>%
      group_by(Ecoregion) %>%
      summarise(X = X %>% mean(na.rm = TRUE),
                Y = Y %>% mean(na.rm = TRUE),
                `Grid Uncertainty` = `Grid Uncertainty` %>% mean(na.rm = TRUE),
                AOO = AOO %>% mean(na.rm = TRUE))

  # set zeros to 1   ## -- manual fix before testing polygon errors -- ##
    area_centroid %<>%
      mutate(AOO = ifelse(AOO == 0, 1, AOO))

  # have a look
    area_centroid
# # A tibble: 10 × 5
   # Ecoregion                        X      Y `Grid Uncertainty`   AOO
   # <chr>                        <dbl>  <dbl>              <dbl> <dbl>
 # 1 Bight of Sofala/Swamp Coast 5.43e5 8.13e6                 30    20
 # 2 Cargados Carajos/Tromelin … 2.66e6 8.21e6                 25    19
 # 3 Central Somali Coast        1.28e6 1.03e7                  6     1
 # 4 Delagoa                     8.19e4 7.37e6                 28    12
 # 5 East African Coral Coast    6.00e5 9.04e6                421   305
 # 6 Mascarene Islands           2.57e6 7.63e6                 45    36
 # 7 Northern Monsoon Current C… 9.15e5 9.95e6                 70    40
 # 8 Seychelles                  1.98e6 9.17e6                128   103
 # 9 Southeast Madagascar        1.43e6 7.51e6                 17     4
# 10 Western and Northern Madag… 1.30e6 8.19e6                490   323

  # set ecoregion order
    area_centroid %<>%
      mutate(Ecoregion = Ecoregion %>% factor(levels = ecoregion_list)) %>%
      arrange(Ecoregion)


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

  # set criteria b2 aoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

 ## -- plot regional data -- ##
  # set save locale
    figure_locale <- "figures/criteria/criterion_b_restricted_geographic_distribution/"

  # create pdf
    CairoPDF(paste0(figure_locale, "criterion_b2_area_of_occupancy.pdf"), 7, 7)


  # open window
    # quartz("comoros b2 aoo", 7, 5)

  ## -- loop to generate figure -- ##
   # loop            # i=3  ## -- for testing -- ##
     for(i in 1:length(ecoregion_list)){

       # print progress to screen
         cat(paste0("...processing:  ", ecoregion_list[i],
                    " [ ", i, " of ", length(ecoregion_list), " ]\n"))

       # set ecoregion bounding box
         e_zoom <-
           regional_coral_reefs %>%
             dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
             st_transform(32737) %>%
             st_bbox()

       # create figure
         p <-
         ggplot() +
          # geom_sf(aes(colour = ECOREGION,
                     # fill   = ECOREGION),
                 # alpha = 0.2,
                 # data   = wio_eco_rgns %>%
                            # st_transform(32737) %>%
                            # st_crop(e_zoom) %>%
                            # dplyr::select(ECOREGION)) +
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
         geom_sf(aes(colour = c_colour[1]),
                 fill   = NA,
                 size   = 0.5,
                 alpha  = 0.4,
                 data   = aoo_grid[[i]]) +
         annotate(x      = area_centroid[i, ]$X,
                  y      = area_centroid[i, ]$Y,
                  # colour = c_colour[i],
                  colour = c_colour,
                  "text",
                  label = paste0("AOO = ", area_centroid[i, ]$AOO, " x10 km^2 units")) +
         theme_void() +
         coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                  ylim = c(e_zoom[2], e_zoom[4]),
                  datum = st_crs(32737)) +
         ggspatial::annotation_scale() +
         scale_colour_manual(values = c_palette) +
         scale_fill_manual(values = c_palette) +
         labs(title    = ecoregion_list[i],
              subtitle = "Criterion B2: Area of Occupancy") +
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
       c_colour)

  # remove intermediate objects
    rm(# regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

