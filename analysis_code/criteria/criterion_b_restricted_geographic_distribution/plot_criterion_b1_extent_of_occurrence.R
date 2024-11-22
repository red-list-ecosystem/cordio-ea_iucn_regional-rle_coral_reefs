##
##  Name:       plot_criterion_b1_extent_of_occurrence.R
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

##  Notes:      1. Changing projection to epsg:32737 instead of wio_crs, but
##                 need to confirm with james                 [ fs: 2024-03-13 ]
##              2. Should add `bquote()` or ēxpression() for superscript
##                 for area labels

##
## 1. Set up
##
 # # -- call to custom wio ecoregions -- ##
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


 ## -- point to ecoregions + custom wio coral reefs -- ##
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

 ## -- calculate criterion b1 exent of occurrence -- ##
  # get list of ecosystem units
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

 ## -- create empty object to hold results -- ##
  # extent of occurrence
    # eco_eoo <- tibble()
    eco_eoo <- list()

  # label centroids
    area_centroid <- tibble()

  # area
    eoo_area <- tibble()

  # loop to calculate eoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

      # print progress to screen
        cat(paste0("...processing ", ecoregion_list[i], " [ ",
                   i, " of ", length(ecoregion_list), " ]\n"))

      # create eoo object
        eco_eoo[[i]] <-
          regional_coral_reefs %>%
            dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
            st_transform(32737) %>%
            st_union() %>%
            # st_collection_extract("POLYGON") %>%
           as_Spatial() %>%
           redlistr::makeEOO()

     ## -- set area annotation -- ##
       # get centroid
         # area_centroid[[i]] <-
           dat <-
           eco_eoo[[i]] %>%
             st_as_sf() %>%
             st_transform(32737) %>%
             st_centroid() %>%
             st_coordinates()

       # # get coordinates
         # area_coordinates <-
           # area_centroid %>%
             # st_coordinates()

       # harvest results
         area_centroid %<>%
           bind_rows(dat %>%
                       data.frame() %>% tibble() %>%
                       mutate(Ecoregion = ecoregion_list[i]))

     ## -- calculate area -- ##
       # get area
         dat <-
           eco_eoo[[i]] %>%
             st_as_sf() %>%
             st_transform(32737) %>%
             st_area()

       # convert to km2
         dat %<>%
           tibble() %>%
           mutate(Ecoregion = ecoregion_list[i]) %>%
           rename(Area = ".") %>%
           mutate(Area = Area %>% as.numeric() / 1e3,
                  Area = Area %>% round(0))

       # harvest results
         eoo_area %<>%
           bind_rows(dat)


    }

 ## -- combine objects -- ##
  # combine centoids and area
    area_centroid %<>%
      left_join(eoo_area)

  # put in order
    area_centroid %<>%
      dplyr::select(Ecoregion,
                    easting  = X,
                    northing = Y,
                    Area)

  # have a look
    area_centroid
# # A tibble: 10 × 4
   # Ecoregion                         easting  northing       Area
   # <chr>                               <dbl>     <dbl>      <dbl>
 # 1 Cargados Carajos/Tromelin Island 2471355.  8331402.  218570512
 # 2 Central Somali Coast             1284644. 10276855.     246429
 # 3 Delagoa                            37180.  7314818.   63445150
 # 4 Mascarene Islands                2608667.  7618350.   73584335
 # 5 Seychelles                       1968018.  9153677.  408229983
 # 6 Southeast Madagascar             1413200.  7482762.   22787127
 # 7 East African Coral Coast          636898.  9024968.  252240131
 # 8 Northern Monsoon Current Coast    960470.  9993733.   23211727
 # 9 Bight of Sofala/Swamp Coast       419764.  7987621.   17850083
# 10 Western and Northern Madagascar  1156300.  8029750. 1166581856

  # convert areas
    area_centroid %<>%
      mutate(Area = Area / 1e3)

  # remove decimals
    area_centroid %<>%
      mutate(Area = Area %>% round(0))


##
## 3. Visualise
##
 ## -- set colour palette -- ##
   # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

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
    figure_locale <- "figures/criteria/criterion_b_restricted_geographic_distribution/"

  # create pdf
    CairoPDF(paste0(figure_locale, "criterion_b1_extent_of_occurrence.pdf"), 7, 7)

  # open window
    # quartz("criterion b1 eoo", 7, 7)

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
           # geom_sf(aes(colour = Ecoregion,
                       # fill   = Ecoregion),
                   # alpha = 0.2,
                   # data   = regional_ecoregions %>%
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
           geom_sf(# aes(colour = c_colour[i]),
                   colour = c_colour[i],
                   fill   = NA,
                   size   = 0.5,
                   alpha  = 0.4,
                   data   = eco_eoo[[i]] %>%
                              st_as_sf() %>%
                              st_transform(32737)) +
      annotate(x      = area_centroid[i, ]$easting,
               y      = area_centroid[i, ]$northing,
               "text",
               label = paste0("EOO = ", area_centroid[i, ]$Area, " ", expression(km^2))) +
      theme_void() +
      coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
               ylim = c(e_zoom[2], e_zoom[4]),
               datum = st_crs(32737)) +
      ggspatial::annotation_scale() +
      scale_colour_manual(values = c_palette) +
      scale_fill_manual(values = c_palette) +
      labs(title    = ecoregion_list[i],
           subtitle = "Criterion B1: Extent of Occurrence") +
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
    rm(e_zoom,
       c_palette,
       r_colour,
       c_colour)

  # remove intermediate objects
    rm(# regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

