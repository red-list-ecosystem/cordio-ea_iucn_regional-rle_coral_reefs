##
##  Name:       plot_number_of_collapsed_sites.R
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
 ## -- call to sensitivity analyses-- ##
  # point to data locale
    data_locale <- "data_intermediate/criteria/"

  # point to data file
    data_file <- "criterion_a_reduction_geographic_distribution.rda"

  # load data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    criterion_a_reduction_geographic_distribution
# # A tibble: 220 × 5
   # Ecoregion                 threshold Variable prop_collapse status
   # <chr>                         <dbl> <chr>            <dbl> <chr>
 # 1 Comoros                           1 Unweigh…          0    LC
 # 2 Delagoa                           1 Unweigh…          0.02 LC
 # 3 East Madagascar                   1 Unweigh…          0    LC
 # 4 Mascarene Isl                     1 Unweigh…          0    LC
 # 5 N Mozambique - S Tanzania         1 Unweigh…          0    LC
 # 6 N Tanzania - Kenya                1 Unweigh…          0    LC
 # 7 North Madagascar                  1 Unweigh…          0    LC
 # 8 Seychelles North                  1 Unweigh…          0.04 LC
 # 9 Seychelles Outer                  1 Unweigh…          0.09 LC
# 10 West Madagascar                   1 Unweigh…          0    LC
# # ℹ 210 more rows
# # ℹ Use `print(n = ...)` to see more rows

  # review ecoregions
    criterion_a_reduction_geographic_distribution %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                   "Delagoa"
 # [3] "East Madagascar"           "Mascarene Isl"
 # [5] "N Mozambique - S Tanzania" "N Tanzania - Kenya"
 # [7] "North Madagascar"          "Seychelles North"
 # [9] "Seychelles Outer"          "West Madagascar"
# [11] "Region"


##
## 3. Visualise
##
  # set region name
    region_name <- "Western Indian Ocean"

  # set variable of interest
    variable_of_interest <- "Unweighted"

  # get list of regions
    ecoregion_list <-
      criterion_a_reduction_geographic_distribution %>% pull(Ecoregion) %>% unique()

 ## -- create figure -- ##
  # open window
    # quartz("number of collapsed sites", 7, 7)

  # plot
    criterion_a_reduction_geographic_distribution %>%
      dplyr::filter(Variable %in% variable_of_interest) %>%
    ggplot(aes(threshold,
               prop_collapse,
               group     = Ecoregion,
               colour    = Ecoregion),
               linewidth = 2) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.8,
                ymin   = 0.00,
                ymax   = 0.25,
                fill   = "lightgreen",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.8,
                ymin   = 0.25,
                ymax   = 0.30,
                fill   = "lightblue",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.5,
                ymin   = 0.30,
                ymax   = 0.50,
                fill   = "lightyellow",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.8,
                ymin   = 0.50,
                ymax   = 0.80,
                fill   = "orange",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.6,
                ymin   = 0.80,
                ymax   = 1,
                fill   = "red",
                colour = NA) +
       geom_line() +
       geom_point(aes(shape = Ecoregion),
                  size = 2,
                  fill = "white") +
       scale_shape_manual(values = 1:(length(ecoregion_list) + 1)) +
       scale_colour_manual(values = 1:(length(ecoregion_list) + 1)) +
       theme_minimal() +
       scale_x_continuous(breaks = seq(1, 10, by = 1))+
       ylab('Proportion of collapsed sites') +
       xlab('% Hard coral cover threshold') +
       ggtitle(region_name) +
       labs(colour = "Ecoregion",
            shape  = "Ecoregion")+
       annotate("label", x = 1, y = 0.07, label = "LC") +
       annotate("label", x = 1, y = 0.27, label = "NT") +
       annotate("label", x = 1, y = 0.35, label = "VU") +
       theme(plot.title = element_text(hjust = 0.5))

 ## -- save for wiki -- ##
  # set save locale
    figure_locale <- "figures/criteria/criterion_a_reduction_geographic_distribution/"

  # print to file
    ggsave(paste0(figure_locale, "number_of_collapsed_sites.png"),
      width  = 7,
      height = 7)


##
## 3. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       figure_locale)

  # remove intermediate objects
    rm(# ecoregion_list,
       region_name,
       variable_of_interest)

  # remove core objects
    rm(criterion_a_reduction_geographic_distribution)

