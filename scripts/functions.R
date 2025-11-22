# functions to get counts ------
get_species_count_ibra <- function(ibra_region) {
  
  galah_call() |> 
    apply_profile(ALA) |> 
    filter(cl1048 == ibra_region) |> 
    atlas_species() |> 
    mutate(ibra_region = ibra_region) |> 
    count(ibra_region) |> 
    ungroup()
}

get_species_count_hex <- function(hex) {

    # deals with the weirdness of elements and 
    # columns being different classes ¯\_(ツ)_/¯ 
    if (inherits(hex, "sfg")) {
      hex <- st_sfc(hex, crs = 4326)
    }
    
    galah_call() |>
      apply_profile(ALA) |>
      geolocate(hex) |>
      atlas_counts(type = "species", limit = NULL)
}

get_temporal_counts <- function(hex, year_start, year_end) {
  
  # deals with the weirdness of elements and 
  # columns being different classes ¯\_(ツ)_/¯ 
  if (inherits(hex, "sfg")) {
    hex <- st_sfc(hex, crs = 4326)
  }
  
  galah_call() |>
    apply_profile(ALA) |>
    geolocate(hex) |>
    filter(year >= year_start,
           year <= year_end) |> 
    atlas_counts(type = "species", limit = NULL)

}

preserved_specs <- function(hex, year_start, year_end) {
  
  # deals with the weirdness of elements and 
  # columns being different classes ¯\_(ツ)_/¯ 
  if (inherits(hex, "sfg")) {
    hex <- st_sfc(hex, crs = 4326)
  }
  
  galah_call() |>
    apply_profile(ALA) |>
    geolocate(hex) |>
    filter(year >= year_start,
           year <= year_end,
           basisOfRecord == "PRESERVED_SPECIMEN") |> 
    atlas_counts(type = "species", limit = NULL)
  
}

# plotting functions -------
plot_spp_richness_temporal<- function(df) {
  
  year_start <- unique(df$year_start)
  year_end <- unique(df$year_end)
  
  ggplot() +
    geom_sf(data = df,
            aes(fill = log(hex_count)),
            colour = NA) +
    scale_fill_gradientn(name = str_glue("Number of records: {year_start} - {year_end}"),
                         colours = c("#EBCF2Ebb", "#EBCF2EFF", "#B4BF3AFF", 
                                     "#88AB38FF", "#5E9432FF", "#3B7D31FF", 
                                     "#225F2FFF", "#244422FF", "#252916FF"),
                         na.value = "grey85",
                         limits = c(overall_min, overall_max),   
                         breaks = c(overall_min, overall_max),
                         labels = c("Low", "High"),
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position = "top",
                                                title.hjust = 0.5)) +
    coord_sf(clip = "off") +
    theme_void() +
    theme(text = element_text(family = "roboto", size = 50),
          legend.title = element_text(size = 55),
          legend.ticks = element_blank(),
          legend.key.width = unit(22, 'mm'),
          legend.key.height = unit(6, "mm"),
          legend.position = c(0.45, -0.05),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
}

plot_heatmap <- function(df, legend_title) {
  
  ggplot(df, 
         aes(x = year, 
             y = reorder(state_abbr, desc(state_abbr)), 
             fill = count)) + 
    geom_tile(colour = "white",
              lwd = 0.2) +
    geom_vline(xintercept = 2022-0.5) +
    annotate("text", 
             x = 2007, 
             y = 10, 
             label = "Christmas Beetle Count starts", 
             size = 4) +
    scale_x_continuous(breaks=seq(1900, 2020, 10)) +
    scale_fill_distiller(name = legend_title,
                         type = "seq",
                         palette = "YlOrBr",
                         direction = 1,
                         trans = "log",
                         na.value = "grey90",
                         limits = beetle_count_range,
                         breaks = scales::breaks_log(n = 5),
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position = "top",
                                                title.hjust = 0.5)) +
    coord_equal(clip = "off") +
    theme_classic() +
    theme(text = element_text(family = "roboto", size = 8),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.y = element_text(hjust = 1, margin = margin(r = -35)),
          legend.ticks = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(20, 'mm'),
          legend.key.height = unit(4, "mm"),
          legend.title = element_text(face = "bold", size = 12),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA), 
          legend.background = element_rect(fill = NA, colour = NA))

}
