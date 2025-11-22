# libraries, config -------
library(galah)
library(ozmaps)
library(sf)
library(tidyverse)
library(readxl)
library(here)
library(showtext)
library(ggridges)
library(waffle)
library(marquee)

galah_config(email = Sys.getenv("ALA_EMAIL"))
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 350)

#TODO: move functions into functions.R

# title --------

insect_order_counts <- galah_call() |> 
  identify("insecta") |> 
  group_by(order) |> 
  atlas_counts()

insect_order_counts |> 
  arrange(order) |> 
  rowwise() |> 
  mutate(random = as.factor(ceiling(runif(1, 1, 8))),
         y = runif(1, -0.2, 0.2)) |>
  ggplot() +
  geom_hline(yintercept = 0, 
             colour = "#ffbc4199", 
             linewidth = 0.3) +
  geom_jitter(aes(x = order, 
                  y = y, 
                  size = count, 
                  fill = random),
              shape = 21,
              alpha = 0.7, 
              colour = "#fdf9ec") +
  scale_fill_manual(values = MetBrewer::met.brewer("Tam")) +
  scale_size(range = c(10, 75)) +
  scale_x_discrete(expand = expansion(add = 0.9)) +
  ylim(-1, 1) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))

ggsave(here("plots", "title.png"), height = 8, width = 14, units = "in")

# acknowledgement ---------

insect_order_counts |> 
  arrange(order) |> 
  rowwise() |> 
  mutate(random = as.factor(ceiling(runif(1, 1, 8))),
         y = runif(1, 0, 0.5)) |>
  ggplot() +
  geom_jitter(aes(x = order, 
                  y = y, 
                  size = count, 
                  fill = random, 
                  alpha = desc(y)),
              shape = 21,
              colour = "#fdf9ec") +
  scale_fill_manual(values = MetBrewer::met.brewer("Tam")) +
  scale_size(range = c(5, 50)) +
  scale_x_discrete(expand = expansion(add = 0.9)) +
  ylim(0, 0.6) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))

ggsave(here("plots", "ack.png"), height = 4, width = 12, units = "in")

# emu <- galah_call() |> 
#   identify("dromaius novaehollandiae") |> 
#   group_by(cl1048, month) |> 
#   atlas_counts() |> 
#   filter(cl1048 %in% c("Flinders Lofty Block",
#                        "Broken Hill Complex", 
#                        "Gawler",
#                        "Murray Darling Depression")) 
# 
# ggplot(emu, aes(x = count, y = reorder(cl1048, count))) +
#   geom_density_ridges(scale = 1.8, 
#                       fill = "#a47823", 
#                       colour = "#fdf9ec") +
#   theme_void() +
#   theme(plot.margin = margin(0.5, 0, 0, 0, "cm"),
#         plot.background = element_rect(fill = "#fdf9ec", colour = "NA"))
# 
# ggsave(here("plots", "ridges.png"), width = 6, height = 1.5, units = "in")


# spatial and temporal gaps ----------

### 1. set up terrestrial grid -----
hex_grid <- st_make_grid(ozmap_country,
                         cellsize = 1,
                         what = "polygons",
                         square = FALSE,
                         flat_topped = TRUE) |> 
  st_as_sf() |> 
  st_set_geometry("hex_geometry") |> 
  st_transform(4326) |> 
  rowid_to_column(var = "hex_id")

aus_hex <- st_join(x = hex_grid, 
                   y = st_transform(ozmap_country, 4326), 
                   join = st_intersects, 
                   left = FALSE) |> 
  select(-NAME)

### 2. get species counts for IBRA regions and hexagons ------
ibra <- st_read(here("data", "ibra7", "ibra7_regions.shp")) |> 
  st_transform(4326) |> 
  st_make_valid() |> 
  select(REG_NAME_7)

# species_counts_ibra <- map(ibra$REG_NAME_7, get_species_count_ibra) |>
#   bind_rows()
# saveRDS(species_counts_ibra, here("data", "species_counts_ibra.RDS"))
species_counts_ibra <- readRDS(here("data", "species_counts_ibra.RDS"))

# spatial counts
# species_counts_hex <- aus_hex |>
#   mutate(species_count = map(hex_geometry, get_species_count_hex)) |>
#   unnest(cols = species_count)
# saveRDS(species_counts_hex, here("data", "species_counts_hex.RDS"))
species_counts_hex <- readRDS(here("data", "species_counts_hex.RDS"))

# temporal counts
decades <- tibble(year_start = seq(1901, 2011, by = 10),
                  year_end = seq(1910, 2020, by = 10))

# preserved_by_decade <- decades |>
#   mutate(data = pmap(list(year_start, year_end),
#                      \(start, end) {
#                        aus_hex |>
#                          mutate(species_count = map(hex_geometry, preserved_specs,
#                                                     year_start = start, year_end = end))
#                      })) |>
#   unnest(cols = data) |>
#   unnest(cols = species_count)
# 
# saveRDS(preserved_by_decade, here("data", "preserved_by_decade.RDS"))
preserved_by_decade <- readRDS(here("data", "preserved_by_decade.RDS"))

# TODO: figure out if there's a more readable way of doing this 
# e.g. with two map functions piped into each other 

# species_counts_by_decade <- decades |> 
#   mutate(data = pmap(list(year_start, year_end),
#                      \(start, end) {
#                        aus_hex |> 
#                          mutate(species_count = map(hex_geometry, get_temporal_counts,
#                                                     year_start = start, year_end = end))
#                      })) |> 
#   unnest(cols = data) |> 
#   unnest(cols = species_count)
#saveRDS(species_counts_by_decade, here("data", "species_counts_by_decade.RDS"))
species_counts_by_decade <- readRDS(here("data", "species_counts_by_decade.RDS"))

### 3. assign hexes to IBRA regions based on area of overlap ------

# equal-area CRS for calculating area
ibra <- st_transform(ibra, 3577)
aus_hex <- st_transform(aus_hex, 3577)

overlaps <- st_intersection(aus_hex, ibra) |> 
  mutate(area_overlap = st_area(hex_geometry))

hex_to_ibra <- overlaps |> 
  group_by(hex_id) |> 
  slice_max(area_overlap, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  select(hex_id, ibra_region = REG_NAME_7) |>
  st_drop_geometry()

# spatial
prop_count_hex_ibra <- hex_to_ibra |> 
  full_join(species_counts_hex, by = "hex_id") |> 
  st_as_sf(sf_column_name = "hex_geometry") |> 
  rename(hex_count = count) |> 
  full_join(species_counts_ibra, by = "ibra_region") |> 
  rename(ibra_count = n) |> 
  filter(!is.na(hex_id)) |> 
  mutate(prop_species = hex_count/ibra_count)

# temporal
# because hexes without counts are missing from data 
# could get around this by making sure the function adds zeros where necessary
all_hex_decades_combos <- aus_hex |> 
  left_join(hex_to_ibra, by = "hex_id") |> 
  crossing(decades)
# 
# prop_count_decades <- all_hex_decades_combos |> 
#   full_join(species_counts_ibra, by = "ibra_region") |> 
#   rename(ibra_count = n) |> 
#   filter(!is.na(hex_id)) |> 
#   full_join(species_counts_by_decade, by = join_by(year_start, year_end, hex_id)) |> 
#   select(-hex_geometry.y) |> 
#   st_as_sf(sf_column_name = "hex_geometry.x") |>
#   rename(hex_count = count,
#          hex_geometry = hex_geometry.x) |> 
#   #mutate(prop_species = hex_count/ibra_count) |> 
#   group_by(year_start) |> 
#   group_split(year_start) %>%
#   set_names(map(., ~.x$year_start[1]))

preserved_counts_decades <- all_hex_decades_combos |> 
  full_join(species_counts_ibra, by = "ibra_region") |> 
  rename(ibra_count = n) |> 
  filter(!is.na(hex_id)) |> 
  full_join(preserved_by_decade, by = join_by(year_start, year_end, hex_id)) |> 
  select(-hex_geometry.y) |> 
  st_as_sf(sf_column_name = "hex_geometry.x") |>
  rename(hex_count = count,
         hex_geometry = hex_geometry.x)   

### 4. plot! -------
# spatial
# unused blue palette
# ("#e0ebeb", "#ccdede", "#abc9c8", "#72aeb6",
#  "#4692b0", "#2f70a1", "#134b73", "#0a3351")

ggplot(data = prop_count_hex_ibra) +
  geom_sf(aes(fill = log(prop_species)),
          colour = NA) +
  scale_fill_gradientn(name = "Proportional species richness",
                       colours = c("#EBCF2Ebb", "#EBCF2EFF", "#B4BF3AFF", 
                                   "#88AB38FF", "#5E9432FF", "#3B7D31FF", 
                                   "#225F2FFF", "#244422FF", "#252916FF"),
                       breaks = c(min(log(prop_count_hex_ibra$prop_species)), 
                                  max(log(prop_count_hex_ibra$prop_species))),
                       labels = c("Low", "High"),
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              title.hjust = 0.5)) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(text = element_text(family = "roboto", size = 18),
        legend.title = element_text(size = 20),
        legend.ticks = element_blank(),
        legend.key.width = unit(22, 'mm'),
        legend.key.height = unit(6, "mm"),
        legend.position = c(0.45, -0.05),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

ggsave(here("plots", "spatial_gaps.png"), height = 10, width = 10, units = "in")

# temporal
# not using GIF, stick with facets instead
# overall_min <- min(unlist(map(preserved_counts_decades, \(x) min(log(x$hex_count), na.rm = TRUE))))
# overall_max <- max(unlist(map(preserved_counts_decades, \(x) max(log(x$hex_count), na.rm = TRUE))))
# temporal_hex_plots <- map(prop_count_decades, plot_spp_richness_temporal)
# 
# plotnames <- map(names(temporal_hex_plots), ~paste0("plots/temporal_hex_", ., ".png")) 
# 
# walk2(plotnames, temporal_hex_plots, ~ggsave(filename = .x, 
#                                       plot = .y, 
#                                       height = 10, 
#                                       width = 10, 
#                                       units = "in"))
# 
# list.files(path = "./plots", pattern = "^temporal_hex_.*png$", full.names = TRUE) |> 
#   map(image_read) |>  # reads each path file
#   image_join() |>  # joins image
#   image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
#   image_write("./plots/temporal_gaps.gif")

decade_labels <- c("1901" = "1901 - 1910",
                   "1911" = "1911 - 1920",
                   "1921" = "1921 - 1930",
                   "1931" = "1931 - 1940",
                   "1941" = "1941 - 1950",
                   "1951" = "1951 - 1960",
                   "1961" = "1961 - 1970",
                   "1971" = "1971 - 1980",
                   "1981" = "1981 - 1990",
                   "1991" = "1991 - 2000",
                   "2001" = "2001 - 2010",
                   "2011" = "2011 - 2020")

ggplot(preserved_counts_decades) +
  geom_sf(aes(fill = log(hex_count)),
          colour = NA) +
  facet_wrap(~ year_start, labeller = as_labeller(decade_labels)) +
  scale_fill_gradientn(name = "Number of species from preserved specimens",
                       colours = c("#EBCF2Ebb", "#EBCF2EFF", "#B4BF3AFF", 
                                   "#88AB38FF", "#5E9432FF", "#3B7D31FF", 
                                   "#225F2FFF", "#244422FF", "#252916FF"),
                       na.value = "grey85",
                       breaks = c(log(min(preserved_counts_decades$hex_count,na.rm = TRUE)),
                                  log(max(preserved_counts_decades$hex_count, na.rm = TRUE))), 
                       labels = c("Low", "High"),
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              title.hjust = 0.5)) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(text = element_text(family = "roboto", size = 18),
        legend.title = element_text(size = 20),
        legend.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(30, 'mm'),
        legend.key.height = unit(6, "mm"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

ggsave(here("plots", "temporal_gaps.png"), height = 9, width = 16, units = "in")

# taxonomic gaps ----------
### 1. records as a proportion of the ALA ---------

# all counts 152791235
all_counts <- galah_call() |>
  atlas_counts() |> 
  pull(count)

# vertebrate classes
vert_counts <- galah_call() |>
  filter(taxonConceptID == search_taxa("Vertebrata")$taxon_concept_id) |>
  group_by(class) |> 
  atlas_counts() 

# inverts
invert_counts <- galah_call() |>
  filter(taxonConceptID == search_taxa("Animalia")$taxon_concept_id,
         taxonConceptID != search_taxa("Vertebrata")$taxon_concept_id) |>
  atlas_counts() 

# plants and fungi
plant_fungi_counts <- galah_call() |> 
  filter(kingdom %in% c("Plantae", "Fungi")) |>
  group_by(kingdom) |> 
  atlas_counts()

prop_of_ala <- invert_counts |> 
  mutate(type = "inverts") |> 
  bind_rows(plant_fungi_counts) |> 
  bind_rows(vert_counts) |> 
  mutate(group = case_when(
    type == "inverts" ~ "Invertebrates",
    kingdom == "Plantae" ~ "Plants",
    kingdom == "Fungi" ~ "Fungi",
    class == "Aves" ~ "Birds",
    class == "Mammalia" ~ "Mammals",
    class == "Reptilia" ~ "Reptiles", 
    class == "Amphibia" ~ "Amphibians",
    .default = "Fish"
  )) |> 
  group_by(group) |> 
  summarise(record_count = sum(count)) |> 
  mutate(prop_ala = record_count/all_counts*100)

saveRDS(prop_of_ala, here("data", "prop_of_ala.RDS"))
prop_of_ala <- readRDS(here("data", "prop_of_ala.RDS")) 

ggplot(prop_of_ala) + 
  geom_col(aes(x = reorder(group, desc(prop_ala)), 
               y = prop_ala),
           fill = "#5b7749",
           width = 0.8) +
  scale_y_continuous(labels = c("0", "20%", "40%", "60%")) +
  labs(y = "Percentage of records in the ALA") +
  pilot::theme_pilot(grid = "", axes = "l") +
  theme(axis.title.y = element_text(family = "roboto", size = 14),
        axis.text.x = element_text(family = "roboto", size = 10),
        axis.text.y = element_text(family = "roboto", size = 10),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))
  

ggsave(here("plots", "prop_ala.png"), height = 6, width = 9, units = "in")

### 2. records relative to number of species (incomplete) --------
invert_spp_count <- galah_call() |>
  filter(taxonConceptID == search_taxa("Animalia")$taxon_concept_id,
         taxonConceptID != search_taxa("Vertebrata")$taxon_concept_id) |>
  atlas_counts(type = "species", limit = NULL) 

vert_spp_count <- galah_call() |>
  filter(taxonConceptID == search_taxa("Vertebrata")$taxon_concept_id) |>
  group_by(class) |> 
  atlas_counts(type = "species")

plant_fungi_spp_counts <- galah_call() |> 
  filter(kingdom %in% c("Plantae", "Fungi")) |>
  group_by(kingdom) |> 
  atlas_counts(type = "species")

records_per_species <- invert_spp_count |> 
  mutate(type = "inverts") |> 
  bind_rows(plant_fungi_spp_counts) |> 
  bind_rows(vert_spp_count) |> 
  mutate(group = case_when(
    type == "inverts" ~ "Invertebrates",
    kingdom == "Plantae" ~ "Plants",
    kingdom == "Fungi" ~ "Fungi",
    class == "Aves" ~ "Birds",
    class == "Mammalia" ~ "Mammals",
    class == "Reptilia" ~ "Reptiles", 
    class == "Amphibia" ~ "Amphibians",
    .default = "Fish"
  )) |> 
  group_by(group) |> 
  summarise(spp_count = sum(count)) |> 
  full_join(prop_of_ala, by = "group") |> 
  mutate(rec_per_spp = record_count / spp_count)

saveRDS(records_per_species, here("data", "records_per_species.RDS"))
records_per_species <- readRDS(here("data", "records_per_species.RDS"))

ggplot(records_per_species) + 
  geom_col(aes(x = reorder(group, desc(rec_per_spp)), 
               y = rec_per_spp),
           fill = "#5b7749", width = 0.8) +
  geom_text(aes(x = reorder(group, desc(rec_per_spp)), 
                 y = rec_per_spp,
                 label = scales::comma(round(rec_per_spp, 0))), 
                vjust = -0.5, 
            size = 3) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(y = "Average number of records per species") +
  pilot::theme_pilot(grid = "", axes = "l") +
  theme(text = element_text(family = "roboto"),
        axis.title.y = element_text(family = "roboto", size = 14),
        axis.text.x = element_text(family = "roboto", size = 10),
        axis.text.y = element_text(family = "roboto", size = 10),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))

ggsave(here("plots", "rec_per_spp.png"), height = 6, width = 9, units = "in")


# list of insects was downloaded from BIE at Cam's suggestion, by searching for
# "insecta", clicking on the classification tab, then downloading species
# removes "biosecurity species" (species that aren't found in Australia but
# which have been added for alert reporting (on list DR22913)
# TODO: check if there's a way to do this via the APIs

insecta_bie <- read_csv(here("data", "insecta.csv")) |> 
  filter(str_detect(taxonID, "ALA_DR22913_", negate = TRUE))

insecta_spp_counts <- insecta_bie |> 
  select(scientificName, order) |> 
  mutate(ala_counts = map(scientificName, 
                          \(x) galah_call() |>
                            identify(x) |>
                            atlas_counts())) |> 
  unnest(cols = ala_counts)
  
# Christmas beetles (and friends) -----------

beetle_ids <- search_taxa(c("Anoplognathus Leach, 1815",  # Christmas Beetle
                            "Cyclocephala signaticollis", # Argentinian Scarab Beetle
                            "Melolonthinae",              # June Beetles
                            "Lamprima aurata"))           # Golden Stag Beetle

beetle_ids_tidy <- beetle_ids |> 
  mutate(vernacular_name = case_when(
    search_term == "Cyclocephala signaticollis" ~ "Argentinian Scarab Beetle",
    search_term == "Melolonthinae" ~ "June Beetles",
    .default = as.character(vernacular_name))) |> 
  select(-c(issues, match_type, scientific_name_authorship))

# beetle_counts <- map(beetle_ids_tidy$taxon_concept_id, 
#                      \(x) galah_call() |> 
#                        filter(taxonConceptID == x, 
#                               year > 1900,
#                               year < 2025) |> 
#                        group_by(year, cl22) |> 
#                        atlas_counts()) |> 
#   set_names(beetle_ids_tidy$vernacular_name) |> 
#   list_rbind(names_to = "vernacular_name")
#   
# saveRDS(beetle_counts, here("data", "beetle_counts.RDS"))
beetle_counts <- readRDS(here("data", "beetle_counts.RDS"))

vernacular_name <- beetle_ids_tidy$vernacular_name
year <- c(1900:2024)
cl22 <- unique(beetle_counts$cl22)
year_beetle_state_combos <- expand_grid(vernacular_name, year, cl22)
states_abbr <- tibble(cl22 = cl22, 
                      state_abbr = c("NSW", "QLD", "VIC", "SA", "ACT", "TAS", "NT", "WA"))

beetle_counts_complete <- year_beetle_state_combos |> 
  left_join(mutate(beetle_counts, year = as.numeric(year)), 
            by = join_by(vernacular_name, year, cl22)) |> 
  left_join(states_abbr, by = "cl22")

beetle_count_range <- range(beetle_counts_complete$count, na.rm = TRUE)

# TODO: could wrap this up in pmap and walk
# Christmas Beetle
plot_heatmap(df = filter(beetle_counts_complete, vernacular_name == "Christmas Beetle"), 
             legend_title = expression("Counts of Christmas Beetles (" * italic("Anoplognathus spp.") * ")"))

ggsave(here("plots", "christmas_beetle.png"), width = 12, height = 4, units = "in")

# Argentinian Scarab Beetle
plot_heatmap(df = filter(beetle_counts_complete, vernacular_name == "Argentinian Scarab Beetle"), 
             legend_title = "Counts of beetles") +
  annotate("text", 
           x = 1926, 
           y = 10, 
           label = as.character(expression(paste("Argentinian Scarab Beetles (", italic("Cyclocephala signaticollis"), ")"))),
           parse = TRUE, size = 4)

ggsave(here("plots", "arg_scarab.png"), width = 12, height = 4, units = "in")

# June Beetles
plot_heatmap(df = filter(beetle_counts_complete, vernacular_name == "June Beetles"), 
             legend_title = "") +
  theme(legend.position = "none") +
  annotate("text", 
           x = 1919, 
           y = 10, 
           label = "June Beetles (subfamily Melolonthinae)",
           size = 4)

ggsave(here("plots", "june_beetle.png"), width = 12, height = 4, units = "in")

# Golden Stag Beetle
plot_heatmap(df = filter(beetle_counts_complete, vernacular_name == "Golden Stag Beetle"), 
             legend_title = "") +
  theme(legend.position = "none") +
  annotate("text", 
           x = 1918, 
           y = 10, 
           label = as.character(expression(paste("Golden Stag Beetles (", italic("Lamprima aurata"), ")"))),
           parse = TRUE, size = 4) 

ggsave(here("plots", "golden_stag.png"), width = 12, height = 4, units = "in")


# Click beetles -----------
# excel sheet is from Juliet 20251107
elaterids_dmp <- read_xlsx(here("data", "elateridae-occurrences.xlsx"), 
                           sheet = "elateridae-DMP-occurrences")

elaterids_other <- read_xlsx(here("data", "elateridae-occurrences.xlsx"), 
                             sheet = "elateridae-other-occurrences")

elaterids_all <- galah_call() |> 
  identify("elateridae") |> 
  apply_profile(ALA) |> 
  select(group = "basic", basisOfRecord, genus, species, year, taxonRank) |> 
  atlas_occurrences()

saveRDS(elaterids_all, here("data", "elaterids_all.RDS"))
elaterids_all <- readRDS(here("data", "elaterids_all.RDS"))

# doubles up a bit but easier to just use smaller datasets
elaterids_putative_dmp <- elaterids_all |> 
  filter(!recordID %in% elaterids_other$recordID,
         dataResourceName == "Museums Victoria provider for OZCAM")

elaterids_putative_other <- elaterids_all |> 
  filter(!recordID %in% elaterids_putative_dmp$recordID)

### click beetles waffle -------
click_df <- tibble(species = c(322, 60, 418), group = c("before", "after", "unknown"))

subtitle_text <- marquee_glue("Elateridae species in the ALA {#8F2E71 **before**} and {#CA9DBC **after**} data mobilisation 
                           
                           1 {.#444444 {cli::symbol$square_small_filled}} = 1 species")

ggplot(click_df) +
  geom_waffle(aes(values = species, fill = group),
              n_rows = 40,       
              color = "white",  
              flip = TRUE,
              radius = grid::unit(0, "npc")) +
  coord_equal() +
  pilot::theme_pilot(grid = "", axes = "l") +
  scale_fill_manual(values = c("#CA9DBC", "#8F2E71",  "grey80")) +
  scale_y_continuous(breaks = c(0, 10, 20),
                     labels = c("0", "50%", "100%")) +
  labs(y = "Percentage of species",
       subtitle = subtitle_text) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.position = "none",
    text = element_text(family = "roboto"),
    plot.subtitle = element_marquee(size = 28), 
    plot.background = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA))
 
ggsave(here("plots", "click_waffle.png"), height = 9, width = 16, units = "in")

### click beetles barplot -----
click_barplot <- elaterids_all |> 
  mutate(
    record_type = case_when(
      basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION", "OCCURRENCE") ~ "Human observations",
      basisOfRecord == c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE") ~ "Preserved specimens",
      .default = "other"),
    taxon_rank_simple = case_when(
      taxonRank == "subfamily" ~ "family",
      taxonRank == "subgenus" ~ "genus",
      .default = as.character(taxonRank))) |> 
  count(record_type, taxon_rank_simple) |> 
  filter(record_type != "other") |> 
  mutate(taxon_rank_simple = str_to_title(taxon_rank_simple))

barplot_subtitle <- marquee_glue(
  "Elateridae records from {#5b7749 **human observations**} and {#305027 **preserved specimens**}"
)  

ggplot(click_barplot) +
  geom_col(aes(x = taxon_rank_simple, 
               y = n, fill = record_type),
           position = "dodge") +
  scale_fill_manual(values = c("#5b7749", "#305027")) +
  labs(y = "Number of occurrence records",
       subtitle = barplot_subtitle) +
  pilot::theme_pilot(grid = "", axes = "l") +
  theme(text = element_text(family = "roboto"),
        axis.text.y.left = element_text(family = "roboto"),
        axis.text.x.bottom = element_text(family = "roboto"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "roboto", size = 18),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.position = "none",
        plot.subtitle = element_marquee(size = 28), 
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))

ggsave(here("plots", "click_barplot.png"), height = 9, width = 16, units = "in")

### click beetles table -----

data.frame(y = c("Before", "Before", "Before", "After", "After", "After"),
           x = c("Records", "Species", "Genera", "Records", "Species", "Genera"),
           values = c(11167, 322, 63, 17232, 382, 64)) |>  
  ggplot() +
  geom_tile(aes(x, y, fill = y), 
            colour = "#fdf9ec",
            linewidth = 7) +
  geom_text(aes(x, y, 
                label = scales::label_comma()(values), 
                colour = y),
            size = 15) + 
  scale_fill_manual(values = c("#CA9DBC", "#8F2E71")) +
  scale_color_manual(values = c("#101010", "#fdf9ec")) +
  scale_x_discrete(limits = c("Records", "Genera", "Species"), position = "top") +
  labs(x = "") +
  coord_equal() +
  pilot::theme_pilot(axes = "", grid = "", legend_position = "none") +
  theme(text = element_text(family = "roboto"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "roboto", size = 22),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "roboto", size = 22),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))

ggsave(here("plots", "click_table.png"), height = 9, width = 16, units = "in")

  
  



