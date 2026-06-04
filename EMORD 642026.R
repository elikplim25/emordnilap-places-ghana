library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(sf)
library(patchwork)
library(purrr)

####################### Input parameters ##########################
country  <- "Ghana"
data_dir <- here::here("palindrome-places", "data")
###################################################################

# ── Read ALL places, unfiltered ───────────────────────────────────────────────
places_all <- read_tsv(
  here::here(data_dir, "GH.txt"),
  col_names = c(
    "geonameid", "name", "asciiname", "alternatenames",
    "latitude", "longitude", "feature_class", "feature_code",
    "country_code", "cc2", "admin1_code", "admin2_code",
    "admin3_code", "admin4_code", "population", "elevation",
    "dem", "timezone", "modification_date"
  ),
  show_col_types = FALSE
)

# Build reference set from ALL places across ALL feature classes
all_place_names_lower <- tolower(places_all$name)

# ── Emordnilap check function ─────────────────────────────────────────────────
is_emordnilap <- function(s, name_set) {
  s_lower    <- tolower(s)
  s_reversed <- stringi::stri_reverse(s_lower)
  s_reversed %in% name_set & s_reversed != s_lower
}

# ── Find emordnilap towns (populated places only for Map 1) ───────────────────
places_emordnilaps <- places_all |>
  filter(feature_class == "P") |>
  filter(is_emordnilap(name, all_place_names_lower)) |>
  mutate(name_reversed = stringi::stri_reverse(tolower(name)))

# ── Partner lookup across ALL feature classes ─────────────────────────────────
# Removing feature_class == "P" here recovers pairs like Rus → Sur
# where the partner may not be a populated place
partner_lookup <- places_all |>
  mutate(name_lower = tolower(name)) |>
  filter(name_lower %in% places_emordnilaps$name_reversed) |>
  group_by(name_lower) |>
  slice_max(population, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(partner_name = name, name_lower,
         partner_lat  = latitude,
         partner_lon  = longitude)

# ── Join and deduplicate pairs ────────────────────────────────────────────────
town_pairs <- places_emordnilaps |>
  left_join(partner_lookup, by = c("name_reversed" = "name_lower")) |>
  filter(!is.na(partner_lat)) |>
  mutate(pair_key = map2_chr(tolower(name), tolower(partner_name),
                             ~paste(sort(c(.x, .y)), collapse = "_"))) |>
  distinct(pair_key, .keep_all = TRUE) |>
  select(-pair_key) |>
  arrange(name)

# Sanity check — confirm you now see more pairs including Rus → Sur
cat("Unique pairs found:", nrow(town_pairs), "\n")
print(town_pairs |> select(name, partner_name,
                           longitude, latitude,
                           partner_lon, partner_lat))

# ── Build sf objects ──────────────────────────────────────────────────────────
map1_sf <- town_pairs |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")

map2_sf <- town_pairs |>
  distinct(partner_name, .keep_all = TRUE) |>
  st_as_sf(coords = c("partner_lon", "partner_lat"), crs = "EPSG:4326")

# ── Country shape ─────────────────────────────────────────────────────────────
shp <- rnaturalearth::ne_countries(scale = 10, country = country,
                                   returnclass = "sf")

# ── Aspect ratio ──────────────────────────────────────────────────────────────
min_width  <- 7
min_height <- 6
bbox       <- st_bbox(shp)
width_height_ratio <- unname(
  abs(bbox["xmax"] - bbox["xmin"]) / abs(bbox["ymax"] - bbox["ymin"])
)
if (width_height_ratio > 1) {
  map_w <- width_height_ratio * min_height
  map_h <- min_height
} else {
  map_w <- min_width
  map_h <- min_width * width_height_ratio
}

# ── Middle connector panel ────────────────────────────────────────────────────
connector_df <- town_pairs |>
  distinct(name, .keep_all = TRUE) |>
  select(name, partner_name) |>
  arrange(name) |>
  mutate(y_pos = row_number())

n_pairs <- nrow(connector_df)

p_connector <- ggplot(connector_df) +
  geom_segment(
    aes(x = 0.45, xend = 0.55, y = y_pos, yend = y_pos),
    colour   = "white", alpha = 0.5, linewidth = 0.4,
    arrow    = arrow(length = unit(0.18, "cm"), type = "open")
  ) +
  geom_text(
    aes(x = 0.43, y = y_pos, label = name),
    hjust  = 1, colour = "white", size = 3.2,
    family = "Instrument Sans SemiBold"
  ) +
  geom_text(
    aes(x = 0.57, y = y_pos, label = partner_name),
    hjust  = 0, colour = "#E8A020", size = 3.2,
    family = "Instrument Sans SemiBold"
  ) +
  xlim(0, 1) +
  ylim(0, n_pairs + 1) +
  labs(title = " ") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "darkgreen", color = NA),
    plot.title      = element_text(color = "transparent", size = 16)
  )

# ── Shared map theme ──────────────────────────────────────────────────────────
map_theme <- list(
  cowplot::theme_map(font_family = "Instrument Sans"),
  theme(
    plot.background = element_rect(color = NA, fill = "darkgreen"),
    text            = element_text(color = "white"),
    plot.title      = element_markdown(
      family = "Libre Bodoni", size = 16, hjust = 0.5),
    plot.subtitle   = element_markdown(
      hjust = 0.5, lineheight = 1.33, size = 8.5)
  )
)

# ── Map 1: original towns ─────────────────────────────────────────────────────
p_map1 <- ggplot(shp) +
  ggfx::with_shadow(
    geom_sf(size = 0.2, fill = "grey90"),
    x_offset = 8, y_offset = 8, colour = "grey12"
  ) +
  geom_sf(
    data  = map1_sf,
    shape = 21, color = "white", size = 3, fill = "grey12"
  ) +
  ggrepel::geom_label_repel(
    data             = map1_sf,
    aes(geometry = geometry, label = name),
    stat             = "sf_coordinates",
    family           = "Instrument Sans SemiBold", color = "grey12", size = 3,
    segment.size     = 0.5, segment.linetype = "dotted",
    fill             = "#FFFFFF99", label.size = 0, max.overlaps = 20
  ) +
  coord_sf(crs = st_crs(shp)) +
  labs(
    title    = "The Towns",
    subtitle = "Populated places whose name spells<br>a different place name in reverse"
  ) +
  map_theme

# ── Map 2: partner towns at their own locations ───────────────────────────────
p_map2 <- ggplot(shp) +
  ggfx::with_shadow(
    geom_sf(size = 0.2, fill = "grey90"),
    x_offset = 8, y_offset = 8, colour = "grey12"
  ) +
  geom_sf(
    data  = map2_sf,
    shape = 21, color = "white", size = 3, fill = "#E8A020"
  ) +
  ggrepel::geom_label_repel(
    data             = map2_sf,
    aes(geometry = geometry, label = partner_name),
    stat             = "sf_coordinates",
    family           = "Instrument Sans SemiBold", color = "grey12", size = 3,
    segment.size     = 0.5, segment.linetype = "dotted",
    fill             = "#FFFFFF99", label.size = 0, max.overlaps = 20
  ) +
  coord_sf(crs = st_crs(shp)) +
  labs(
    title    = "Their Emordnilaps",
    subtitle = "The same reversed names — shown<br>at those towns' actual locations"
  ) +
  map_theme

# ── Combine all three panels ──────────────────────────────────────────────────
combined <- p_map1 + p_connector + p_map2 +
  plot_layout(widths = c(5, 1.2, 5)) +
  plot_annotation(
    title    = "Emordnilap Places in Ghana",
    subtitle = "Towns whose name spells a completely different town name when reversed.",
    caption  = "**Source:** GeoNames.org, Natural Earth Data | **Visualization:** Elikplim Sabblah",
    theme    = theme(
      plot.background = element_rect(color = NA, fill = "darkgreen"),
      text            = element_text(color = "white", family = "Instrument Sans"),
      plot.title      = element_markdown(
        family = "Libre Bodoni", size = 22, hjust = 0.5, color = "white"),
      plot.subtitle   = element_markdown(
        hjust = 0.5, lineheight = 1.33, color = "white", size = 10),
      plot.caption    = element_markdown(hjust = 0.5, color = "white")
    )
  )

# ── Save ──────────────────────────────────────────────────────────────────────
if (!dir.exists(here::here("emordnilap-places", "plots"))) {
  dir.create(here::here("emordnilap-places", "plots"), recursive = TRUE)
}

ggsave(
  here::here("emordnilap-places", "plots",
             glue::glue("emordnilap_places_{country}.png")),
  plot   = combined,
  dpi    = 300,
  width  = map_w * 2 + 2.5,
  height = map_h + 2
)