library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(sf)
library(ggrepel)

####################### Input parameters ##########################
country  <- "Ghana"
data_dir <- here::here("palindrome-places", "data")
###################################################################

places <- read_tsv(
  file.path(data_dir, "GH.txt"),
  col_names = c(
    "geonameid", "name", "asciiname", "alternatenames",
    "latitude", "longitude", "feature_class", "feature_code",
    "country_code", "cc2", "admin1_code", "admin2_code",
    "admin3_code", "admin4_code", "population", "elevation",
    "dem", "timezone", "modification_date"
  ),
  quote = "",
  show_col_types = FALSE
)

all_place_names_lower <- tolower(places$name)

is_emordnilap <- function(s, name_set) {
  s_lower    <- tolower(s)
  s_reversed <- vapply(strsplit(s_lower, ""), function(x) paste(rev(x), collapse = ""), character(1))
  s_reversed %in% name_set & s_reversed != s_lower
}

places_emordnilaps <- places |>
  filter(feature_class == "P") |>
  filter(is_emordnilap(name, all_place_names_lower)) |>
  mutate(
    name_reversed = vapply(strsplit(tolower(name), ""), function(x) paste(rev(x), collapse = ""), character(1))
  )

partner_lookup <- places |>
  filter(feature_class == "P") |>
  mutate(name_lower = tolower(name)) |>
  filter(name_lower %in% places_emordnilaps$name_reversed) |>
  group_by(name_lower) |>
  slice_max(population, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(partner_name = name, name_lower,
         partner_lat = latitude, partner_lon = longitude)

pairs <- places_emordnilaps |>
  left_join(partner_lookup, by = c("name_reversed" = "name_lower")) |>
  filter(!is.na(partner_lat)) |>
  arrange(name)

# ── Ghana shape ───────────────────────────────────────────────────────────────
shp <- rnaturalearth::ne_countries(scale = 10, country = country,
                                   returnclass = "sf")

# ── Two sf point layers on the same map ───────────────────────────────────────

# Original towns — one point per unique name
towns_sf <- pairs |>
  distinct(name, .keep_all = TRUE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")

# Emordnilap partners — one point per unique partner name
partners_sf <- pairs |>
  distinct(partner_name, .keep_all = TRUE) |>
  st_as_sf(coords = c("partner_lon", "partner_lat"), crs = "EPSG:4326")

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +

  ggfx::with_shadow(
    geom_sf(data = shp, fill = "grey88", color = "grey50", linewidth = 0.2),
    x_offset = 6, y_offset = 6, colour = "grey15"
  ) +

  # Original town points — white fill
  geom_sf(data = towns_sf,
          shape = 21, color = "white", fill = "white", size = 3, stroke = 0.8) +

  # Partner points — gold fill
  geom_sf(data = partners_sf,
          shape = 21, color = "white", fill = "#E8A020", size = 3, stroke = 0.8) +

  # Original town labels — white
  ggrepel::geom_label_repel(
    data             = towns_sf,
    aes(geometry = geometry, label = name),
    stat             = "sf_coordinates",
    family           = "Instrument Sans SemiBold",
    size             = 3, color = "white",
    fill             = NA, label.size = 0,
    segment.color    = "white", segment.size = 0.35,
    segment.linetype = "dotted",
    max.overlaps     = 20, seed = 42
  ) +

  # Emordnilap partner labels — gold
  ggrepel::geom_label_repel(
    data             = partners_sf,
    aes(geometry = geometry, label = partner_name),
    stat             = "sf_coordinates",
    family           = "Instrument Sans SemiBold",
    size             = 3, color = "#E8A020",
    fill             = NA, label.size = 0,
    segment.color    = "#E8A020", segment.size = 0.35,
    segment.linetype = "dotted",
    max.overlaps     = 20, seed = 42
  ) +

  coord_sf(crs = st_crs(shp)) +
  labs(
    title    = "Emordnilap Places in Ghana",
    subtitle = "<span style='color:white'>**Town names**</span> whose reverse spells a different place — shown in <span style='color:#E8A020'>**gold**</span>",
    caption  = "**Source:** GeoNames.org, Natural Earth Data | **Visualization:** Elikplim Sabblah"
  ) +
  theme_void(base_family = "Instrument Sans") +
  theme(
    plot.background = element_rect(fill = "darkgreen", color = NA),
    text            = element_text(color = "white"),
    plot.title      = element_markdown(
      family = "Libre Bodoni", size = 22, hjust = 0.5, color = "white",
      margin = margin(t = 12, b = 6)
    ),
    plot.subtitle   = element_markdown(
      hjust = 0.5, size = 10, lineheight = 1.4,
      margin = margin(b = 10)
    ),
    plot.caption    = element_markdown(
      hjust = 0.5, color = "white",
      margin = margin(t = 10, b = 10)
    )
  )

# ── Save ──────────────────────────────────────────────────────────────────────
if (!dir.exists(here::here("emordnilap-places", "plots"))) {
  dir.create(here::here("emordnilap-places", "plots"), recursive = TRUE)
}

ggsave(
  here::here("emordnilap-places", "plots",
             glue::glue("emordnilap_places_{country}.png")),
  plot   = p,
  dpi    = 300,
  width  = 9,
  height = 10
)
