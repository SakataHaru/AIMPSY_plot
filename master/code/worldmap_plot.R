# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "tidyverse"))

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

sf_use_s2(FALSE)

softlink_dir <- if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "softlink") {
  "."
} else {
  "softlink"
}
data_dir <- file.path(softlink_dir, "data")
nh3_node_csv <- file.path(data_dir, "AIM_R31_NH3_trade_nodes.csv")

# =========================
# 1. Load world map
# =========================

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) |>
  st_make_valid() |>
  mutate(iso3 = if_else(iso_a3 == "-99", adm0_a3, iso_a3))

# =========================
# 2. AIM-Technology R31 region mapping
# =========================

region_map <- tribble(
  ~iso3, ~region,

  # Individual Asian regions
  "JPN", "JPN",
  "CHN", "CHN",
  "IND", "IND",
  "IDN", "IDN",
  "KOR", "KOR",
  "THA", "THA",
  "MYS", "MYS",
  "VNM", "VNM",

  # Southeast Asia
  "BRN", "XSE", "KHM", "XSE", "LAO", "XSE",
  "MMR", "XSE", "PHL", "XSE", "SGP", "XSE",
  "TLS", "XSE",

  # South Asia
  "AFG", "XSA", "BGD", "XSA", "BTN", "XSA",
  "LKA", "XSA", "MDV", "XSA", "NPL", "XSA",
  "PAK", "XSA",

  # East Asia
  "MNG", "XEA", "PRK", "XEA", "TWN", "XEA",

  # Central Asia
  "KAZ", "XCS", "KGZ", "XCS", "TJK", "XCS",
  "TKM", "XCS", "UZB", "XCS",

  # Middle East
  "ARE", "XME", "BHR", "XME", "IRN", "XME",
  "IRQ", "XME", "ISR", "XME", "JOR", "XME",
  "KWT", "XME", "LBN", "XME", "OMN", "XME",
  "QAT", "XME", "SAU", "XME", "SYR", "XME",
  "YEM", "XME",

  # Oceania
  "AUS", "AUS",
  "NZL", "XOC", "PNG", "XOC", "FJI", "XOC",
  "SLB", "XOC", "VUT", "XOC", "WSM", "XOC",

  # North America
  "CAN", "CAN",
  "USA", "USA",
  "MEX", "MEX",

  # Latin America
  "BRA", "BRA",
  "ARG", "XLM", "BOL", "XLM", "CHL", "XLM",
  "COL", "XLM", "CRI", "XLM", "CUB", "XLM",
  "DOM", "XLM", "ECU", "XLM", "GTM", "XLM",
  "HND", "XLM", "HTI", "XLM", "JAM", "XLM",
  "NIC", "XLM", "PAN", "XLM", "PER", "XLM",
  "PRY", "XLM", "SLV", "XLM", "URY", "XLM",
  "VEN", "XLM", "GUY", "XLM", "SUR", "XLM",
  "TTO", "XLM", "BHS", "XLM", "BRB", "XLM",
  "BLZ", "XLM",

  # Europe
  "TUR", "TUR",
  "RUS", "RUS",

  # EU15
  "AUT", "XE15", "BEL", "XE15", "DNK", "XE15",
  "FIN", "XE15", "FRA", "XE15", "DEU", "XE15",
  "GRC", "XE15", "IRL", "XE15", "ITA", "XE15",
  "LUX", "XE15", "NLD", "XE15", "PRT", "XE15",
  "ESP", "XE15", "SWE", "XE15", "GBR", "XE15",

  # EU10 / Central and Eastern EU
  "BGR", "XE3", "CYP", "XE10", "CZE", "XE10",
  "EST", "XE10", "HUN", "XE10", "LVA", "XE10",
  "LTU", "XE10", "MLT", "XE10", "POL", "XE10",
  "ROU", "XE3", "SVK", "XE10", "SVN", "XE10",

  # Western Europe outside EU15
  "CHE", "XEWI", "NOR", "XEWI", "ISL", "XEWI",

  # Eastern Europe / Balkans / Caucasus
  "ALB", "XEEI", "BIH", "XEEI", "HRV", "XEEI",
  "MNE", "XEEI", "MKD", "XEEI", "SRB", "XEEI",
  "UKR", "XEEI", "BLR", "XEEI", "MDA", "XEEI",
  "ARM", "XEEI", "AZE", "XEEI", "GEO", "XEEI",

  # North Africa
  "DZA", "XENI", "EGY", "XENI", "LBY", "XENI",
  "MAR", "XENI", "TUN", "XENI", "ESH", "XENI",

  # South Africa
  "ZAF", "ZAF",

  # Africa
  "AGO", "XAF", "BEN", "XAF", "BWA", "XAF",
  "BFA", "XAF", "BDI", "XAF", "CMR", "XAF",
  "CAF", "XAF", "TCD", "XAF", "COD", "XAF",
  "COG", "XAF", "CIV", "XAF", "DJI", "XAF",
  "ERI", "XAF", "ETH", "XAF", "GAB", "XAF",
  "GHA", "XAF", "GIN", "XAF", "GNB", "XAF",
  "GNQ", "XAF", "KEN", "XAF", "LBR", "XAF",
  "LSO", "XAF", "MDG", "XAF", "MWI", "XAF",
  "MLI", "XAF", "MRT", "XAF", "MOZ", "XAF",
  "NAM", "XAF", "NER", "XAF", "NGA", "XAF",
  "RWA", "XAF", "SEN", "XAF", "SLE", "XAF",
  "SOM", "XAF", "SSD", "XAF", "SDN", "XAF",
  "SWZ", "XAF", "TZA", "XAF", "TGO", "XAF",
  "UGA", "XAF", "ZMB", "XAF", "ZWE", "XAF",

  # No fuel / unclassified
  "GRL", "XNF"
)

# =========================
# 3. Join map and region data
# =========================

world_region <- world |>
  left_join(region_map, by = "iso3") |>
  mutate(region = if_else(is.na(region), "XNF", region))

# =========================
# 4. Color setting
# =========================

region_cols <- c(
  JPN  = "#a9c6d3",
  CHN  = "#f0d278",
  IND  = "#d8ff35",
  IDN  = "#d6cddd",
  KOR  = "#eec684",
  THA  = "#7bb6c5",
  MYS  = "#9bcfaf",
  VNM  = "#dab5d2",
  XSE  = "#e799bd",
  XSA  = "#b2acd0",
  XEA  = "#e5eba8",
  XCS  = "#cfd9a2",
  XME  = "#aee099",
  AUS  = "#97cba8",
  XOC  = "#e9a2ae",
  CAN  = "#dcebc9",
  USA  = "#e3afc5",
  XE15 = "#ed97a1",
  XE10 = "#8bb8cf",
  XE3  = "#d8a5cc",
  TUR  = "#cf83d5",
  XEWI = "#edd29b",
  XEEI = "#afd5b8",
  XENI = "#b8c8d9",
  RUS  = "#f2a47d",
  MEX  = "#aeb9cc",
  BRA  = "#6fd06a",
  XLM  = "#99b9c7",
  ZAF  = "#b9d8d7",
  XAF  = "#e6e8bb",
  XNF  = "#c5c9d8"
)

# =========================
# 5. Plot
# =========================

p <- ggplot(world_region) +
  geom_sf(aes(fill = region), color = "black", linewidth = 0.25) +
  scale_fill_manual(
    values = region_cols,
    breaks = names(region_cols),
    name = NULL
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  )

print(p)

# =========================
# 6. Save
# =========================

ggsave(
  filename = "output/AIM_R31_region_map.png",
  plot = p,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)

# =========================
# 7. Plot region boundaries only
# =========================

region_boundaries <- world_region |>
  st_set_crs(NA) |>
  group_by(region) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid() |>
  st_set_crs(st_crs(world_region))

p_region_boundaries <- ggplot(region_boundaries) +
  geom_sf(fill = NA, color = "black", linewidth = 0.4) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  theme_void()

print(p_region_boundaries)

ggsave(
  filename = "output/AIM_R31_region_boundaries.png",
  plot = p_region_boundaries,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)

# =========================
# 8. Create NH3 trade node coordinates
# =========================

build_nh3_region_nodes <- function() {
  region_points <- suppressWarnings(
    region_boundaries |>
      filter(region != "XNF") |>
      st_transform("+proj=eqearth") |>
      st_centroid() |>
      st_transform(4326)
  ) |>
    mutate(
      lon = st_coordinates(geometry)[, "X"],
      lat = st_coordinates(geometry)[, "Y"]
    ) |>
    st_drop_geometry() |>
    transmute(N_PSY = region, lon, lat)

  xnf_point <- suppressWarnings(
    world |>
      filter(iso3 == "GRL") |>
      st_transform("+proj=eqearth") |>
      st_centroid() |>
      st_transform(4326)
  ) |>
    mutate(
      lon = st_coordinates(geometry)[, "X"],
      lat = st_coordinates(geometry)[, "Y"]
    ) |>
    st_drop_geometry() |>
    transmute(N_PSY = "XNF", lon, lat)

  generated_points <- bind_rows(region_points, xnf_point) |>
    distinct(N_PSY, .keep_all = TRUE)

  if (file.exists(nh3_node_csv)) {
    csv_points <- read.csv(nh3_node_csv, stringsAsFactors = FALSE) |>
      transmute(N_PSY, lon = as.numeric(lon), lat = as.numeric(lat))

    generated_points <- bind_rows(
      generated_points |> filter(!N_PSY %in% csv_points$N_PSY),
      csv_points
    )
  }

  generated_points |>
    mutate(
      lon = case_when(
        N_PSY == "XE3" ~ 25,
        N_PSY == "XENI" ~ 19.3,
        TRUE ~ lon
      ),
      lat = case_when(
        N_PSY == "XE3" ~ 44,
        N_PSY == "XENI" ~ 42.7,
        TRUE ~ lat
      )
    ) |>
    distinct(N_PSY, .keep_all = TRUE)
}

nh3_nodes_tbl <- build_nh3_region_nodes()

plot_nh3_region_nodes <- function() {
  ggplot() +
    geom_sf(data = region_boundaries, fill = NA, color = "grey70", linewidth = 0.25) +
    geom_point(
      data = nh3_nodes_tbl,
      aes(x = lon, y = lat),
      shape = 21,
      fill = "white",
      color = "red3",
      stroke = 0.8,
      size = 2.4
    ) +
    geom_text(
      data = nh3_nodes_tbl,
      aes(x = lon, y = lat, label = N_PSY),
      nudge_y = 2,
      size = 3,
      color = "red3"
    ) +
    coord_sf(ylim = c(-60, 75), expand = FALSE) +
    theme_void() +
    labs(title = "NH3 trade R31 region nodes")
}

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(nh3_nodes_tbl, file = nh3_node_csv, row.names = FALSE)

ggsave(
  filename = "output/AIM_R31_NH3_trade_nodes.png",
  plot = plot_nh3_region_nodes(),
  width = 300,
  height = 130,
  units = "mm",
  dpi = 300,
  bg = "white"
)
