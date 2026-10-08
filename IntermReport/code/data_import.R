# MODEL DATA IMPORT ------------------------------------------------------------
# Keep Figure-specific filtering, grouping, and aggregation in panel functions.

read_iamc_gdx <- function(main_dir, model_name) {
  gdx_path <- file.path(main_dir, "merged_output.gdx")
  if (!file.exists(gdx_path)) {
    stop("Missing ", model_name, " GDX file: ", gdx_path)
  }

  raw_data <- gdxrrw::rgdx.param(
    normalizePath(gdx_path, winslash = "\\", mustWork = TRUE),
    "iamc_gdx"
  )

  required_columns <- c("Sc", "Sr", "Sv", "Y5", "iamc_gdx")
  missing_columns <- setdiff(required_columns, names(raw_data))
  if (length(missing_columns) > 0L) {
    stop(
      model_name,
      " iamc_gdx lacks expected dimensions/field: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  dplyr::transmute(
    raw_data,
    Scenario = as.character(.data$Sc),
    Region = as.character(.data$Sr),
    Variable = as.character(.data$Sv),
    Year = suppressWarnings(as.integer(as.character(.data$Y5))),
    Value = suppressWarnings(as.numeric(as.character(.data$iamc_gdx))),
    Model = model_name
  )
}

import_model_data <- function(
  aimt_main_dir,
  psm_main_dir,
  psm_primary_dir,
  scenario_map,
  variable_specs,
  gams_system_dir
) {
  gdxrrw::igdx(gams_system_dir)

  message("Reading AIM-Technology IAMC data: ", file.path(aimt_main_dir, "merged_output.gdx"))
  aimt_iamc <- read_iamc_gdx(aimt_main_dir, "AIM-Technology")

  message("Reading PSM IAMC data: ", file.path(psm_main_dir, "merged_output.gdx"))
  psm_iamc <- read_iamc_gdx(psm_main_dir, "PSM")

  list(
    aimt_iamc = aimt_iamc,
    psm_iamc = psm_iamc,
    variable_specs = variable_specs,
    metadata = list(
      aimt_main_dir = aimt_main_dir,
      aimt_iamc_gdx = file.path(aimt_main_dir, "merged_output.gdx"),
      psm_main_dir = psm_main_dir,
      psm_iamc_gdx = file.path(psm_main_dir, "merged_output.gdx"),
      psm_primary_dir = psm_primary_dir,
      scenario_map = scenario_map,
      gams_system_dir = gams_system_dir,
      iamc_symbol = "iamc_gdx"
    )
  )
}

# 2050 primary data are read only when Fig. 2-b--d or Fig. 3 are built. The source file
# itself is year-specific; three link symbols and biomass pmax are loaded. Flows
# are reduced over their primary-time dimensions immediately to avoid retaining
# millions of timeslice rows in the figure data list.
primary_time_weight_from_timeresol <- function(time_resol) {
  key <- toupper(trimws(time_resol))
  weights <- c(
    "12DAY" = ceiling(365 / 12 * 1000) / 1000,
    "6HOUR" = 6,
    "4HOUR" = 4,
    "3HOUR" = 3,
    "1HOUR" = 1
  )
  if (length(key) != 1L || is.na(key) || !key %in% names(weights)) {
    stop("Unsupported time_resol: ", time_resol)
  }
  unname(weights[[key]])
}

load_psm_primary <- function(
  psm_primary_dir,
  scenario_map,
  year = 2050L,
  time_resol,
  gams_system_dir,
  region_map_path
) {
  if (length(year) != 1L || is.na(year) || year != as.integer(year)) {
    stop("Primary year must be one integer model year.")
  }
  if (anyDuplicated(scenario_map) || anyDuplicated(names(scenario_map))) {
    stop("scenario_map must contain unique scenario names and keys.")
  }
  time_weight <- primary_time_weight_from_timeresol(time_resol)
  gdxrrw::igdx(gams_system_dir)

  read_symbol <- function(gdx_path, scenario, symbol) {
    raw <- gdxrrw::rgdx.param(
      normalizePath(gdx_path, winslash = "\\", mustWork = TRUE), symbol
    )
    if (!all(c("L", symbol) %in% names(raw))) {
      stop(symbol, " lacks the expected L index or value field: ", gdx_path)
    }
    out <- raw %>%
      dplyr::transmute(
        Scenario = scenario,
        Link = as.character(.data$L),
        Value = as.numeric(.data[[symbol]])
      ) %>%
      dplyr::group_by(.data$Scenario, .data$Link) %>%
      dplyr::summarise(Value = sum(.data$Value), .groups = "drop")
    message(
      "  ", symbol, ": columns=", paste(names(raw), collapse = ","),
      "; rows=", nrow(raw), "; aggregated_links=", nrow(out)
    )
    rm(raw)
    if (any(!is.finite(out$Value))) {
      stop("Non-finite primary values in ", symbol, ": ", gdx_path)
    }
    if (symbol != "vfs_l") {
      out$Value <- out$Value * time_weight * 3.6e-6
    }
    out
  }

  scenario_paths <- stats::setNames(
    file.path(psm_primary_dir, unname(scenario_map), paste0(year, ".gdx")),
    unname(scenario_map)
  )
  missing_paths <- scenario_paths[!file.exists(scenario_paths)]
  if (length(missing_paths) > 0L) {
    stop("Missing PSM primary GDX: ", paste(missing_paths, collapse = ", "))
  }
  if (!file.exists(region_map_path)) {
    stop("Missing PSM node-to-region map: ", region_map_path)
  }
  region_map_raw <- utils::read.csv(region_map_path, stringsAsFactors = FALSE)
  if (!all(c("N_IND", "R31") %in% names(region_map_raw))) {
    stop("PSM region map must contain N_IND and R31: ", region_map_path)
  }
  region_map <- region_map_raw %>%
    dplyr::transmute(Node = as.character(.data$N_IND), Region = as.character(.data$R31)) %>%
    dplyr::distinct()
  by_symbol <- list(vfs_l = list(), vff_l = list(), vfb_l = list())
  biomass_limits <- list()
  for (scenario in unname(scenario_map)) {
    gdx_path <- scenario_paths[[scenario]]
    message("Reading PSM primary ", year, " / ", scenario, ": ", gdx_path)
    for (symbol in names(by_symbol)) {
      by_symbol[[symbol]][[scenario]] <- read_symbol(gdx_path, scenario, symbol)
    }
    # Annual resource constraints are already TWh; no timeslice weight applies.
    raw_limits <- gdxrrw::rgdx.param(
      normalizePath(gdx_path, winslash = "\\", mustWork = TRUE), "pmax", squeeze = FALSE
    )
    if (!all(c("ME", "MK", "pmax") %in% names(raw_limits))) {
      stop("pmax lacks the expected ME/MK indices or value field: ", gdx_path)
    }
    biomass_limits[[scenario]] <- raw_limits %>%
      dplyr::transmute(
        Scenario = scenario, Region = as.character(.data$ME),
        Resource = as.character(.data$MK), Value = as.numeric(.data$pmax) * 0.0036
      ) %>%
      dplyr::filter(grepl("^R_CRN(AGR|FRR|MAN|MSW|1ST|P0[1-8])$", .data$Resource) |
                      .data$Resource == "CRNALL")
    rm(raw_limits)
  }
  list(
    psm_vfs_l = dplyr::bind_rows(by_symbol$vfs_l),
    psm_vff_l = dplyr::bind_rows(by_symbol$vff_l),
    psm_vfb_l = dplyr::bind_rows(by_symbol$vfb_l),
    psm_biomass_limits = dplyr::bind_rows(biomass_limits),
    psm_region_map = region_map,
    metadata = list(
      year = as.integer(year),
      time_resol = time_resol,
      time_weight_hours = time_weight,
      capacity_unit = "GW",
      flow_unit = "EJ/yr",
      biomass_limit_symbol = "pmax(ME,MK)",
      biomass_limit_unit = "EJ/yr",
      scenario_map = scenario_map,
      gdx_paths = scenario_paths,
      region_map_path = normalizePath(region_map_path, winslash = "/", mustWork = TRUE)
    )
  )
}

# Fig. 3 map inputs are the same coordinate sources used by ALLPLOT_TRADING.R,
# copied under IntermReport/data for this report. No routes are inferred.
load_fig3_geodata <- function(base_data_dir) {
  node_path <- file.path(base_data_dir, "nodes_centroid.geojson")
  trade_path <- file.path(base_data_dir, "searoute_r31_representative.geojson")
  region_map_path <- file.path(base_data_dir, "M_R272.csv")
  input_paths <- c(node_path, trade_path, region_map_path)
  missing <- input_paths[!file.exists(input_paths)]
  if (length(missing)) stop("Missing Fig. 3 coordinate files: ", paste(missing, collapse = ", "))
  nodes_raw <- sf::st_read(node_path, quiet = TRUE)
  trade_raw <- sf::st_read(trade_path, quiet = TRUE)
  region_map_raw <- utils::read.csv(region_map_path, stringsAsFactors = FALSE)
  if (!all(c("N_PSY", "centroid_x", "centroid_y") %in% names(nodes_raw)) ||
      !all(c("R31", "Longitude", "Latitude") %in% names(trade_raw)) ||
      !all(c("N_IND", "R31") %in% names(region_map_raw))) {
    stop("Fig. 3 coordinate files have unexpected columns.")
  }
  region_map <- region_map_raw %>%
    dplyr::transmute(Node = as.character(.data$N_IND), Region = as.character(.data$R31)) %>%
    dplyr::distinct()
  s2_state <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_state)), add = TRUE)
  r31_boundaries <- nodes_raw %>%
    sf::st_make_valid() %>%
    sf::st_transform(6933) %>%
    dplyr::left_join(region_map, by = c("N_PSY" = "Node")) %>%
    dplyr::filter(!is.na(.data$Region)) %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)
  list(
    nodes = nodes_raw %>% sf::st_drop_geometry() %>%
      dplyr::transmute(Node = as.character(.data$N_PSY),
                       lon = as.numeric(.data$centroid_x), lat = as.numeric(.data$centroid_y)),
    regions = trade_raw %>% sf::st_drop_geometry() %>%
      dplyr::transmute(Region = sub("^R31-", "", as.character(.data$R31)),
                       lon = as.numeric(.data$Longitude), lat = as.numeric(.data$Latitude)),
    basemap = r31_boundaries,
    metadata = list(
      node_path = node_path, trade_path = trade_path,
      region_map_path = region_map_path
    )
  )
}

# AIM-Technology biomass map inputs, read independently of PSM primary data.
load_aimt_biomass_data <- function(primary_gdx, trade_gdx, scenario, year = 2050,
                                    gams_system_dir = "C:/GAMS/win64/26.1") {
  for (path in c(primary_gdx, trade_gdx)) {
    if (!file.exists(path)) stop("Missing AIM-Technology biomass input: ", path)
  }
  if (basename(primary_gdx) != paste0(year, ".gdx") ||
      basename(dirname(primary_gdx)) != scenario) {
    stop("AIM-Technology primary path must match the requested scenario/year.")
  }
  gdxrrw::igdx(gams_system_dir)
  raw_limits <- gdxrrw::rgdx.param(primary_gdx, "emax", squeeze = FALSE)
  if (!all(c("ME", "MK", "emax") %in% names(raw_limits))) {
    stop("AIM-Technology emax must have ME/MK indices.")
  }
  # ene_sup.gms defines biomass emax in PJ/yr (input EJ multiplied by 1000).
  limits <- raw_limits %>%
    dplyr::transmute(Scenario = scenario, Region = as.character(.data$ME),
      Resource = as.character(.data$MK), Value = as.numeric(.data$emax) / 1000) %>%
    dplyr::filter(.data$Region != "WOR",
                  grepl("^CRN(AGR|FRR|MAN|MSW|1ST|P0[1-8])$", .data$Resource))
  raw_trade <- gdxrrw::rgdx.param(trade_gdx, "trade_merged")
  required <- c("Sc", "K_EXP", "Sr", "Sr.1", "Y5", "trade_merged")
  if (!all(required %in% names(raw_trade))) {
    stop("AIM-Technology trade_merged has unexpected dimensions.")
  }
  trade <- raw_trade %>%
    dplyr::transmute(Scenario = as.character(.data$Sc),
      Commodity = as.character(.data$K_EXP), Origin = as.character(.data$Sr),
      Destination = as.character(.data[["Sr.1"]]),
      Year = as.integer(as.character(.data$Y5)), Value = as.numeric(.data$trade_merged)) %>%
    dplyr::filter(.data$Scenario == scenario, .data$Year == year,
                  .data$Commodity %in% c("CRNR1", "CRNR2"))
  if (!scenario %in% as.character(raw_trade$Sc) ||
      !any(as.character(raw_trade$Sc) == scenario & as.character(raw_trade$Y5) == year)) {
    stop("Requested scenario/year is absent from AIM-Technology trade_merged.")
  }
  if (any(!is.finite(limits$Value) | limits$Value < 0) ||
      any(!is.finite(trade$Value) | trade$Value < 0)) {
    stop("Invalid AIM-Technology biomass potentials or trade values.")
  }
  list(limits = limits, trade = trade,
    metadata = list(model = "AIM-Technology", scenario = scenario, year = year,
      primary_gdx = primary_gdx, trade_gdx = trade_gdx,
      potential_symbol = "emax(ME,MK)", potential_source_unit = "PJ/yr",
      trade_symbol = "trade_merged(Sc,K_EXP,Sr,Sr,Y5)", unit = "EJ/yr"))
}