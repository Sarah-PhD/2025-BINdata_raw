# Standalone manuscript analysis and figures, corrected birch abundance and composition, 2024–2025
# ALL browsing models use TOTAL birch abundance (downy + silver).
# Stand models: mean total birch stems per valid sampled plot.
# Plot models: total birch stems in the physical plot.
# Species-specific counts are retained only for descriptive stem figures.
# Plot-response models with stand covariates remain a separate explicitly named analysis.
# DENSITY REVISION: uses pre-corrected density fields in ÄBIN_compact; no re-calculation.
# Reindeer is excluded from all total cervid density predictors.
# Reconstructed from the supplied R console script, with verified duplicate corrections.
# IMPORTANT: Review statistical choices and data assumptions before manuscript use.
# Run in a fresh R session using Source. Do not continue past an error.

base_dir <- "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw"
compact_file <- file.path(base_dir, "abin_compact.csv")
output_dir <- file.path(base_dir, "birch_consolidated_outputs")
if (!file.exists(compact_file)) stop("Cannot find compact input: ", compact_file)
for (d in c("tables", "figures", "models", "audit"))
  dir.create(file.path(output_dir, d), recursive=TRUE, showWarnings=FALSE)
options(contrasts = c("contr.treatment", "contr.poly"))
options(na.print = NULL)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggeffects)
library(ggplot2)
library(patchwork)
library(broom.mixed)
library(sjPlot)
library(performance)
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
mean_keep_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}
max_keep_na <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}
convert_damage <- function(x) {
  x <- as.character(x)
  
  case_when(
    x == "0"      ~ 0,
    x == "≤10"    ~ 5,
    x == "11_25"  ~ 18,
    x == "26_50"  ~ 38,
    x == "51_75"  ~ 63,
    x == "76_100" ~ 88,
    TRUE ~ NA_real_
  )
}
standardise_area <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_squish() %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_replace_all("[_]", "") %>%
    str_replace_all("\\s+", "")
}
area_map <- c(
  "Lyksele"     = "Lycksele",
  "LYKSELE"     = "Lycksele",
  "lycksele"    = "Lycksele",
  "ÖsterMalma"  = "ÖsterMalma",
  "OsterMalma"  = "ÖsterMalma",
  "Ostermalma"  = "ÖsterMalma",
  "Östermalma"  = "ÖsterMalma",
  "Öster Malma" = "ÖsterMalma",
  "Oster Malma" = "ÖsterMalma",
  "Växjo"       = "Växjö"
)
fix_area_names <- function(df, area_col = area) {
  df %>%
    mutate(
      area_raw = {{ area_col }},
      area = dplyr::recode(area_raw, !!!area_map, .default = area_raw),
      area = dplyr::recode(area, !!!area_map, .default = area),
      area = standardise_area(area)
    ) %>%
    select(-area_raw)
}
ÄBIN_compact <- read.csv(
  compact_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
# Immediate pellet check from imported file
ÄBIN_compact %>%
  clean_names() %>%
  filter(year == 2025) %>%
  summarise(
    n = n(),
    moose_sum = sum(num(moose_pellets), na.rm = TRUE),
    red_deer_sum = sum(num(red_deer_pellets), na.rm = TRUE),
    small_deer_sum = sum(num(small_deer_pellets), na.rm = TRUE),
    reindeer_sum = sum(num(reindeer_pellets), na.rm = TRUE)
  )
Birch <- ÄBIN_compact %>%
  clean_names() %>%
  rename(
    stand_number     = stand,
    surveyer         = surveyor,
    pine_undamaged   = pine_unbrowsed,
    spruce_undamaged = spruce_ub
  ) %>%
  mutate(
    spruce_damaged = spruce_winter_damage_stems
  )
keep_cols <- c(
  "area", "year", "date", "stand_number", "plot", "surveyer",
  "nearest_tract", "productivity", "north", "east", "pct", "half_height",
  
  "pine_undamaged", "pine_stems", "pine_winter_damage_stems",
  "proportion_pine_damage_winter",
  
  "spruce_undamaged", "spruce_stems", "spruce_damaged",
  
  "downy_total", "downy_height", "downy_damage",
  "silver_total", "silver_height", "silver_damage",
  "rowan_total", "rowan_height", "rowan_damage",
  "aspen_total", "aspen_height", "aspen_damage",
  "salix_total", "salix_height", "salix_damage",
  "oak_total", "oak_height", "oak_damage",
  
  "moose_pellets",
  "red_deer_pellets",
  "small_deer_pellets",
  "reindeer_pellets",
  "wild_boar",
  "moose_pellets_only",
  "moose_pellets_log",
  "deer_pellets",
  "deer_pellets_log",
  "total_cervid_pellets",
  "total_cervid_pellets_log"
)
missing_keep <- setdiff(keep_cols, names(Birch))
if (length(missing_keep) > 0) {
  stop("These required columns are missing from Birch: ",
       paste(missing_keep, collapse = ", "))
}
Birch_clean <- Birch[, keep_cols]
numeric_cols <- c(
  "year", "north", "east", "half_height",
  "pine_undamaged", "pine_stems", "pine_winter_damage_stems",
  "proportion_pine_damage_winter",
  "spruce_undamaged", "spruce_stems", "spruce_damaged",
  "downy_total", "downy_height",
  "silver_total", "silver_height",
  "rowan_total", "rowan_height",
  "aspen_total", "aspen_height",
  "salix_total", "salix_height",
  "oak_total", "oak_height",
  "moose_pellets", "red_deer_pellets",
  "small_deer_pellets", "reindeer_pellets", "wild_boar",
  "moose_pellets_only", "moose_pellets_log",
  "deer_pellets", "deer_pellets_log",
  "total_cervid_pellets", "total_cervid_pellets_log"
)
Birch_clean <- Birch_clean %>%
  mutate(across(all_of(numeric_cols), num))
height_cols <- c(
  "half_height", "downy_height", "silver_height",
  "rowan_height", "aspen_height", "salix_height", "oak_height"
)
Birch_clean <- Birch_clean %>%
  mutate(
    across(
      all_of(height_cols),
      ~ case_when(
        is.na(.) ~ NA_real_,
        . > 8 ~ . / 100,
        TRUE ~ .
      )
    )
  )
damage_cols <- c(
  "downy_damage", "silver_damage", "rowan_damage",
  "aspen_damage", "salix_damage", "oak_damage"
)
Birch_clean <- Birch_clean %>%
  mutate(
    across(
      all_of(damage_cols),
      convert_damage,
      .names = "{.col}_N"
    )
  )
Birch_2425 <- Birch_clean %>%
  filter(year %in% c(2024, 2025)) %>%
  fix_area_names(area)
# Area-level latitude fallback for the small number of missing GPS locations
area_lat <- tibble::tribble(
  ~area,        ~area_lat,
  "Fredrika",    64.2056,
  "Furudal",     61.4302,
  "Lofsdalen",   62.0846,
  "Ljusdal",     61.9583,
  "Lycksele",    64.6845,
  "Nordmaling",  63.6590,
  "Råneå",       66.2720,
  "Sorsele",     65.6100,
  "Växjö",       57.1893,
  "Åtvidaberg",  58.3171,
  "ÖsterMalma",  58.7000,
  "Barksätter",  58.9776,
  "Malå",        65.2017
)
Birch_2425 <- Birch_2425 %>%
  left_join(area_lat, by = "area")
east_min  <- 200000
east_max  <- 1000000
north_min <- 6000000
north_max <- 8000000
Birch_2425 <- Birch_2425 %>%
  mutate(
    coord_swap_flag = ifelse(
      !is.na(north) & !is.na(east) &
        north >= east_min  & north <= east_max &
        east  >= north_min & east  <= north_max,
      TRUE, FALSE
    ),
    east_fixed  = ifelse(coord_swap_flag, north, east),
    north_fixed = ifelse(coord_swap_flag, east, north),
    east = east_fixed,
    north = north_fixed,
    east  = ifelse(east == 0, NA, east),
    north = ifelse(north == 0, NA, north),
    
    coord_type = case_when(
      is.na(east) | is.na(north) ~ "missing",
      east >= 10 & east <= 25 & north >= 55 & north <= 70 ~ "wgs84_ok",
      east >= 55 & east <= 70 & north >= 10 & north <= 25 ~ "wgs84_swapped",
      TRUE ~ "other"
    ),
    
    east_orig = east,
    north_orig = north,
    east  = ifelse(coord_type == "wgs84_swapped", north_orig, east_orig),
    north = ifelse(coord_type == "wgs84_swapped", east_orig, north_orig),
    
    coord_type_fixed = case_when(
      is.na(east) | is.na(north) ~ "missing",
      east >= 10 & east <= 25 & north >= 55 & north <= 70 ~ "wgs84",
      TRUE ~ "other"
    ),
    
    # Use coordinate latitude if available, otherwise area-level latitude
    north_model = case_when(
      coord_type_fixed == "wgs84" ~ north,
      is.na(north) & !is.na(area_lat) ~ area_lat,
      TRUE ~ area_lat
    )
  ) %>%
  select(
    -east_fixed, -north_fixed,
    -east_orig, -north_orig
  )
# ---------------------------------------------------------------
# VERIFIED SOURCE DENSITIES — DO NOT RECALCULATE FROM RAW PELLETS
# ---------------------------------------------------------------
# For 2024–2025, grouped non-missing values in ÄBIN_compact matched
# ÄBIN_combined corrected pellet densities (tolerance 1e-6).
# The 595 missingness discrepancies were confined to 2022 and
# are outside this analysis. Duplicate keys preclude a definitive
# row-level audit; do not interpret this as full provenance verification.
# Existing *_pellets columns are ALREADY corrected density estimates.
# Reindeer is excluded from total cervid density and all deer models.
required_density <- c("moose_pellets", "red_deer_pellets",
                      "small_deer_pellets")
if (!all(required_density %in% names(Birch_2425))) {
  stop("Missing corrected density column(s): ",
       paste(setdiff(required_density, names(Birch_2425)), collapse=", "))
}
Birch_2425 <- Birch_2425 %>%
  mutate(across(all_of(required_density), num)) %>%
  mutate(
    moose_density = moose_pellets,
    red_deer_density = red_deer_pellets,
    small_deer_density = small_deer_pellets,
    moose_density_log = log1p(moose_density),
    red_deer_density_log = log1p(red_deer_density),
    small_deer_density_log = log1p(small_deer_density),
    # NA in any constituent means total density is unknown.
    total_cervid_density = moose_density + red_deer_density +
      small_deer_density,
    total_cervid_density_log = log1p(total_cervid_density),
    # Legacy variable names retained for the existing model/figure code.
    moose_pellets_only = moose_density,
    moose_pellets_log = moose_density_log,
    red_deer_pellets_log = red_deer_density_log,
    small_deer_pellets_log = small_deer_density_log,
    deer_pellets = red_deer_density + small_deer_density,
    deer_pellets_log = log1p(deer_pellets),
    total_cervid_pellets = total_cervid_density,
    total_cervid_pellets_log = total_cervid_density_log
  )
if (any(vapply(Birch_2425[required_density],
               function(z) any(z < 0, na.rm=TRUE), logical(1)))) {
  stop("Negative corrected density values found; inspect input.")
}
write.csv(
  Birch_2425 %>% select(year, area, date, all_of(required_density),
                        moose_density, red_deer_density,
                        small_deer_density, total_cervid_density,
                        total_cervid_density_log),
  file.path(output_dir, "audit", "existing_cervid_densities_2024_2025.csv"),
  row.names=FALSE
)
write.csv(
  Birch_2425 %>% filter(if_any(all_of(required_density), is.na)),
  file.path(output_dir, "audit", "missing_cervid_density_rows.csv"),
  row.names=FALSE
)
cat("\\n--- EXISTING CORRECTED DENSITY AUDIT (REINDEER EXCLUDED) ---\\n")
print(Birch_2425 %>% group_by(year) %>% summarise(
  n=n(),
  moose_nonmissing=sum(!is.na(moose_density)),
  red_deer_nonmissing=sum(!is.na(red_deer_density)),
  small_deer_nonmissing=sum(!is.na(small_deer_density)),
  total_nonmissing=sum(!is.na(total_cervid_density)),
  .groups="drop"
))
# Keep the original data unchanged for auditing
Birch_2425 <- Birch_2425 %>%
  mutate(
    original_plot = as.character(plot),
    valid_plot_id =
      !is.na(year) &
      !is.na(area) &
      !is.na(stand_number) &
      !is.na(plot) &
      nzchar(trimws(as.character(area))) &
      nzchar(trimws(as.character(stand_number))) &
      nzchar(trimws(as.character(plot)))
  )
write.csv(
  filter(Birch_2425, !valid_plot_id),
  file.path(output_dir, "audit", "incomplete_plot_identifiers.csv"),
  row.names = FALSE
)
valid_source <- Birch_2425 %>%
  filter(valid_plot_id)
valid_source <- valid_source %>%
  mutate(
    region = case_when(
      area %in% c(
        "Fredrika", "Lögda", "Lycksele",
        "Nordmaling", "Råneå", "Sorsele", "Malå"
      ) ~ "North",
      
      area %in% c(
        "Furudal", "Ljusdal", "Lofsdalen"
      ) ~ "Centre",
      
      area %in% c(
        "ÖsterMalma", "Åtvidaberg",
        "Växjö", "Barksätter"
      ) ~ "South",
      
      TRUE ~ NA_character_
    ),
    region = factor(region, levels = c("South", "Centre", "North"))
  )
if (anyNA(valid_source$region)) {
  stop("Some study areas have no regional classification.")
}
print(valid_source %>% count(area, region))
if (!all(c("north", "east", "coord_type_fixed") %in%
         names(valid_source))) {
  stop("Required coordinate columns are missing.")
}
missing_coords <- valid_source %>%
  filter(is.na(north) | is.na(east))
# Save all affected records for inspection
write.csv(
  missing_coords,
  file.path(output_dir, "audit", "missing_plot_coordinates.csv"),
  row.names = FALSE
)
# Print the number of affected records by area and year
missing_coord_summary <- missing_coords %>%
  count(year, area, coord_type_fixed, name = "n_records")
print(as.data.frame(missing_coord_summary))
cat(
  "\nTotal records with missing coordinates:",
  nrow(missing_coords), "\n"
)
# Print coordinate-system summary
print(
  as.data.frame(
    valid_source %>%
      count(coord_type_fixed)
  )
)

# Coordinates must be in a known, consistent system.
# WGS84 and projected coordinates are not directly interchangeable.
print(valid_source %>% count(coord_type_fixed))
# All non-missing coordinates must be WGS84
unexpected_coords <- valid_source %>%
  filter(
    !is.na(coord_type_fixed),
    !coord_type_fixed %in% c("wgs84", "missing")
  )
if (nrow(unexpected_coords) > 0) {
  stop("Unexpected coordinate systems found.")
}
cat("Coordinate formats checked.\n")
# Retain missing tract as an explicit value rather than NA
valid_source <- valid_source %>%
  mutate(
    tract_id = if_else(
      is.na(nearest_tract) |
        !nzchar(trimws(as.character(nearest_tract))),
      "TRACT_MISSING",
      trimws(as.character(nearest_tract))
    ),
    
    stand_id = interaction(
      year, area, tract_id, stand_number,
      drop = TRUE, sep = "__"
    ),
    
    # Coordinates identify the physical sampling location
    # within each stand and inventory year.
    location_key = case_when(
      !is.na(north) & !is.na(east) ~ paste0(
        "GPS_",
        format(north, scientific = FALSE, digits = 15),
        "_",
        format(east, scientific = FALSE, digits = 15)
      ),
      
      TRUE ~ paste0(
        "PLOT_",
        trimws(as.character(plot))
      )
    ),
    
    plot_id = interaction(
      stand_id, location_key,
      drop = TRUE, sep = "__"
    )
  )
missing_coords_check <- valid_source %>%
  filter(is.na(north) | is.na(east)) %>%
  select(
    year, area, nearest_tract,
    stand_number, plot, surveyer,
    stand_id, location_key, plot_id
  )
print(as.data.frame(missing_coords_check))
fallback_duplicates <- missing_coords_check %>%
  count(stand_id, location_key) %>%
  filter(n > 1)
if (nrow(fallback_duplicates) > 0) stop("Repeated missing-coordinate fallback plot IDs")
duplicate_locations <- valid_source %>%
  count(stand_id, plot_id, name = "n_records") %>%
  filter(n_records > 1)
write.csv(
  duplicate_locations,
  file.path(output_dir, "audit", "duplicate_physical_locations.csv"),
  row.names = FALSE
)
# Compare the actual measurement variables, not identifiers
measurement_cols <- c(
  "pine_undamaged", "pine_stems",
  "pine_winter_damage_stems",
  "spruce_undamaged", "spruce_stems", "spruce_damaged",
  "downy_total", "downy_height", "downy_damage_N",
  "silver_total", "silver_height", "silver_damage_N",
  "rowan_total", "rowan_height", "rowan_damage_N",
  "aspen_total", "aspen_height", "aspen_damage_N",
  "salix_total", "salix_height", "salix_damage_N",
  "oak_total", "oak_height", "oak_damage_N",
  "moose_pellets", "red_deer_pellets",
  "small_deer_pellets", "reindeer_pellets",
  "pct", "half_height"
)
measurement_cols <- intersect(
  measurement_cols, names(valid_source)
)
# Audit duplicate records and distinguish missingness from contradictory values.
location_conflicts <- valid_source %>%
  semi_join(duplicate_locations, by = c("stand_id", "plot_id")) %>%
  group_by(stand_id, plot_id) %>%
  summarise(
    across(all_of(measurement_cols), ~ n_distinct(.x, na.rm = FALSE)),
    .groups = "drop"
  ) %>%
  filter(if_any(all_of(measurement_cols), ~ .x > 1))

write.csv(location_conflicts,
          file.path(output_dir, "audit", "conflicting_plot_records.csv"),
          row.names = FALSE)

# Never resolve contradictory non-missing measurements automatically.
nonmissing_conflicts <- valid_source %>%
  semi_join(duplicate_locations, by = c("stand_id", "plot_id")) %>%
  group_by(stand_id, plot_id) %>%
  summarise(
    across(all_of(measurement_cols), ~ n_distinct(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(if_any(all_of(measurement_cols), ~ .x > 1))

if (nrow(nonmissing_conflicts) > 0) {
  write.csv(nonmissing_conflicts,
            file.path(output_dir, "audit", "contradictory_measurements.csv"),
            row.names = FALSE)
  stop("Conflicting non-missing measurements: review audit/contradictory_measurements.csv")
}

# Only two known incomplete pairs may have different missingness patterns.
expected_stands <- c("2024__ÖsterMalma__45__4_4", "2025__ÖsterMalma__29__6")
if (nrow(location_conflicts) != 2L ||
    !setequal(as.character(location_conflicts$stand_id), expected_stands)) {
  stop("Unexpected differences among duplicate records: review audit/conflicting_plot_records.csv")
}

# Prefer the most complete observation; ties retain the first source row.
valid_source$completeness_score <- rowSums(
  !is.na(valid_source[, measurement_cols, drop = FALSE])
)
source_plots <- valid_source %>%
  group_by(stand_id, plot_id) %>%
  arrange(desc(completeness_score), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-completeness_score)

stopifnot(nrow(source_plots) == n_distinct(source_plots$plot_id))
cat("\nOriginal records:", nrow(valid_source),
    "\nUnique physical plots:", nrow(source_plots),
    "\nDuplicate records removed:", nrow(valid_source) - nrow(source_plots), "\n")
write.csv(source_plots, file.path(output_dir, "audit", "unique_physical_plots.csv"), row.names = FALSE)

source_plots <- source_plots %>%
  mutate(
    year = factor(year),
    area = factor(area),
    
    pct = case_when(
      tolower(trimws(as.character(pct))) %in%
        c("yes", "y", "ja", "j", "true", "1", "pct") ~ "PCT",
      
      tolower(trimws(as.character(pct))) %in%
        c("no", "n", "nej", "false", "0", "not pct", "no pct") ~ "No PCT",
      
      TRUE ~ NA_character_
    ),
    
    pct = factor(pct, levels = c("No PCT", "PCT")),
    
    downy_total = as.numeric(downy_total),
    silver_total = as.numeric(silver_total),
    
    total_birch = if_else(
      !is.na(downy_total) | !is.na(silver_total),
      coalesce(downy_total, 0) + coalesce(silver_total, 0),
      NA_real_
    ),
    
    prop_s_plot = if_else(
      total_birch > 0,
      coalesce(silver_total, 0) / total_birch,
      NA_real_
    )
  )
plot_long <- source_plots %>%
  select(
    year, area, stand_number, plot,
    stand_id, plot_id, pct, region, north_model,
    prop_s_plot, total_birch,
    total_cervid_pellets_log,
    downy_total, silver_total,
    downy_height, silver_height,
    downy_damage_N, silver_damage_N
  ) %>%
  pivot_longer(
    cols = c(
      downy_total, silver_total,
      downy_height, silver_height,
      downy_damage_N, silver_damage_N
    ),
    names_to = c("sp", ".value"),
    names_pattern = "(downy|silver)_(total|height|damage_N)"
  ) %>%
  mutate(
    species = factor(
      if_else(sp == "downy", "Downy", "Silver"),
      levels = c("Downy", "Silver")
    ),
    stem_count = total,
    height = height
  ) %>%
  select(-sp, -total) %>%
  filter(!is.na(stem_count), !is.na(prop_s_plot))
stand_pellets <- source_plots %>%
  group_by(year, area, stand_id) %>%
  summarise(
    total_cervid_pellets_log_mean =
      mean_keep_na(total_cervid_pellets_log),
    n_pellet_plots = sum(!is.na(total_cervid_pellets_log)),
    n_sampled_plots = n(),
    .groups = "drop"
  )
stand_wide <- plot_long %>%
  group_by(year, area, stand_id) %>%
  summarise(
    pct = first(pct),
    region = first(region),
    mean_north = mean_keep_na(north_model),
    
    downy_stems_mean =
      mean_keep_na(stem_count[species == "Downy"]),
    silver_stems_mean =
      mean_keep_na(stem_count[species == "Silver"]),
    
    downy_damage_mean =
      mean_keep_na(damage_N[species == "Downy"]),
    silver_damage_mean =
      mean_keep_na(damage_N[species == "Silver"]),
    
    downy_height_mean =
      mean_keep_na(height[species == "Downy"]),
    silver_height_mean =
      mean_keep_na(height[species == "Silver"]),
    
    .groups = "drop"
  ) %>%
  left_join(
    stand_pellets,
    by = c("year", "area", "stand_id")
  ) %>%
  # Corrected stand composition is calculated below from unique physical plots.
  # Species-specific mean stems above are retained for the descriptive figures.
  select(-any_of(c("total_stems_mean", "prop_s_stand")))

# Corrected calculation: ratio of sums across valid sampled plots.
# A missing species count is zero ONLY when the other species was recorded.
# A plot with both counts missing is excluded from the denominator.
corrected_stand_composition <- source_plots %>%
  mutate(
    valid_birch_count = !is.na(downy_total) | !is.na(silver_total),
    downy_count = if_else(valid_birch_count, coalesce(as.numeric(downy_total),0), NA_real_),
    silver_count = if_else(valid_birch_count, coalesce(as.numeric(silver_total),0), NA_real_),
    birch_count = downy_count + silver_count
  ) %>%
  group_by(year,area,stand_id) %>%
  summarise(
    valid_plots=sum(!is.na(birch_count)),
    total_downy=sum(downy_count,na.rm=TRUE),
    total_silver=sum(silver_count,na.rm=TRUE),
    total_birch_stems=sum(birch_count,na.rm=TRUE),
    total_stems_mean=mean_keep_na(birch_count),
    corrected_downy_stems_mean=if_else(valid_plots > 0, total_downy / valid_plots, NA_real_),
    corrected_silver_stems_mean=if_else(valid_plots > 0, total_silver / valid_plots, NA_real_),
    .groups="drop"
  ) %>%
  mutate(prop_s_stand=if_else(total_birch_stems>0,
                              total_silver/total_birch_stems,NA_real_))
stand_wide <- stand_wide %>%
  left_join(corrected_stand_composition %>%
              select(year,area,stand_id,total_stems_mean,prop_s_stand,
                     corrected_downy_stems_mean, corrected_silver_stems_mean),
            by=c("year","area","stand_id"))
write.csv(corrected_stand_composition,
          file.path(output_dir,"audit","corrected_stand_composition.csv"),row.names=FALSE)

# Replace legacy means (which excluded plots with missing species counts)
# with corrected zero-inclusive species means before building ALL models.
stand_wide <- stand_wide %>%
  dplyr::mutate(
    downy_stems_mean = corrected_downy_stems_mean,
    silver_stems_mean = corrected_silver_stems_mean
  )
stand_long <- stand_wide %>%
  pivot_longer(
    cols = c(
      downy_stems_mean, silver_stems_mean,
      downy_damage_mean, silver_damage_mean,
      downy_height_mean, silver_height_mean
    ),
    names_to = c("sp", ".value"),
    names_pattern =
      "(downy|silver)_(stems_mean|damage_mean|height_mean)"
  ) %>%
  mutate(
    species = factor(
      if_else(sp == "downy", "Downy", "Silver"),
      levels = c("Downy", "Silver")
    ),
    stem_count = stems_mean,
    damage_N = damage_mean,
    height = height_mean
  ) %>%
  select(-sp, -stems_mean, -damage_mean, -height_mean) %>%
  filter(
    !is.na(prop_s_stand),
    !is.na(stem_count),
    stem_count > 0
  )
stand_covariates <- stand_long %>%
  select(
    year, area, stand_id, species, prop_s_stand, mean_north,
    stand_total_birch = total_stems_mean,
    stand_height = height,
    total_cervid_pellets_log_mean
  )
plot_response_stand_cov <- plot_long %>%
  select(
    year, area, stand_id, plot_id,
    species, damage_N, pct, region
  ) %>%
  left_join(
    stand_covariates,
    by = c("year", "area", "stand_id", "species"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    mean_north_c =
      mean_north - mean(stand_long$mean_north, na.rm = TRUE),
    
    total_birch_c =
      stand_total_birch -
      mean(stand_wide$total_stems_mean, na.rm = TRUE),
    
    height_c =
      stand_height -
      mean(stand_long$height, na.rm = TRUE)
  ) %>%
  filter(
    if_all(
      all_of(c(
        "damage_N", "species", "prop_s_stand",
        "mean_north_c", "total_birch_c", "height_c",
        "total_cervid_pellets_log_mean",
        "pct", "year", "region"
      )),
      ~ !is.na(.x)
    )
  ) %>%
  droplevels()
# Plot-level total_birch already comes from the physical plot in source_plots.
# Retain stem_count_c ONLY for descriptive/legacy audit purposes.
plot_long <- plot_long %>%
  mutate(
    north_c = north_model - mean(north_model, na.rm = TRUE),
    stem_count_c = stem_count - mean(stem_count, na.rm = TRUE),
    total_birch_c = total_birch - mean(source_plots$total_birch, na.rm = TRUE),
    height_c = height - mean(height, na.rm = TRUE)
  )
stand_long <- stand_long %>%
  mutate(
    mean_north_c =
      mean_north - mean(mean_north, na.rm = TRUE),
    stem_count_c =
      stem_count - mean(stem_count, na.rm = TRUE),
    total_birch_c =
      total_stems_mean - mean(stand_wide$total_stems_mean, na.rm = TRUE),
    height_c = height - mean(height, na.rm = TRUE)
  )
# Audit species-specific descriptive abundance; models below use total birch abundance.
stopifnot(isTRUE(all.equal(
  stand_long$stem_count,
  ifelse(as.character(stand_long$species) == "Downy",
         stand_long$corrected_downy_stems_mean,
         stand_long$corrected_silver_stems_mean),
  check.attributes = FALSE
)))
write.csv(stand_long %>%
            dplyr::select(year,area,stand_id,species,stem_count,stem_count_c,
                          prop_s_stand),
          file.path(output_dir,"audit","species_specific_stems_DESCRIPTIVE_ONLY.csv"),
          row.names=FALSE)
plot_data <- plot_long %>%
  filter(stem_count > 0) %>%
  filter(
    if_all(
      all_of(c(
        "damage_N", "species", "prop_s_plot",
        "north_c", "total_birch_c", "height_c",
        "total_cervid_pellets_log",
        "pct", "year", "region"
      )),
      ~ !is.na(.x)
    )
  ) %>%
  droplevels()
stand_data <- stand_long %>%
  filter(
    if_all(
      all_of(c(
        "damage_N", "species", "prop_s_stand",
        "mean_north_c", "total_birch_c", "height_c",
        "total_cervid_pellets_log_mean",
        "pct", "year", "region"
      )),
      ~ !is.na(.x)
    )
  ) %>%
  droplevels()
# ABUNDANCE VALIDATION: one total per stand-year, identical for both species.
stopifnot(
  !anyNA(stand_data$total_birch_c),
  !anyNA(plot_data$total_birch_c),
  all(stand_data$total_stems_mean >= stand_data$stem_count - 1e-8),
  all(plot_data$total_birch >= plot_data$stem_count - 1e-8)
)
stand_total_check <- stand_data %>%
  group_by(year, area, stand_id) %>%
  summarise(n_totals=n_distinct(total_stems_mean), .groups="drop")
stopifnot(all(stand_total_check$n_totals == 1L))
write.csv(stand_data %>%
            select(year, area, stand_id, species, stem_count, total_stems_mean,
                   total_birch_c, prop_s_stand),
          file.path(output_dir,"audit","TOTAL_birch_abundance_used_by_stand_models.csv"),
          row.names=FALSE)
write.csv(plot_data %>%
            select(year, area, stand_id, plot_id, species, stem_count, total_birch,
                   total_birch_c, prop_s_plot),
          file.path(output_dir,"audit","TOTAL_birch_abundance_used_by_plot_models.csv"),
          row.names=FALSE)
audit <- bind_rows(
  tibble(
    level = "plot",
    rows = nrow(plot_data),
    stands = n_distinct(plot_data$stand_id),
    plots = n_distinct(plot_data$plot_id),
    areas = n_distinct(plot_data$area)
  ),
  tibble(
    level = "stand",
    rows = nrow(stand_data),
    stands = n_distinct(stand_data$stand_id),
    plots = NA_integer_,
    areas = n_distinct(stand_data$area)
  )
)
write.csv(
  audit,
  file.path(output_dir, "audit", "analysis_sample_sizes.csv"),
  row.names = FALSE
)
write.csv(
  plot_data,
  file.path(output_dir, "audit", "plot_model_data.csv"),
  row.names = FALSE
)
write.csv(
  stand_data,
  file.path(output_dir, "audit", "stand_model_data.csv"),
  row.names = FALSE
)
write.csv(
  plot_response_stand_cov,
  file.path(output_dir, "audit", "plot_response_stand_predictors.csv"),
  row.names = FALSE
)
print(audit)
cat("\nSection 2 complete: plot IDs and regions corrected.\n")
###############################################################################
# 3. MODEL SPECIFICATIONS — TOTAL CERVID DENSITY
###############################################################################
# A model-comparison pair uses exactly the same rows and random effects.
# Comparisons use ML, final estimates are refitted with REML.
stand_add <- damage_N ~ species + prop_s_stand + mean_north_c + total_birch_c +
  height_c + total_cervid_pellets_log_mean + pct + year + (1|area/stand_id)
stand_int <- damage_N ~ species*(prop_s_stand + mean_north_c + total_birch_c +
                                   height_c + total_cervid_pellets_log_mean) + pct + year + (1|area/stand_id)
# Plot models use TOTAL birch stems per physical plot, centred across plots.
# Stand models use mean TOTAL birch stems per valid sampled plot, centred across stands.
plot_add <- damage_N ~ species + prop_s_plot + north_c + total_birch_c +
  height_c + total_cervid_pellets_log + pct + year + (1|area/stand_id/plot_id)
plot_int <- damage_N ~ species*(prop_s_plot + north_c + total_birch_c +
                                  height_c + total_cervid_pellets_log) + pct + year + (1|area/stand_id/plot_id)
fit <- function(formula, data, reml=TRUE) {
  if (nrow(data)<30) stop("Insufficient rows for model")
  if (anyNA(data[,all.vars(lme4::nobars(formula)),drop=FALSE]))
    stop("Incomplete model predictors detected; inspect model-data audit")
  lmer(formula,data=data,REML=reml,control=lmerControl(optimizer="bobyqa"), na.action=na.fail)
}
models <- list()
models$stand_add <- fit(stand_add,stand_data)
models$stand_int <- fit(stand_int,stand_data)
models$plot_add <- fit(plot_add,plot_data)
models$plot_int <- fit(plot_int,plot_data)
# B: same stand-level predictors as A, but individual plot damage responses.
stand_cov_add <- damage_N ~ species + prop_s_stand + mean_north_c +
  total_birch_c + height_c + total_cervid_pellets_log_mean +
  pct + year + (1|area/stand_id)
stand_cov_int <- damage_N ~ species*(prop_s_stand + mean_north_c +
                                       total_birch_c + height_c + total_cervid_pellets_log_mean) +
  pct + year + (1|area/stand_id)
models$plot_response_stand_add <- fit(stand_cov_add, plot_response_stand_cov)
models$plot_response_stand_int <- fit(stand_cov_int, plot_response_stand_cov)
comparison_plot_response_stand <- anova(
  fit(stand_cov_add, plot_response_stand_cov, FALSE),
  fit(stand_cov_int, plot_response_stand_cov, FALSE))
write.csv(as.data.frame(comparison_plot_response_stand),
          file.path(output_dir, "tables", "plot_response_stand_predictors_ML.csv"))
# ML likelihood-ratio tests: additive vs interaction, same sample by design.
comparison_stand <- anova(fit(stand_add,stand_data,FALSE),fit(stand_int,stand_data,FALSE))
comparison_plot <- anova(fit(plot_add,plot_data,FALSE),fit(plot_int,plot_data,FALSE))
write.csv(as.data.frame(comparison_stand),file.path(output_dir,"tables","stand_additive_vs_interaction_ML.csv"))
write.csv(as.data.frame(comparison_plot),file.path(output_dir,"tables","plot_additive_vs_interaction_ML.csv"))
# Species-specific main stand and supplementary plot models.
stand_sp <- damage_N ~ prop_s_stand + mean_north_c + total_birch_c + height_c +
  total_cervid_pellets_log_mean + pct + year + (1|area)
# A: one species-specific stand mean per stand-occasion; a stand random intercept
# cannot be estimated with one observation per stand-occasion.
# A single species contributes at most one row per physical plot.
# Therefore plot_id cannot be a random intercept in species-specific models.
# Retain stand nesting within area.
plot_sp <- damage_N ~ prop_s_plot + north_c + total_birch_c + height_c +
  total_cervid_pellets_log + pct + year + (1|area/stand_id)
for (sp in c("Downy","Silver")) {
  models[[paste0("stand_",tolower(sp))]] <- fit(stand_sp,droplevels(filter(stand_data,species==sp)))
  models[[paste0("plot_",tolower(sp))]] <- fit(plot_sp,droplevels(filter(plot_data,species==sp)))
  models[[paste0("plot_response_stand_",tolower(sp))]] <- fit(
    damage_N ~ prop_s_stand + mean_north_c + total_birch_c + height_c +
      total_cervid_pellets_log_mean + pct + year + (1|area/stand_id),
    droplevels(filter(plot_response_stand_cov, species == sp)))
}
# Regional species-interaction models; no latitude term within region, consistent
# with the earlier regional models. Full region interaction formally tests contrasts.
stand_reg <- damage_N ~ species*(prop_s_stand + total_birch_c + height_c +
                                   total_cervid_pellets_log_mean) + pct + year + (1|area/stand_id)
plot_reg <- damage_N ~ species*(prop_s_plot + total_birch_c + height_c +
                                  total_cervid_pellets_log) + pct + year + (1|area/stand_id/plot_id)
for (rg in c("South","Centre","North")) {
  models[[paste0("stand_",tolower(rg))]] <- fit(stand_reg,droplevels(filter(stand_data,region==rg)))
  models[[paste0("plot_",tolower(rg))]] <- fit(plot_reg,droplevels(filter(plot_data,region==rg)))
  models[[paste0("plot_response_stand_",tolower(rg))]] <- fit(
    damage_N ~ species*(prop_s_stand + total_birch_c + height_c +
                          total_cervid_pellets_log_mean) + pct + year + (1|area/stand_id),
    droplevels(filter(plot_response_stand_cov, region == rg)))
}
models$stand_region_interaction <- fit(
  damage_N ~ species*prop_s_stand*region + species*(total_birch_c+height_c+
                                                      total_cervid_pellets_log_mean) + pct + year + (1|area/stand_id),stand_data)
models$plot_region_interaction <- fit(
  damage_N ~ species*prop_s_plot*region + species*(total_birch_c+height_c+
                                                     total_cervid_pellets_log) + pct + year + (1|area/stand_id/plot_id),plot_data)
###############################################################################
# 4. OUTPUTS, DIAGNOSTICS AND REGIONAL FIGURE RECONSTRUCTION
###############################################################################
for (nm in names(models)) {
  m <- models[[nm]]
  saveRDS(m,file.path(output_dir,"models",paste0(nm,".rds")))
  write.csv(as.data.frame(coef(summary(m))),
            file.path(output_dir,"tables",paste0(nm,"_fixed_effects.csv")))
  capture.output(summary(m),file=file.path(output_dir,"audit",paste0(nm,"_summary.txt")))
  capture.output(performance::check_singularity(m),
                 file=file.path(output_dir,"audit",paste0(nm,"_singularity.txt")))
}
# Reconstruct original South/Centre/North figure from regional stand models.
# These predictions hold other covariates at reference/typical values; verify
# against original ggpredict call before treating this as a reproduction.
region_preds <- lapply(c("South","Centre","North"),function(rg){
  m <- models[[paste0("stand_",tolower(rg))]]
  p <- as.data.frame(ggeffects::ggpredict(m,terms=c("prop_s_stand [0:1 by=0.05]","species")))
  p$region <- rg
  p
})
pred <- bind_rows(region_preds)
regional_figure <- ggplot(pred,aes(x=x,y=predicted,colour=group,fill=group))+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=.18,colour=NA)+
  geom_line(linewidth=1)+
  facet_wrap(~region,scales="free_y")+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("Pure Downy","Mixed","Pure Silver"))+
  labs(x="Stand-level silver birch proportion",y="Predicted browsing damage (%)",
       colour="Species",fill="Species")+theme_bw()
ggsave(file.path(output_dir,"figures","regional_stand_interactions_RECONSTRUCTION.png"),
       regional_figure,width=12,height=4,dpi=300)
# Audit figure-critical estimates: names of terms can vary by factor contrasts.
regional_terms <- bind_rows(lapply(c("South","Centre","North"),function(rg){
  m <- models[[paste0("stand_",tolower(rg))]]
  cf <- as.data.frame(coef(summary(m)))
  cf$term <- rownames(cf); cf$region <- rg
  cf[grepl("species.*:prop_s_stand|prop_s_stand:species",cf$term),,drop=FALSE]
}))
write.csv(regional_terms,file.path(output_dir,"audit","regional_species_proportion_terms.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(output_dir,"audit","sessionInfo.txt"))
message("Completed only if sourced from the beginning without errors. Check duplicate keys and audits before manuscript use.")
###############################################################################
# STAND-LEVEL MANUSCRIPT FIGURES AND ACCESSIBLE TABLES
# Included in this single standalone script; no other R scripts needed.
# All results are recomputed; numbers from historical figures are NOT copied.
###############################################################################
stopifnot(exists('models'), exists('stand_data'), exists('stand_long'),
          exists('source_plots'), exists('output_dir'))
for (pkg in c('dplyr','tidyr','ggplot2','lme4','lmerTest','ggeffects','emmeans')) {
  if (!requireNamespace(pkg, quietly=TRUE)) stop('Install missing package: ',pkg)
}
fig_dir <- file.path(output_dir,'figures','manuscript_stand')
tab_dir <- file.path(output_dir,'tables','manuscript_stand')
dir.create(fig_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(tab_dir,recursive=TRUE,showWarnings=FALSE)
cols <- c(Downy='#59B874', Silver='#627FB2')

# Helper: tables viewable in RStudio (Environment > table > click, or View()).
coef_table <- function(model, label) {
  z <- as.data.frame(coef(summary(model)))
  z$Predictor <- rownames(z)
  z$Model <- label
  rownames(z) <- NULL
  z <- z[,c('Model','Predictor',setdiff(names(z),c('Model','Predictor')))]
  z
}
stand_models <- models[grepl('^stand_',names(models))]
# Basic descriptive analyses use stand-level species observations.
basic_stand_data <- stand_long %>%
  dplyr::filter(!is.na(damage_N),!is.na(stem_count),stem_count>0,
                !is.na(mean_north_c),!is.na(species)) %>% droplevels()
basic_models <- list(
  basic_damage_species=lm(damage_N~species,data=basic_stand_data),
  basic_stems_species=lm(stem_count~species,data=basic_stand_data),
  basic_damage_latitude=lm(damage_N~species*mean_north_c,data=basic_stand_data),
  basic_stems_latitude=lm(stem_count~species*mean_north_c,data=basic_stand_data)
)
all_stand_models <- c(basic_models,stand_models)
stand_results_table <- dplyr::bind_rows(lapply(names(all_stand_models),function(nm)
  coef_table(all_stand_models[[nm]],nm)))
stand_model_overview <- dplyr::bind_rows(lapply(names(all_stand_models),function(nm){
  m <- all_stand_models[[nm]]
  data.frame(Model=nm,Observations=nobs(m),AIC=AIC(m),BIC=BIC(m),
             Singular=if(inherits(m,'merMod')) lme4::isSingular(m) else NA)
}))
stand_descriptives <- basic_stand_data %>% dplyr::group_by(species) %>%
  dplyr::summarise(n=n(),n_stands=dplyr::n_distinct(stand_id),
                   mean_damage=mean(damage_N),sd_damage=sd(damage_N),
                   mean_stems=mean(stem_count),sd_stems=sd(stem_count),.groups='drop')
write.csv(stand_results_table,file.path(tab_dir,'ALL_stand_model_coefficients.csv'),row.names=FALSE)
write.csv(stand_model_overview,file.path(tab_dir,'stand_model_overview.csv'),row.names=FALSE)
write.csv(stand_descriptives,file.path(tab_dir,'stand_species_descriptives.csv'),row.names=FALSE)

# Figure 1: descriptive points and linear species trends across latitude.
# One row per species x stand; trend is descriptive, NOT a mixed-model prediction.
# Lögda is displayed at Fredrika's latitude only for this figure.
# The underlying model data and fitted manuscript models are not modified.
lat_df <- basic_stand_data %>%
  dplyr::mutate(latitude=ifelse(as.character(area)=="Lögda",64.2056,mean_north))

# Use one shared tick for Fredrika and Lögda, with a two-line label.
# Dodge other area labels into two rows to avoid overlaps.
area_axis <- area_lat %>%
  dplyr::filter(area %in% unique(as.character(lat_df$area)) |
                  (area=="Fredrika" & "Lögda" %in% as.character(lat_df$area))) %>%
  dplyr::arrange(area_lat)
area_breaks <- area_axis$area_lat
area_labels <- ifelse(area_axis$area=="Fredrika",
                      "Fredrika/Lögda",area_axis$area)
latitude_axis <- scale_x_continuous(
  breaks=area_breaks, labels=area_labels,
  guide=guide_axis(n.dodge=1),
  expand=expansion(mult=c(0.05,0.05))
)
latitude_theme <- theme_classic() + theme(
  axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=11),
  axis.text.y=element_text(size=12),
  axis.title.y=element_text(size=14),
  plot.title=element_text(size=16),
  plot.margin=margin(t=12,r=12,b=35,l=12),
  legend.text=element_text(size=13)
)
# Figure 1 statistics: ordinary species-specific LM slopes and R2.
# These describe the drawn geom_smooth lines, not mixed-model inference.
latitude_stats <- dplyr::bind_rows(lapply(c("stem_count", "damage_N"), function(y) {
  dplyr::bind_rows(lapply(c("Downy", "Silver"), function(sp) {
    dd <- lat_df %>% dplyr::filter(species == sp, !is.na(.data[[y]]))
    m <- stats::lm(stats::reformulate("latitude", response = y), data = dd)
    data.frame(panel = y, species = sp, n = stats::nobs(m),
               p = coef(summary(m))["latitude", "Pr(>|t|)"],
               R2 = summary(m)$r.squared)
  }))
}))
write.csv(latitude_stats, file.path(tab_dir, "Figure_1_latitude_statistics.csv"), row.names=FALSE)
fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < .001, "<0.001", sprintf("%.3f",p)))
latitude_label <- function(response) {
  z <- latitude_stats %>% dplyr::filter(panel == response)
  interaction_model <- stats::lm(stats::reformulate("species * latitude", response=response),
                                 data=lat_df)
  ct <- coef(summary(interaction_model))
  term <- grep("speciesSilver:latitude|latitude:speciesSilver", rownames(ct), value=TRUE)
  p_int <- if(length(term)==1) ct[term,"Pr(>|t|)"] else NA_real_
  paste0(paste(sprintf("%s: p = %s, R² = %.3f", z$species,fmt_p(z$p),z$R2),collapse="\n"),
         "\nSpecies × latitude: p = ",fmt_p(p_int))
}
p_stems <- ggplot(lat_df,aes(latitude,stem_count,colour=species))+
  geom_point(alpha=.35,size=1.6)+geom_smooth(method='lm',formula=y~x,se=TRUE,linewidth=.9)+
  annotate('text',x=-Inf,y=Inf,label=latitude_label('stem_count'),
           hjust=-.05,vjust=1.15,size=3.2,colour='black')+
  scale_colour_manual(values=cols)+latitude_axis+
  coord_cartesian(ylim=c(0,100))+
  labs(title='A  Stand-level mean birch stems across latitude',x=NULL,
       y='Mean birch stems per sampled plot',colour=NULL)+latitude_theme
p_damage <- ggplot(lat_df,aes(latitude,damage_N,colour=species))+
  geom_point(alpha=.35,size=1.6)+geom_smooth(method='lm',formula=y~x,se=TRUE,linewidth=.9)+
  annotate('text',x=-Inf,y=Inf,label=latitude_label('damage_N'),
           hjust=-.05,vjust=1.15,size=3.2,colour='black')+
  scale_colour_manual(values=cols)+latitude_axis+
  coord_cartesian(ylim=c(0,100))+
  labs(title='B  Stand-level mean birch browsing damage across latitude',
       x=NULL,y='Mean browsing damage per stand (%)',colour=NULL)+latitude_theme
if(requireNamespace('patchwork',quietly=TRUE)) {
  figure1 <- p_stems+p_damage+patchwork::plot_layout(guides='collect')
} else { figure1 <- p_damage }
print(figure1)
ggsave(file.path(fig_dir,'Figure_1_latitude_stems_damage.png'),figure1,width=20,height=9,dpi=300)

# Figure 2: regional interaction predictions from current stand models.
# Uses the SAME models as the manuscript tables, not historical coefficients.
# Uses the existing corrected cervid densities from ÄBIN_compact.
region_pred <- dplyr::bind_rows(lapply(c('South','Centre','North'),function(rg){
  m <- models[[paste0('stand_',tolower(rg))]]
  p <- as.data.frame(ggeffects::ggpredict(m,terms=c('prop_s_stand [0:1 by=0.05]','species')))
  p$Region <- rg
  p
}))
region_pred$Region <- factor(region_pred$Region,levels=c('South','Centre','North'))
# Annotate each regional panel with the species-by-mixture interaction p-value
# and marginal/conditional Nakagawa R-squared from its fitted stand model.
regional_annotations <- dplyr::bind_rows(lapply(c('South','Centre','North'),function(rg){
  m <- models[[paste0('stand_',tolower(rg))]]
  ct <- as.data.frame(coef(summary(m)))
  interaction_term <- grep('species.*:prop_s_stand|prop_s_stand:species',
                           rownames(ct),value=TRUE)
  pcol <- grep('^Pr\\(',colnames(ct),value=TRUE)[1]
  pval <- if(length(interaction_term)==1 && !is.na(pcol))
    ct[interaction_term,pcol] else NA_real_
  r2 <- tryCatch(performance::r2_nakagawa(m),error=function(e) NULL)
  r2m <- if(!is.null(r2)) as.numeric(r2$R2_marginal) else NA_real_
  r2c <- if(!is.null(r2)) as.numeric(r2$R2_conditional) else NA_real_
  data.frame(Region=rg,x=0.03,y=Inf,
             label=sprintf('Species × stand proportion: p = %s\nMarginal R² = %s\nConditional R² = %s',
                           if(is.na(pval)) 'NA' else format.pval(pval,digits=3,eps=0.001),
                           if(is.na(r2m)) 'NA' else sprintf('%.3f',r2m),
                           if(is.na(r2c)) 'NA' else sprintf('%.3f',r2c)))
}))
regional_annotations$Region <- factor(regional_annotations$Region,
                                      levels=c('South','Centre','North'))
write.csv(regional_annotations,file.path(tab_dir,'regional_figure_statistics.csv'),row.names=FALSE)
figure2 <- ggplot(region_pred,aes(x=x,y=predicted,colour=group,fill=group))+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=.18,colour=NA)+
  geom_line(linewidth=1.2)+facet_wrap(~Region,scales='free_y',nrow=1)+
  geom_text(data=regional_annotations,
            aes(x=x,y=y,label=label),inherit.aes=FALSE,
            hjust=0,vjust=1.25,size=4.3,lineheight=1.15,colour='black')+
  scale_colour_manual(values=cols)+scale_fill_manual(values=cols)+
  scale_x_continuous(breaks=c(0,.5,1),labels=c('Pure\nDowny','Mixed','Pure\nSilver'))+
  labs(x='Stand-level silver birch proportion',y='Predicted browsing damage (%)',
       colour='Species',fill='Species')+
  theme_bw(base_size=15)+
  theme(strip.text=element_text(size=17,face='bold'),
        axis.text=element_text(size=13),axis.title=element_text(size=16),
        legend.text=element_text(size=14),legend.title=element_text(size=14))
print(figure2)
ggsave(file.path(fig_dir,'Figure_2_regional_birch_mixture.png'),figure2,width=17,height=6,dpi=300)


# ============================================================
# FIGURE 2b: NATIONAL BIRCH MIXTURE WITH P-VALUES AND R2
# ============================================================

national_keys <- c("stand_int", "stand_downy", "stand_silver")

if (all(national_keys %in% names(models))) {
  
  # Extract p-values from fitted mixed models
  get_p <- function(model, term) {
    tab <- coef(summary(model))
    pcol <- grep("^Pr\\(", colnames(tab), value = TRUE)
    
    if (term %in% rownames(tab) && length(pcol) > 0) {
      return(tab[term, pcol[1]])
    }
    
    # Approximate Wald p-value if not provided
    if (!term %in% rownames(tab)) return(NA_real_)
    
    z <- tab[term, "Estimate"] / tab[term, "Std. Error"]
    2 * pnorm(abs(z), lower.tail = FALSE)
  }
  
  fmt_p <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return("<0.001")
    sprintf("%.3f", p)
  }
  
  get_r2 <- function(model) {
    r <- tryCatch(
      performance::r2_nakagawa(model),
      error = function(e) NULL
    )
    
    if (is.null(r)) return("R²m = NA; R²c = NA")
    
    sprintf(
      "R²m = %.3f; R²c = %.3f",
      r$R2_marginal,
      r$R2_conditional
    )
  }
  
  m_int <- models[["stand_int"]]
  m_downy <- models[["stand_downy"]]
  m_silver <- models[["stand_silver"]]
  
  # Panel A: National interaction model
  interaction_term <- grep(
    "species.*:prop_s_stand|prop_s_stand:species",
    names(lme4::fixef(m_int)),
    value = TRUE
  )
  
  p_interaction <- if (length(interaction_term) == 1) {
    get_p(m_int, interaction_term)
  } else {
    NA_real_
  }
  
  label_A <- paste0(
    "Downy proportion slope: p = ",
    fmt_p(get_p(m_int, "prop_s_stand")),
    "\nSpecies × proportion: p = ",
    fmt_p(p_interaction),
    "\n", get_r2(m_int)
  )
  
  # Panel B: Separate national species models
  label_B <- paste0(
    "Downy: p = ",
    fmt_p(get_p(m_downy, "prop_s_stand")),
    "\n", get_r2(m_downy),
    "\nSilver: p = ",
    fmt_p(get_p(m_silver, "prop_s_stand")),
    "\n", get_r2(m_silver)
  )
  
  # Generate predictions
  combined <- as.data.frame(
    ggeffects::ggpredict(
      m_int,
      terms = c("prop_s_stand [0:1 by=0.05]", "species")
    )
  ) %>%
    dplyr::mutate(
      Panel = "A. National interaction model",
      species = as.character(group)
    )
  
  separate <- dplyr::bind_rows(
    lapply(c("Downy", "Silver"), function(sp) {
      m <- models[[paste0("stand_", tolower(sp))]]
      
      as.data.frame(
        ggeffects::ggpredict(
          m,
          terms = "prop_s_stand [0:1 by=0.05]"
        )
      ) %>%
        dplyr::mutate(species = sp)
    })
  ) %>%
    dplyr::mutate(
      Panel = "B. Species-specific national models"
    )
  
  national_predictions <- dplyr::bind_rows(combined, separate)
  
  # Statistical annotation positions
  national_annotations <- data.frame(
    Panel = c(
      "A. National interaction model",
      "B. Species-specific national models"
    ),
    x = c(0.03, 0.03),
    y = c(48, 48),
    label = c(label_A, label_B)
  )
  
  # Draw the figure
  figure_national <- ggplot(
    national_predictions,
    aes(
      x = x,
      y = predicted,
      colour = species,
      fill = species
    )
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.17,
      colour = NA
    ) +
    geom_line(linewidth = 1.15) +
    facet_wrap(~Panel, nrow = 1) +
    geom_text(
      data = national_annotations,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 3.4,
      lineheight = 1.15
    ) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c("Pure\nDowny", "Mixed", "Pure\nSilver")
    ) +
    scale_y_continuous(
      breaks = seq(0, 50, 10),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(ylim = c(0, 50)) +
    labs(
      x = "Stand-level silver birch proportion",
      y = "Predicted browsing damage (%)",
      colour = "Species",
      fill = "Species"
    ) +
    theme_bw(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(figure_national)
  
  ggsave(
    file.path(
      fig_dir,
      "Figure_2b_national_birch_mixture.png"
    ),
    plot = figure_national,
    width = 14,
    height = 7,
    dpi = 300
  )
  
  write.csv(
    national_predictions,
    file.path(tab_dir, "national_mixture_predictions.csv"),
    row.names = FALSE
  )
}


# Figure 3: region x species slopes for three cervid pellet categories.
# This is an ADDITIONAL model, separate from the main total-cervid model.
# Missing corrected density values are NOT silently changed to zero.
pellet_cols <- c(
  "moose_density_log",
  "red_deer_density_log",
  "small_deer_density_log"
)

if (all(pellet_cols %in% names(source_plots))) {
  
  pellet_stand <- source_plots %>%
    dplyr::group_by(year, area, stand_id) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(pellet_cols),
        ~ mean_keep_na(as.numeric(.x))
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      moose_log = moose_density_log,
      red_log = red_deer_density_log,
      small_log = small_deer_density_log
    )
  deer_data <- stand_data %>% dplyr::left_join(
    dplyr::select(pellet_stand,year,area,stand_id,moose_log,red_log,small_log),by=c('year','area','stand_id')) %>%
    dplyr::filter(dplyr::if_all(c(moose_log,red_log,small_log),~!is.na(.x))) %>% droplevels()
  if(nrow(deer_data)>100) {
    deer_model <- lmerTest::lmer(
      damage_N ~ species*region*(moose_log+red_log+small_log)+
        species*(prop_s_stand+total_birch_c+height_c)+pct+year+(1|area/stand_id),
      data=deer_data,REML=TRUE)
    all_stand_models$additional_regional_deer <- deer_model
    stand_results_table <- dplyr::bind_rows(stand_results_table,
                                            coef_table(deer_model,'additional_regional_deer'))
    write.csv(stand_results_table,file.path(tab_dir,'ALL_stand_model_coefficients.csv'),row.names=FALSE)
    deer_slopes <- dplyr::bind_rows(lapply(c('moose_log','red_log','small_log'),function(v){
      # Confidence-limit column names depend on emmeans version and df method.
      z <- as.data.frame(confint(emmeans::emtrends(deer_model, ~ species | region, var = v), level = 0.95))
      lo <- intersect(c('lower.CL', 'asymp.LCL', 'lower.HPD'), names(z))
      hi <- intersect(c('upper.CL', 'asymp.UCL', 'upper.HPD'), names(z))
      if (length(lo) == 0L || length(hi) == 0L)
        stop('Cannot find confidence limits for ', v, ': ', paste(names(z), collapse = ', '))
      z$CI_lower <- z[[lo[1]]]
      z$CI_upper <- z[[hi[1]]]
      z$Cervid <- c(moose_log='Moose',red_log='Red deer',small_log='Small deer')[[v]]
      z$Slope <- z[[paste0(v,'.trend')]]
      z
    }))
    write.csv(deer_slopes,file.path(tab_dir,'regional_deer_slopes.csv'),row.names=FALSE)
    # Horizontal forest plot: intervals and zero effect are easier to read.
    # Each cervid has its own x scale: compare direction and uncertainty,
    # NOT numerical magnitudes across panels by apparent line length.
    deer_slopes$region <- factor(deer_slopes$region,
                                 levels=c('South','Centre','North'))
    deer_slopes$Cervid <- factor(deer_slopes$Cervid,
                                 levels=c('Moose','Red deer','Small deer'))
    figure3 <- ggplot(deer_slopes,aes(y=region,x=Slope,colour=species))+
      geom_vline(xintercept=0,linetype='dashed',colour='grey55')+
      geom_errorbar(aes(xmin=CI_lower,xmax=CI_upper),orientation='y',
                    position=position_dodge(width=.5),width=.15,linewidth=.7)+
      geom_point(position=position_dodge(width=.5),size=2.8)+
      facet_wrap(~Cervid,scales='free_x',nrow=1)+
      scale_colour_manual(values=cols)+
      labs(title='Cervid density and birch browsing damage',
           subtitle='Species-specific slopes with 95% confidence intervals',
           x='Estimated change in browsing damage per unit log(1 + density)',
           y='Region',colour='Birch species',
           caption='Panel axes differ; compare numerical estimates rather than visual lengths across cervid species.')+
      theme_bw(base_size=13)+
      theme(strip.text=element_text(face='bold'),
            legend.position='bottom',plot.caption=element_text(hjust=0))
    print(figure3)
    ggsave(file.path(fig_dir,'Figure_3_regional_deer_slopes.png'),figure3,
           width=15,height=6,dpi=300)
    
  }
}

# Figure 4: pine browsing damage vs birch mixture.
# Pine response must be checked before interpretation; this uses the available
# 'proportion_pine_damage_winter' as recorded in the compact dataset.
if('proportion_pine_damage_winter' %in% names(source_plots)) {
  pine_data <- source_plots %>% dplyr::group_by(year,area,stand_id) %>%
    dplyr::summarise(pine_damage=if(all(is.na(proportion_pine_damage_winter))) NA_real_
                     else mean(as.numeric(proportion_pine_damage_winter),na.rm=TRUE),
                     prop_s_stand=if(all(is.na(total_birch))) NA_real_ else
                       sum(coalesce(as.numeric(silver_total),0),na.rm=TRUE)/sum(total_birch,na.rm=TRUE),
                     .groups='drop') %>%
    dplyr::filter(is.finite(pine_damage),is.finite(prop_s_stand),
                  prop_s_stand>=0,prop_s_stand<=1) %>% droplevels()
  if(nrow(pine_data)>30 && dplyr::n_distinct(pine_data$area)>1) {
    pine_model <- lmerTest::lmer(pine_damage~prop_s_stand+year+(1|area),
                                 data=pine_data,REML=TRUE)
    # Stand-aggregated response: one row per stand. A stand-within-area
    # random intercept cannot be estimated with one observation per stand.
    # (1|area) is the identifiable random effect for this dataset.
    if (lme4::isSingular(pine_model)) warning('Pine model is singular: inspect area random effects.')
    pine_tab <- coef_table(pine_model,'additional_pine_mixture')
    write.csv(pine_tab,file.path(tab_dir,'pine_mixture_model.csv'),row.names=FALSE)
    # Annotate the fixed-effect test for silver birch proportion, adjusted for
    # year and area. This is NOT a test of the prediction line itself.
    pine_coef <- as.data.frame(coef(summary(pine_model)))
    pine_p_col <- grep('^Pr\\(', names(pine_coef), value=TRUE)
    if (length(pine_p_col) == 0 || !'prop_s_stand' %in% rownames(pine_coef)) {
      stop('P-value for prop_s_stand unavailable; check lmerTest model summary.')
    }
    pine_p <- pine_coef['prop_s_stand', pine_p_col[1]]
    pine_p_label <- if (is.na(pine_p)) 'p = NA' else if (pine_p < 0.001) {
      'p < 0.001'
    } else sprintf('p = %.3f', pine_p)
    # Marginal and conditional Nakagawa R2; retain NA if unavailable.
    pine_r2 <- tryCatch(performance::r2_nakagawa(pine_model),
                        error=function(e) NULL)
    r2_m <- if(is.null(pine_r2)) NA_real_ else as.numeric(pine_r2$R2_marginal)
    r2_c <- if(is.null(pine_r2)) NA_real_ else as.numeric(pine_r2$R2_conditional)
    r2_fmt <- function(x) if(is.finite(x)) sprintf('%.3f',x) else 'NA'
    pine_stats <- paste0(pine_p_label,
                         '   |   Marginal R² = ',r2_fmt(r2_m),
                         '   |   Conditional R² = ',r2_fmt(r2_c))
    pine_pred <- as.data.frame(ggeffects::ggpredict(pine_model,terms='prop_s_stand [0:1 by=0.02]'))
    figure4 <- ggplot(pine_pred,aes(x,predicted))+
      geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill='#627FB2',alpha=.17)+
      geom_line(linewidth=1,colour='#627FB2')+
      scale_x_continuous(breaks=c(0,.5,1),labels=c('Pure\nDowny','Mixed','Pure\nSilver'))+
      labs(title='Overall effect of birch mixture on pine browsing',
           subtitle=pine_stats,
           x='Birch mixture composition',y='Predicted pine browsing damage',
           caption='P-value: silver birch proportion fixed effect; R²: Nakagawa marginal and conditional.')+
      theme_classic(base_size=14)+
      theme(plot.title=element_text(size=16,face='bold'),
            plot.subtitle=element_text(size=13,face='bold',colour='black',margin=margin(b=10)),
            plot.caption=element_text(size=9,colour='grey25',hjust=0),
            plot.margin=margin(15,20,15,15))
    print(figure4)
    ggsave(file.path(fig_dir,'Figure_4_pine_browsing_mixture.png'),figure4,
           width=10,height=6.5,dpi=300)
  }
}

# Easy access to everything from RStudio, no need to browse CSVs.
if (interactive()) {
  View(stand_results_table)
  View(stand_model_overview)
  View(stand_descriptives)
}
message('Main stand figures: ',fig_dir)
message('Accessible stand model tables: ',tab_dir)
message('Plot models remain in the original script as supplementary analyses.')


###############################################################################
# 5. PUBLICATION-READY WORD TABLES — RUN AFTER ALL MODELS HAVE FINISHED
###############################################################################
# Installs (once): install.packages(c("officer", "flextable"))
# Exports an editable .docx with stand main, species-specific, regional,
# supplementary plot models, and ML comparisons. No fabricated estimates.
for (pkg in c("officer", "flextable")) {
  if (!requireNamespace(pkg, quietly=TRUE)) {
    stop("Install the Word-table packages first: install.packages(c('officer','flextable'))")
  }
}
word_dir <- file.path(output_dir, "tables", "word")
dir.create(word_dir, recursive=TRUE, showWarnings=FALSE)

fmt_num <- function(x, digits=2) {
  ifelse(is.na(x), "\u2014", formatC(x, digits=digits, format="f"))
}
fmt_p <- function(x) {
  ifelse(is.na(x), "\u2014",
         ifelse(x < 0.001, "<0.001", formatC(x, digits=3, format="f")))
}
make_coef <- function(m) {
  z <- as.data.frame(coef(summary(m)))
  term <- rownames(z)
  p_col <- grep("^Pr\\(", names(z), value=TRUE)
  # lmerTest supplies p-values for lmer models. No p-value invented if absent.
  pv <- if(length(p_col)) z[[p_col[1]]] else rep(NA_real_,nrow(z))
  out <- data.frame(
    Predictor=term,
    Estimate=fmt_num(z[["Estimate"]]),
    SE=fmt_num(z[["Std. Error"]]),
    t_or_z=fmt_num(z[[if("t value" %in% names(z)) "t value" else
      if("z value" %in% names(z)) "z value" else
        names(z)[3]]]),
    p=fmt_p(pv),
    check.names=FALSE
  )
  names(out)[4] <- if("t value" %in% names(z)) "t" else if("z value" %in% names(z)) "z" else "Statistic"
  out
}
model_titles <- c(
  stand_add="Main stand model — additive",
  stand_int="Main stand model — species interactions",
  stand_downy="Downy birch — stand model",
  stand_silver="Silver birch — stand model",
  stand_south="South — stand model",
  stand_centre="Centre — stand model",
  stand_north="North — stand model",
  stand_region_interaction="All regions — stand interaction model",
  plot_add="Supplementary plot model — additive",
  plot_int="Supplementary plot model — species interactions",
  plot_downy="Supplementary downy birch — plot model",
  plot_silver="Supplementary silver birch — plot model",
  plot_south="Supplementary South — plot model",
  plot_centre="Supplementary Centre — plot model",
  plot_north="Supplementary North — plot model",
  plot_region_interaction="Supplementary all-regions plot interaction",
  plot_response_stand_add="Plot responses with stand covariates — additive",
  plot_response_stand_int="Plot responses with stand covariates — interaction",
  plot_response_stand_downy="Downy plot responses with stand covariates",
  plot_response_stand_silver="Silver plot responses with stand covariates",
  plot_response_stand_south="South plot responses with stand covariates",
  plot_response_stand_centre="Centre plot responses with stand covariates",
  plot_response_stand_north="North plot responses with stand covariates"
)
make_ft <- function(df) {
  ft <- flextable::flextable(df)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bold(ft, part="header")
  ft <- flextable::fontsize(ft, size=9, part="all")
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, layout="autofit")
  ft
}
doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Birch browsing — statistical model tables",
                             style="heading 1")
doc <- officer::body_add_par(doc,
                             "Estimates are generated from the fitted models in this R session. Review variable labels, model diagnostics and journal requirements before submission.",
                             style="Normal")
table_index <- 0
for (nm in names(model_titles)) {
  if (!nm %in% names(models)) next
  table_index <- table_index + 1
  m <- models[[nm]]
  tab <- make_coef(m)
  # CSV is useful for later copyediting and independent verification.
  write.csv(tab, file.path(word_dir,paste0(nm,"_formatted.csv")),row.names=FALSE)
  doc <- officer::body_add_par(doc,
                               paste0("Table ",table_index,". ",model_titles[[nm]]),style="heading 2")
  doc <- flextable::body_add_flextable(doc,make_ft(tab))
  doc <- officer::body_add_par(doc,
                               paste0("N = ",stats::nobs(m),
                                      "; fixed-effect estimates and standard errors; ",
                                      if(inherits(m,"merMod")) "linear mixed-effects model fitted with REML." else "linear model.",
                                      " The t column is the model's test statistic. P-values are from lmerTest where available."),
                               style="Normal")
}
# ML likelihood-ratio comparisons; explicitly distinguish from REML estimates.
comparisons <- list(
  "Stand: additive vs interaction"=comparison_stand,
  "Plot: additive vs interaction"=comparison_plot,
  "Plot response with stand covariates: additive vs interaction"=
    comparison_plot_response_stand
)
for (nm in names(comparisons)) {
  x <- as.data.frame(comparisons[[nm]])
  x$Model <- rownames(x)
  rownames(x) <- NULL
  x <- x[,c("Model",setdiff(names(x),"Model")),drop=FALSE]
  for (k in names(x)) {
    if(is.numeric(x[[k]])) {
      x[[k]] <- if(grepl("Pr\\(",k)) fmt_p(x[[k]]) else fmt_num(x[[k]])
    }
  }
  table_index <- table_index+1
  doc <- officer::body_add_par(doc,
                               paste0("Table ",table_index,". ",nm," (maximum likelihood comparison)"),
                               style="heading 2")
  doc <- flextable::body_add_flextable(doc,make_ft(x))
  doc <- officer::body_add_par(doc,
                               "Models were refitted with ML for likelihood-ratio testing. This comparison uses the same observations and random-effects structure within each pair.",
                               style="Normal")
}
word_path <- file.path(word_dir,"birch_browsing_manuscript_model_tables.docx")
print(doc,target=word_path)
cat("\nEditable Word model tables saved to:\n",word_path,"\n")

# ============================================================
# EXPORT MODEL SUMMARIES
# ============================================================

summary_file <- file.path(
  tab_dir,
  "ALL_model_summaries.txt"
)

sink(summary_file)

for (model_name in names(models)) {
  
  cat("\n\n")
  cat("====================================================\n")
  cat("MODEL:", model_name, "\n")
  cat("====================================================\n\n")
  
  model <- models[[model_name]]
  
  if (inherits(model, c("lmerMod", "lm", "glmmTMB"))) {
    print(summary(model))
  }
}

sink()

cat("\nModel summaries saved to:\n", summary_file, "\n")

summary(
  models$stand_int,
  ddf = "Satterthwaite"
)

summary(models$stand_downy)

summary(models$stand_silver)

class(models$stand_int)
class(models$stand_downy)
class(models$stand_silver)

performance::r2_nakagawa(models$stand_int)
performance::r2_nakagawa(models$stand_downy)
performance::r2_nakagawa(models$stand_silver)


# ============================================================
# AREA-CLUSTERED CR2 SENSITIVITY: MAIN STAND MODEL
# ============================================================
if (requireNamespace("clubSandwich", quietly=TRUE)) {
  cr2 <- clubSandwich::coef_test(
    models$stand_int, vcov="CR2",
    cluster=model.frame(models$stand_int)$area,
    test="Satterthwaite")
  print(cr2)
  write.csv(as.data.frame(cr2),
            file.path(output_dir,"tables","stand_int_TOTAL_birch_CR2.csv"),
            row.names=FALSE)
} else {
  message("Optional: install.packages('clubSandwich') for area-clustered CR2 inference")
}

# Install only if necessary
install.packages("emmeans")
install.packages("pbkrtest")
library(emmeans)
library(pbkrtest)
# Calculate adjusted means
emm_species <- emmeans(
  models$stand_int,
  ~ species
)

# Show adjusted means
summary(emm_species)

# Test difference between species
pairs(emm_species)

# Show 95% confidence intervals
confint(pairs(emm_species))
summary(emmeans::ref_grid(models$stand_int))



library(ggplot2)
library(dplyr)
library(ggeffects)

# Existing regional stand models
regional_models <- list(
  South  = models$stand_south,
  Centre = models$stand_centre,
  North  = models$stand_north
)

# Check height is included in each model
lapply(regional_models, formula)

# Generate predictions from fitted models
height_predictions <- lapply(
  names(regional_models),
  function(reg) {
    
    pred <- ggpredict(
      regional_models[[reg]],
      terms = c("height_c [all]", "species"),
      type = "fixed"
    )
    
    as.data.frame(pred) %>%
      mutate(region = reg)
  }
) %>%
  bind_rows() %>%
  mutate(
    region = factor(
      region,
      levels = c("South", "Centre", "North")
    )
  )

# Create three-panel figure
p_height <- ggplot(
  height_predictions,
  aes(
    x = x,
    y = predicted,
    colour = group,
    group = group
  )
) +
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    fill = "grey65",
    alpha = 0.25,
    colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~region, nrow = 1) +
  scale_colour_manual(
    values = c(
      "Downy" = "forestgreen",
      "Silver" = "royalblue"
    )
  ) +
  labs(
    x = "Centred birch height (m)",
    y = "Predicted browsing damage (%)",
    colour = "Species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = 13,
      face = "bold"
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

print(p_height)

# Save publication-quality figure
ggsave(
  "Regional_height_browsing.png",
  plot = p_height,
  width = 11,
  height = 4,
  dpi = 300
)

library(ggplot2)
library(dplyr)
library(performance)

# Existing predictions from regional models
height_plot <- height_predictions %>%
  mutate(
    region = factor(
      region,
      levels = c("South", "Centre", "North")
    ),
    group = factor(
      group,
      levels = c("Downy", "Silver")
    )
  )

# Calculate R-squared for each regional model
model_stats <- lapply(
  names(regional_models),
  function(reg) {
    
    m <- regional_models[[reg]]
    r2 <- performance::r2_nakagawa(m)
    
    # Test the species x height interaction
    coefs <- summary(m)$coefficients
    interaction_row <- grep(
      "speciesSilver:height_c|height_c:speciesSilver",
      rownames(coefs)
    )
    
    p <- if (length(interaction_row) == 1 &&
             "Pr(>|t|)" %in% colnames(coefs)) {
      coefs[interaction_row, "Pr(>|t|)"]
    } else {
      NA_real_
    }
    
    p_label <- if (is.na(p)) {
      "NA"
    } else if (p < 0.001) {
      "<0.001"
    } else {
      sprintf("%.3f", p)
    }
    
    data.frame(
      region = reg,
      label = paste0(
        "Species × height: p = ", p_label,
        "\nMarginal R² = ",
        sprintf("%.3f", r2$R2_marginal),
        "\nConditional R² = ",
        sprintf("%.3f", r2$R2_conditional)
      )
    )
  }
) %>%
  bind_rows() %>%
  mutate(
    region = factor(
      region,
      levels = c("South", "Centre", "North")
    )
  )

# Create figure matching original regional figure
p_height <- ggplot(
  height_plot,
  aes(
    x = x,
    y = predicted,
    colour = group,
    fill = group
  )
) +
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 0.85) +
  
  facet_wrap(
    ~region,
    nrow = 1,
    scales = "free_y"
  ) +
  
  geom_text(
    data = model_stats,
    aes(
      x = -Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.08,
    vjust = 1.3,
    size = 3
  ) +
  
  scale_colour_manual(
    values = c(
      "Downy" = "#40AD65",
      "Silver" = "#4B77BA"
    ),
    name = "Species"
  ) +
  scale_fill_manual(
    values = c(
      "Downy" = "#40AD65",
      "Silver" = "#4B77BA"
    ),
    name = "Species"
  ) +
  
  labs(
    x = "Centred birch height (m)",
    y = "Predicted browsing damage (%)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(
      fill = "grey85",
      colour = "grey50"
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    panel.grid.major = element_line(
      colour = "grey90",
      linewidth = 0.3
    ),
    panel.grid.minor = element_line(
      colour = "grey95",
      linewidth = 0.2
    ),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.spacing = unit(0.8, "lines")
  )

print(p_height)

ggsave(
  "Regional_height_browsing_matching.png",
  p_height,
  width = 12,
  height = 5,
  dpi = 300
)


library(emmeans)
library(dplyr)

regional_models <- list(
  South  = models$stand_south,
  Centre = models$stand_centre,
  North  = models$stand_north
)

# Estimate species-specific height slopes
height_slopes <- lapply(
  names(regional_models),
  function(reg) {
    
    m <- regional_models[[reg]]
    
    trends <- emtrends(
      m,
      ~ species,
      var = "height_c",
      lmer.df = "kenward-roger"
    )
    
    cat("\n\n==========", reg, "==========\n")
    
    # Slopes for each species
    print(summary(trends, infer = c(TRUE, TRUE)))
    
    # Test whether slopes differ
    print(
      summary(
        pairs(trends),
        infer = c(TRUE, TRUE)
      )
    )
    
    return(trends)
  }
)

names(height_slopes) <- names(regional_models)
