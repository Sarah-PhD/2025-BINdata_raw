###############################################################################
# ÄBIN 2008–2025 MASTER PIPELINE
# Cleaned and updated
# Author: Sarah Gore
# Date: 2026-03-10
###############################################################################

#==============================================================================
# 0) PACKAGES
#==============================================================================

library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)

#==============================================================================
# 1) HELPERS
#==============================================================================

#----- safe numeric conversion -------------------------------------------------
to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

#----- rowSums but keep NA if all contributing cells are NA --------------------
rowSums_keepNA <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

#----- standardise names to snake_case -----------------------------------------
clean_names_basic <- function(x) {
  x %>%
    str_replace_all("\\.+", "_") %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("[\\?/\\-]+", "_") %>%
    str_replace_all("[()]", "") %>%
    str_replace_all("__+", "_") %>%
    str_replace_all("^_|_$", "")
}

#----- damage label recoder ----------------------------------------------------
severity_to_num <- function(x) {
  recode(
    x,
    "≤10"    = 5,
    "11_25"  = 18,
    "26_50"  = 38,
    "51_75"  = 63,
    "76_100" = 88,
    .default = NA_real_
  )
}

#----- collapse duplicate combo columns safely ---------------------------------
collapse_combo_columns <- function(df, cols_to_collapse) {
  if (length(cols_to_collapse) == 0) return(df)
  
  orig_names <- names(df)[cols_to_collapse]
  
  norm_names <- vapply(
    strsplit(orig_names, ","),
    function(parts) {
      parts <- trimws(parts)
      parts <- sort(parts)
      paste(parts, collapse = ",")
    },
    FUN.VALUE = character(1)
  )
  
  groups <- split(cols_to_collapse, norm_names)
  
  combined_mat <- sapply(groups, function(idx) {
    rowSums_keepNA(df[, idx, drop = FALSE])
  })
  
  combined_df <- as.data.frame(combined_mat)
  colnames(combined_df) <- names(groups)
  
  bind_cols(
    df[, -cols_to_collapse, drop = FALSE],
    combined_df
  )
}

#----- add browsing/stem/proportion columns from combo columns -----------------
add_damage_props <- function(df, prefix, codes, ub_col,
                             winter_tokens = c("fts", "fb", "fs"),
                             summer_tokens = c("ps"),
                             rebrows_tokens = c("fts", "fb", "fs", "ss", "ps", "od")) {
  
  combo_regex <- paste0(
    "^", prefix, "_(",
    paste(codes, collapse = "|"),
    ")(,(",
    paste(codes, collapse = "|"),
    "))*$"
  )
  
  combo_cols <- names(df)[str_detect(names(df), combo_regex)]
  
  if (length(combo_cols) == 0) {
    message("No combo columns found for ", prefix, ". Skipping.")
    return(df)
  }
  
  sum_cols <- intersect(c(combo_cols, ub_col), names(df))
  if (length(sum_cols) == 0) {
    message("No total stem columns found for ", prefix, ". Skipping.")
    return(df)
  }
  
  token_regex <- function(tokens) {
    paste0("(^", prefix, "_|,)(?:", paste(tokens, collapse = "|"), ")(,|$)")
  }
  
  winter_tokens_use  <- intersect(winter_tokens, codes)
  summer_tokens_use  <- intersect(summer_tokens, codes)
  rebrows_tokens_use <- intersect(rebrows_tokens, codes)
  
  winter_cols  <- combo_cols[str_detect(combo_cols, token_regex(winter_tokens_use))]
  summer_cols  <- combo_cols[str_detect(combo_cols, token_regex(summer_tokens_use))]
  rebrows_cols <- combo_cols[str_detect(combo_cols, token_regex(rebrows_tokens_use))]
  
  stem_col <- paste0(prefix, "_stems")
  winter_n <- paste0(prefix, "_winter_damage_stems")
  winter_p <- paste0(prefix, "_winter_damage_prop")
  summer_n <- paste0(prefix, "_summer_damage_stems")
  summer_p <- paste0(prefix, "_summer_damage_prop")
  rebrows_n <- paste0(prefix, "_rebrowsed_stems")
  rebrows_p <- paste0(prefix, "_rebrowsing_prop")
  
  df %>%
    mutate(across(all_of(unique(c(sum_cols, winter_cols, summer_cols, rebrows_cols))), to_num)) %>%
    mutate(
      !!stem_col := rowSums_keepNA(select(., all_of(sum_cols))),
      !!winter_n := if (length(winter_cols) > 0) rowSums_keepNA(select(., all_of(winter_cols))) else NA_real_,
      !!winter_p := if_else(is.na(.data[[stem_col]]) | .data[[stem_col]] == 0, NA_real_, .data[[winter_n]] / .data[[stem_col]]),
      !!summer_n := if (length(summer_cols) > 0) rowSums_keepNA(select(., all_of(summer_cols))) else NA_real_,
      !!summer_p := if_else(is.na(.data[[stem_col]]) | .data[[stem_col]] == 0, NA_real_, .data[[summer_n]] / .data[[stem_col]]),
      !!rebrows_n := if (length(rebrows_cols) > 0) rowSums_keepNA(select(., all_of(rebrows_cols))) else NA_real_,
      !!rebrows_p := if_else(is.na(.data[[stem_col]]) | .data[[stem_col]] == 0, NA_real_, .data[[rebrows_n]] / .data[[stem_col]])
    )
}

#==============================================================================
# 2) PATHS
#==============================================================================

base_path_2025 <- "//storage-ume.slu.se/home$/shge0002/My Documents/ÄBIN Data/Raw ÄBIN data 2025/ÄBIN2025rawCVS"
historical_path <- "//storage-ume.slu.se/home$/shge0002/Desktop/ABIN files/2008-2024 ÄBIN1024.xlsx"

#==============================================================================
# 3) BUILD 2025 FROM RAW CSV FILES
#==============================================================================

#------------------------------------------------------------------------------
# 3A) Stands
#------------------------------------------------------------------------------
stands <- read_csv(file.path(base_path_2025, "ÄBIN Survey 2025_0.csv"), show_col_types = FALSE) %>%
  rename(stand_id = GlobalID)

#------------------------------------------------------------------------------
# 3B) Plots
#------------------------------------------------------------------------------
plots <- read_csv(file.path(base_path_2025, "ABIN_2025_wp_repeat_1.csv"), show_col_types = FALSE) %>%
  rename(
    plot_id = GlobalID,
    stand_id = ParentGlobalID
  )

#------------------------------------------------------------------------------
# 3C) Pine
#------------------------------------------------------------------------------
pine <- read_csv(file.path(base_path_2025, "ABIN_2025_Pines_2.csv"), show_col_types = FALSE) %>%
  rename(
    plot_id = ParentGlobalID,
    damage = `Select damage type`,
    severity = `how much relative damage`
  ) %>%
  mutate(
    severity_num = severity_to_num(severity)
  )

pine_damage_wide <- pine %>%
  count(plot_id, damage, name = "damage_events") %>%
  pivot_wider(
    names_from = damage,
    values_from = damage_events,
    values_fill = NA
  )

pine_damage_wide[is.na(pine_damage_wide)] <- NA

pine_summary <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_damage_events_2025 = n(),
    pine_severity_sum_2025 = sum(severity_num, na.rm = TRUE),
    pine_severity_mean_2025 = mean(severity_num, na.rm = TRUE),
    pine_n_trees_2025 = n(),
    pine_severe_n_2025 = sum(severity_num >= 38, na.rm = TRUE), # 26–50% or higher
    pine_stems_lthh_2025 = sum(`Pine Stems < HH`, na.rm = TRUE),
    pine_browsed_lthh_2025 = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

plots_full <- plots %>%
  left_join(pine_damage_wide, by = "plot_id") %>%
  left_join(pine_summary, by = "plot_id")

#------------------------------------------------------------------------------
# 3D) Spruce
#------------------------------------------------------------------------------
spruce <- read_csv(file.path(base_path_2025, "ABIN_2025_Spruce_3.csv"), show_col_types = FALSE) %>%
  rename(
    plot_id = ParentGlobalID,
    damage = `Select damage type`
  )

spruce_damage_wide <- spruce %>%
  count(plot_id, damage, name = "damage_events") %>%
  pivot_wider(
    names_from = damage,
    values_from = damage_events,
    values_fill = NA
  ) %>%
  rename_with(~ paste0("spruce_", .x), -plot_id) %>%
  mutate(
    total_spruce_2025 = rowSums_keepNA(select(., -plot_id))
  )

plots_full <- plots_full %>%
  left_join(spruce_damage_wide, by = "plot_id")

#------------------------------------------------------------------------------
# 3E) Contorta
#------------------------------------------------------------------------------
contorta <- read_csv(file.path(base_path_2025, "ABIN_2025_Contorta_4.csv"), show_col_types = FALSE) %>%
  rename(
    plot_id = ParentGlobalID,
    damage = `Select damage type`,
    severity = `how much relative damage`
  ) %>%
  mutate(
    severity_num = severity_to_num(severity)
  )

contorta_damage_wide <- contorta %>%
  count(plot_id, damage, name = "damage_events") %>%
  pivot_wider(
    names_from = damage,
    values_from = damage_events,
    values_fill = NA
  ) %>%
  rename_with(~ paste0("contorta_", .x), -plot_id) %>%
  mutate(
    total_contorta_2025 = rowSums_keepNA(select(., -plot_id))
  )

contorta_summary <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_damage_events_2025 = n(),
    contorta_severity_sum_2025 = sum(severity_num, na.rm = TRUE),
    contorta_severity_mean_2025 = mean(severity_num, na.rm = TRUE),
    contorta_n_trees_2025 = n(),
    contorta_severe_n_2025 = sum(severity_num >= 38, na.rm = TRUE), # 26–50% or higher
    contorta_stems_lthh_2025 = sum(`Contorta Stems < HH`, na.rm = TRUE),
    contorta_browsed_lthh_2025 = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

plots_full <- plots_full %>%
  left_join(contorta_damage_wide, by = "plot_id") %>%
  left_join(contorta_summary, by = "plot_id")

abin_hist <- abin_hist[, -133]


#------------------------------------------------------------------------------
# 3F) Merge 2025
#------------------------------------------------------------------------------
abin2025_raw <- plots_full %>%
  left_join(stands, by = "stand_id")

write_xlsx(abin2025_raw, "abin2025_raw.xlsx")

#==============================================================================
# 4) CLEAN HISTORICAL 2008–2024 FILE
#==============================================================================

abin_hist <- read_excel(historical_path, sheet = "Raw")

rename_damage_name <- function(nm) {
  if (grepl("Undamaged", nm, ignore.case = TRUE)) return("ub")
  
  nm_clean <- sub("^Pine\\s+", "", nm)
  parts <- strsplit(nm_clean, "\\s*\\+\\s*")[[1]]
  parts <- trimws(parts)
  
  codes <- vapply(parts, function(p) {
    if (p %in% c("Bark")) return("fb")
    if (p %in% c("Stem")) return("fs")
    if (p %in% c("Winter Top Shoot", "WTS")) return("fts")
    if (p %in% c("Other")) return("o")
    if (p %in% c("Old")) return("od")
    if (p %in% c("Summer Top Shoot", "STS")) return("ps")
    if (p %in% c("SS")) return("ss")
    if (p %in% c("Undamaged")) return("ub")
    p
  }, FUN.VALUE = character(1))
  
  codes <- sort(unique(codes))
  paste(codes, collapse = ",")
}

rename_conifer_damage <- function(nm) {
  if (grepl("^Spruce\\b", nm)) {
    species <- "spruce"
    nm_clean <- sub("^Spruce\\s+", "", nm)
  } else if (grepl("^Contorta\\b", nm)) {
    species <- "contorta"
    nm_clean <- sub("^Contorta\\s+", "", nm)
  } else {
    return(nm)
  }
  
  parts <- unlist(strsplit(nm_clean, "\\s*\\+\\s*"))
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  
  codes <- vapply(parts, function(p) {
    p_low <- tolower(p)
    
    if (p_low %in% c("undamaged")) return("ub")
    
    if (species == "contorta") {
      if (p_low %in% c("fresh ts only", "fresh ts")) return("fts")
      if (p_low %in% c("presummer ts")) return("ps")
      if (p_low %in% c("fresh side shoot browsing", "side shoot browsing")) return("ss")
      if (p_low %in% c("fresh bark damage", "bark damage")) return("fb")
      if (p_low %in% c("fresh stem breakage", "stem breakage")) return("fs")
      if (p_low %in% c("other fresh damage")) return("o")
      if (p_low %in% c("old damage only", "old other damage", "old damage", "old")) return("od")
    }
    
    if (species == "spruce") {
      if (p_low %in% c("winter ts", "wts")) return("fts")
      if (p_low %in% c("winter stem", "ws")) return("fs")
      if (p_low %in% c("winter bark", "wb")) return("fb")
      if (p_low %in% c("old")) return("od")
    }
    
    p
  }, FUN.VALUE = character(1))
  
  codes <- sort(unique(codes))
  paste0(species, "_", paste(codes, collapse = ","))
}

cn <- names(abin_hist)

# Pine damage columns
cols_damage <- 31:58
cn[cols_damage] <- vapply(cn[cols_damage], rename_damage_name, FUN.VALUE = character(1))
cn[13] <- "ub"

# Spruce / Contorta
idx <- grep("^(Spruce|Contorta)\\b", cn)
cn[idx] <- vapply(cn[idx], rename_conifer_damage, FUN.VALUE = character(1))

names(abin_hist) <- cn
abin_hist <- abin_hist[, -133]

abin_hist <- abin_hist %>%
  rename(
    stand = Stand_number,
    surveyor = Surveyer
  ) %>%
  mutate(
    Date = as.Date(Date, origin = "1899-12-30")
  )

write_xlsx(abin_hist, "abin2008_2024_clean.xlsx")

#==============================================================================
# 5) CLEAN 2025 TO MATCH HISTORICAL STRUCTURE
#==============================================================================

abin2025 <- read_excel("abin2025_raw.xlsx")

abin2025 <- abin2025 %>%
  mutate(
    Area = as.character(Area),
    Area = recode(
      Area,
      om = "OsterMalma",
      at = "Åtvidaberg",
      vx = "Växjö",
      bk = "Barksätter",
      fk = "Fredrika",
      fu = "Furudal",
      lf = "Lofsdalen",
      lj = "Ljusdal",
      ly = "Lycksele",
      ma = "Malå",
      no = "Nordmaling",
      ra = "Råneå",
      so = "Sorsele"
    )
  ) %>%
  rename(
    plot = `Sampling plot 1-10`,
    pct = `Pre-commercially thinned?`,
    half_height = `Half height (m)`,
    north = `x.x`,
    east = `y.x`,
    nearest_tract = `Closest tract`,
    productivity = `Site productivity`,
    downy_height = `Downy Birch Highest Height (m)`,
    downy_total = `Downy Birch Stems`,
    downy_damage = `Downy birch damage (whole plot)`,
    silver_height = `Silver Birch Highest Height (m)`,
    silver_total = `Silver Birch Stems`,
    silver_damage = `Silver Birch damage (whole plot)`,
    rowan_height = `Rowan Highest Height (m)`,
    rowan_total = `Rowan Stems`,
    rowan_damage = `Rowan damage (whole plot)`,
    oak_height = `Oak Highest Height (m)`,
    oak_total = `Oak Stems`,
    oak_damage = `Oak Damage (whole plot)`,
    salix_height = `Salix Highest Height (m)`,
    salix_total = `Salix Stems`,
    salix_damage = `Salix Damage (whole plot)`,
    aspen_height = `Aspen Highest Height (m)`,
    aspen_total = `Aspen Stems`,
    aspen_damage = `Aspen Damage (whole plot)`
  )

# Remove metadata columns if present
drop_cols <- c(
  "ObjectID.x", "ObjectID.y", "plot_id",
  "CreationDate.x", "CreationDate.y",
  "Creator.x", "Creator.y",
  "EditDate.x", "EditDate.y",
  "Editor.x", "Editor.y",
  "stand_id",
  "Tallest tree 1 (m)",
  "Tallest tree 2 (m)"
)

drop_cols <- intersect(drop_cols, names(abin2025))
if (length(drop_cols) > 0) {
  abin2025 <- abin2025 %>% select(-all_of(drop_cols))
}

#------------------------------------------------------------------------------
# Collapse duplicate comma-code columns safely
#------------------------------------------------------------------------------
code_cols <- which(str_detect(names(abin2025), "^(spruce_|contorta_|ub|fts|fb|fs|ps|ss|od|o|[a-z]+,[a-z,]+)"))

if (length(code_cols) > 0) {
  abin2025 <- collapse_combo_columns(abin2025, code_cols)
}

#------------------------------------------------------------------------------
# Add year/date and match historical naming
#------------------------------------------------------------------------------
abin2025 <- abin2025 %>%
  mutate(
    Year = 2025,
    Date = as.Date(Date)
  )

# Rename 2025 core cols to historical equivalents
abin2025 <- abin2025 %>%
  rename(
    stand = Stand,
    surveyor = Surveyor
  )

# Make historical names cleaner too
names(abin_hist) <- clean_names_basic(names(abin_hist))
names(abin2025)  <- clean_names_basic(names(abin2025))

#==============================================================================
# 6) BIND HISTORICAL + 2025
#==============================================================================

abin_all <- bind_rows(abin_hist, abin2025)

#==============================================================================
# 7) STANDARDISE KEY VARIABLES
#==============================================================================

# Half height in cm
if ("half_height" %in% names(abin_all)) {
  abin_all <- abin_all %>%
    mutate(
      half_height_raw = to_num(half_height),
      half_height_cm = case_when(
        is.na(half_height_raw) ~ NA_real_,
        half_height_raw < 7 ~ half_height_raw * 100,
        TRUE ~ half_height_raw
      )
    )
}

# Convert logicals to numeric
abin_all[] <- lapply(abin_all, function(x) {
  if (is.logical(x)) as.numeric(x) else x
})

#==============================================================================
# 8) PREFIX PINE DAMAGE COMBO COLUMNS
#==============================================================================

damage_codes <- c("fts", "o", "od", "fb", "fs", "ps", "ss", "ub")

damage_combo_regex <- paste0(
  "^(",
  paste(damage_codes, collapse = "|"),
  ")(,(",
  paste(damage_codes, collapse = "|"),
  "))*$"
)

cols_to_prefix <- names(abin_all) %>%
  keep(~ str_detect(.x, damage_combo_regex)) %>%
  discard(~ str_detect(.x, "^[A-Za-z]+_")) %>%
  discard(~ .x == "NA")

if ("ub" %in% cols_to_prefix) {
  abin_all <- abin_all %>%
    rename(pine_unbrowsed = ub)
  cols_to_prefix <- setdiff(cols_to_prefix, "ub")
}

if (length(cols_to_prefix) > 0) {
  abin_all <- abin_all %>%
    rename_with(~ paste0("pine_", .x), all_of(cols_to_prefix))
}

#==============================================================================
# 9) UNIFY PELLET COLUMNS
#==============================================================================

moose_cols     <- intersect(c("moose_pellets", "moose_piles"), names(abin_all))
reddeer_cols   <- intersect(c("red_deer_pellets", "red_deer_piles"), names(abin_all))
smalldeer_cols <- intersect(c("small_deer_pellets", "small_deer_piles"), names(abin_all))
reindeer_cols  <- intersect(c("reindeer", "reindeer_piles", "reindeer_pellets"), names(abin_all))
wildboar_cols  <- intersect(c("wild_boar"), names(abin_all))

abin_all <- abin_all %>%
  mutate(
    moose_pellets = if (length(moose_cols) > 0) coalesce(!!!syms(moose_cols)) else NA_real_,
    red_deer_pellets = if (length(reddeer_cols) > 0) coalesce(!!!syms(reddeer_cols)) else NA_real_,
    small_deer_pellets = if (length(smalldeer_cols) > 0) coalesce(!!!syms(smalldeer_cols)) else NA_real_,
    reindeer_pellets = if (length(reindeer_cols) > 0) coalesce(!!!syms(reindeer_cols)) else NA_real_,
    wild_boar = if (length(wildboar_cols) > 0) coalesce(!!!syms(wildboar_cols)) else NA_real_
  ) %>%
  mutate(
    across(
      c(moose_pellets, red_deer_pellets, small_deer_pellets, reindeer_pellets, wild_boar),
      to_num
    )
  )

#==============================================================================
# 10) COMPUTE PINE / SPRUCE / CONTORTA STEMS AND BROWSING PROPORTIONS
#==============================================================================

# Pine
abin_all <- add_damage_props(
  abin_all,
  prefix = "pine",
  codes = c("fts", "o", "od", "fb", "fs", "ps", "ss"),
  ub_col = "pine_unbrowsed"
)

# Spruce
abin_all <- add_damage_props(
  abin_all,
  prefix = "spruce",
  codes = c("fts", "o", "od", "fb", "fs", "ps"),
  ub_col = "spruce_ub"
)

# Contorta
abin_all <- add_damage_props(
  abin_all,
  prefix = "contorta",
  codes = c("fts", "o", "od", "fb", "fs", "ps", "ss"),
  ub_col = "contorta_ub"
)

#==============================================================================
# 11) HARMONISE 2025-ONLY DERIVED COLUMNS INTO STANDARD COLUMNS
#==============================================================================

# Where historical columns already exist, fill their NAs from the new derived cols.
# This is the safe updated step. Keep this. Do not use the old ifelse() blocks.

abin_all <- abin_all %>%
  mutate(
    pine_stems = coalesce(pine_stems, Pine_stems, Pine_stem),
    pine_winter_damage_stems = coalesce(pine_winter_damage_stems, Pine_Winter_Damage_Stems, pine_winter_browsed_stems),
    proportion_pine_damage_winter = coalesce(proportion_pine_damage_winter, Proportion_Pine_Damage_Winter, pine_winter_damage_prop),
    
    pine_summer_damage_stems = coalesce(pine_summer_damage_stems, Pine_Summer_Damage_Stems, pine_summer_damage_stems),
    proportion_pine_damage_summer = coalesce(proportion_pine_damage_summer, Proportion_Pine_Damage_Summer, pine_summer_damage_prop),
    
    pine_yearly_damage_stems = coalesce(pine_yearly_damage_stems, Pine_Yearly_Damage_Stems),
    proportion_pine_damage_yearly = coalesce(proportion_pine_damage_yearly, Proportion_Pine_Damage_Yearly)
  )

# Optional cleanup of temporary duplicates if present
drop_temp <- intersect(
  c(
    "Pine_stems",
    "pine_winter_browsed_stems", "Pine_Winter_Damage_Stems",
    "winter_browsing", "pine_winter_damage_prop",
    "pine_summer_browsed_stems", "pine_summer_damage_prop",
    "rebrowsing", "pine_rebrowsing_prop"
  ),
  names(abin_all)
)

if (length(drop_temp) > 0) {
  abin_all <- abin_all %>% select(-all_of(drop_temp))
}

#==============================================================================
# 12) FINAL NAME CLEAN
#==============================================================================

names(abin_all) <- clean_names_basic(names(abin_all))
names(abin_all) <- tolower(names(abin_all))

#==============================================================================
# 12B) MERGE DUPLICATE COLUMN NAMES CREATED BY NAME CLEANING
#==============================================================================

library(dplyr)

coalesce_all_duplicate_names <- function(df) {
  dup_nms <- unique(names(df)[duplicated(names(df))])
  
  if (length(dup_nms) == 0) {
    message("No duplicate column names found.")
    return(df)
  }
  
  message("Merging duplicate column names:")
  print(dup_nms)
  
  for (nm in dup_nms) {
    idx <- which(names(df) == nm)
    cols <- df[, idx, drop = FALSE]
    merged <- Reduce(dplyr::coalesce, cols)
    df <- df[, -idx, drop = FALSE]
    df[[nm]] <- merged
  }
  
  df
}

abin_all <- coalesce_all_duplicate_names(abin_all)

dup_after <- unique(names(abin_all)[duplicated(names(abin_all))])

if (length(dup_after) == 0) {
  message("All duplicate column names successfully merged.")
} else {
  message("These duplicate names still remain:")
  print(dup_after)
}

#==============================================================================
# 13) OPTIONAL CHECKS
#==============================================================================

if (all(c("stand", "year") %in% names(abin_all))) {
  df_ma20 <- abin_all %>% filter(stand == "MA20")
  print(nrow(df_ma20))
  print(unique(df_ma20$year))
}
#==============================================================================
# 14) EXPORT MASTER DATASET
#==============================================================================

write_csv(abin_all, "abin2008_2025_full.csv")
write_xlsx(abin_all, "abin2008_2025_full.xlsx")

#==============================================================================
# 15) CREATE COMPACT VERSION
#==============================================================================

compact_keep <- intersect(
  c(
    "area", "year", "stand", "plot", "age", "surveyor", "date",
    "nearest_tract", "productivity", "north", "east", "pct",
    "half_height", "half_height_cm",
    
    "pine_unbrowsed",
    "pine_stems",
    "pine_winter_damage_stems",
    "proportion_pine_damage_winter",
    "pine_summer_damage_stems",
    "proportion_pine_damage_summer",
    "pine_rebrowsed_stems",
    "pine_rebrowsing_prop",
    
    "spruce_ub",
    "spruce_stems",
    "spruce_winter_damage_stems",
    "spruce_winter_damage_prop",
    "spruce_summer_damage_stems",
    "spruce_summer_damage_prop",
    "spruce_rebrowsed_stems",
    "spruce_rebrowsing_prop",
    
    "contorta_ub",
    "contorta_stems",
    "contorta_winter_damage_stems",
    "contorta_winter_damage_prop",
    "contorta_summer_damage_stems",
    "contorta_summer_damage_prop",
    "contorta_rebrowsed_stems",
    "contorta_rebrowsing_prop",
    
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
    "wild_boar"
  ),
  names(abin_all)
)

abin_compact <- abin_all %>%
  select(all_of(compact_keep))

write_csv(abin_compact, "abin_compact.csv")
write_xlsx(abin_compact, "abin_compact.xlsx")

