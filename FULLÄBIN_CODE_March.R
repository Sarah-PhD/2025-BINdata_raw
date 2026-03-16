##############################################################################
# ÄBIN 2008–2025 MASTER PIPELINE
# Full combined version
# Keeps the older advantages:
#   - safer year-aware pine harmonisation
#   - explicit handling of 2025 recalculated pine variables
# Adds the newer advantages:
#   - cleaner final names
#   - duplicate-name merge after cleaning
#   - full export + compact export
##############################################################################

#=============================================================================
# 0) PACKAGES
#=============================================================================

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(purrr)

#=============================================================================
# 1) HELPERS
#=============================================================================

to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

rowSums_keepNA_df <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

rowSums_keepNA <- function(m) {
  m_num <- as.data.frame(lapply(m, function(x) as.numeric(as.character(x))))
  rs <- rowSums(m_num, na.rm = TRUE)
  all_na <- rowSums(!is.na(m_num)) == 0
  rs[all_na] <- NA_real_
  rs
}

clean_names_basic <- function(x) {
  x %>%
    stringr::str_replace_all("\\.+", "_") %>%
    stringr::str_replace_all("\\s+", "_") %>%
    stringr::str_replace_all("[\\?/\\-]+", "_") %>%
    stringr::str_replace_all("[()]", "") %>%
    stringr::str_replace_all("__+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

severity_to_num <- function(x) {
  dplyr::recode(
    x,
    "≤10"    = 5,
    "11_25"  = 18,
    "26_50"  = 38,
    "51_75"  = 63,
    "76_100" = 88,
    .default = NA_real_
  )
}

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
  
  dplyr::bind_cols(
    df[, -cols_to_collapse, drop = FALSE],
    combined_df
  )
}

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
  
  stem_col   <- paste0(prefix, "_stems")
  winter_n   <- paste0(prefix, "_winter_damage_stems")
  winter_p   <- paste0(prefix, "_winter_damage_prop")
  summer_n   <- paste0(prefix, "_summer_damage_stems")
  summer_p   <- paste0(prefix, "_summer_damage_prop")
  rebrows_n  <- paste0(prefix, "_rebrowsed_stems")
  rebrows_p  <- paste0(prefix, "_rebrowsing_prop")
  
  df %>%
    mutate(across(all_of(unique(c(sum_cols, winter_cols, summer_cols, rebrows_cols))), to_num)) %>%
    mutate(
      !!stem_col := rowSums_keepNA_df(select(., all_of(sum_cols))),
      !!winter_n := if (length(winter_cols) > 0) rowSums_keepNA_df(select(., all_of(winter_cols))) else NA_real_,
      !!winter_p := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[winter_n]] / .data[[stem_col]]
      ),
      !!summer_n := if (length(summer_cols) > 0) rowSums_keepNA_df(select(., all_of(summer_cols))) else NA_real_,
      !!summer_p := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[summer_n]] / .data[[stem_col]]
      ),
      !!rebrows_n := if (length(rebrows_cols) > 0) rowSums_keepNA_df(select(., all_of(rebrows_cols))) else NA_real_,
      !!rebrows_p := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[rebrows_n]] / .data[[stem_col]]
      )
    )
}

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

rename_if_present <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    names(df)[names(df) == old] <- new
  }
  df
}

#=============================================================================
# 2) PATHS
#=============================================================================

base_path_2025 <- "//storage-ume.slu.se/home$/shge0002/My Documents/ÄBIN Data/Raw ÄBIN data 2025/ÄBIN2025rawCVS"
historical_path <- "//storage-ume.slu.se/home$/shge0002/Desktop/ABIN files/2008-2024 ÄBIN1024.xlsx"

#=============================================================================
# 3) BUILD 2025 FROM RAW CSV FILES
#=============================================================================

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

pine_summary <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_damage_events_2025 = n(),
    pine_severity_sum_2025 = sum(severity_num, na.rm = TRUE),
    pine_severity_mean_2025 = mean(severity_num, na.rm = TRUE),
    pine_n_trees_2025 = n(),
    pine_severe_n_2025 = sum(severity_num >= 38, na.rm = TRUE),
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
    contorta_severe_n_2025 = sum(severity_num >= 38, na.rm = TRUE),
    contorta_stems_lthh_2025 = sum(`Contorta Stems < HH`, na.rm = TRUE),
    contorta_browsed_lthh_2025 = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

plots_full <- plots_full %>%
  left_join(contorta_damage_wide, by = "plot_id") %>%
  left_join(contorta_summary, by = "plot_id")

#------------------------------------------------------------------------------
# 3F) Merge 2025
#------------------------------------------------------------------------------
abin2025_raw <- plots_full %>%
  left_join(stands, by = "stand_id")

#=============================================================================
# 4) CLEAN HISTORICAL 2008–2024 FILE
#=============================================================================

abin_hist <- read_excel(historical_path, sheet = "Raw")

cn <- names(abin_hist)

cols_damage <- 31:58
cn[cols_damage] <- vapply(cn[cols_damage], rename_damage_name, FUN.VALUE = character(1))
cn[13] <- "ub"

idx <- grep("^(Spruce|Contorta)\\b", cn)
cn[idx] <- vapply(cn[idx], rename_conifer_damage, FUN.VALUE = character(1))

names(abin_hist) <- cn

# Drop column 133 only if it exists
if (ncol(abin_hist) >= 133) {
  abin_hist <- abin_hist[, -133]
}

abin_hist <- abin_hist %>%
  rename(
    stand = Stand_number,
    surveyor = Surveyer
  ) %>%
  mutate(
    Date = as.Date(Date, origin = "1899-12-30")
  )

#=============================================================================
# 5) CLEAN 2025 TO MATCH HISTORICAL STRUCTURE
#=============================================================================

abin2025 <- abin2025_raw %>%
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

code_cols <- which(str_detect(names(abin2025), "^(spruce_|contorta_|ub|fts|fb|fs|ps|ss|od|o|[a-z]+,[a-z,]+)"))
if (length(code_cols) > 0) {
  abin2025 <- collapse_combo_columns(abin2025, code_cols)
}

abin2025 <- abin2025 %>%
  mutate(
    Year = 2025,
    Date = as.Date(Date)
  ) %>%
  rename(
    stand = Stand,
    surveyor = Surveyor
  )

names(abin_hist) <- clean_names_basic(names(abin_hist))
names(abin2025)  <- clean_names_basic(names(abin2025))

#=============================================================================
# 6) BIND HISTORICAL + 2025
#=============================================================================

abin_all <- bind_rows(abin_hist, abin2025)

#=============================================================================
# 7) STANDARDISE KEY VARIABLES
#=============================================================================

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

abin_all[] <- lapply(abin_all, function(x) {
  if (is.logical(x)) as.numeric(x) else x
})

#=============================================================================
# 8) PREFIX PINE DAMAGE COMBO COLUMNS
#=============================================================================

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

#=============================================================================
# 9) UNIFY PELLET COLUMNS
#=============================================================================

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

#=============================================================================
# 10) COMPUTE PINE / SPRUCE / CONTORTA STEMS AND BROWSING PROPORTIONS
#=============================================================================

abin_all <- add_damage_props(
  abin_all,
  prefix = "pine",
  codes = c("fts", "o", "od", "fb", "fs", "ps", "ss"),
  ub_col = "pine_unbrowsed"
)

abin_all <- add_damage_props(
  abin_all,
  prefix = "spruce",
  codes = c("fts", "o", "od", "fb", "fs", "ps"),
  ub_col = "spruce_ub"
)

abin_all <- add_damage_props(
  abin_all,
  prefix = "contorta",
  codes = c("fts", "o", "od", "fb", "fs", "ps", "ss"),
  ub_col = "contorta_ub"
)

#=============================================================================
# 11) YEAR-AWARE HARMONISE FINAL PINE COLUMNS
# Recompute 2025 pine values directly from pine combo columns
# older years keep historical values
#=============================================================================

# Which year column exists?
if ("year" %in% names(abin_all)) {
  year_col <- "year"
} else if ("Year" %in% names(abin_all)) {
  year_col <- "Year"
} else {
  stop("No year column found in abin_all")
}

abin_all[[year_col]] <- to_num(abin_all[[year_col]])

# Historical columns if present
hist_pine_stems_col <- if ("Pine_stems" %in% names(abin_all)) "Pine_stems" else NULL
hist_pine_winter_n_col <- if ("Pine_Winter_Damage_Stems" %in% names(abin_all)) "Pine_Winter_Damage_Stems" else NULL
hist_pine_winter_p_col <- if ("Proportion_Pine_Damage_Winter" %in% names(abin_all)) "Proportion_Pine_Damage_Winter" else NULL

for (nm in c(hist_pine_stems_col, hist_pine_winter_n_col, hist_pine_winter_p_col)) {
  if (!is.null(nm)) abin_all[[nm]] <- to_num(abin_all[[nm]])
}

# Identify pine combo columns again
pine_codes <- c("fts", "o", "od", "fb", "fs", "ps", "ss")

pine_combo_regex <- paste0(
  "^pine_(",
  paste(pine_codes, collapse = "|"),
  ")(,(",
  paste(pine_codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(abin_all)[str_detect(names(abin_all), pine_combo_regex)]

if (length(pine_combo_cols) == 0) {
  stop("No pine combo columns found for recomputing 2025 pine values.")
}

# Total pine stems = all pine combo columns + pine_unbrowsed
pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(abin_all))
if (length(pine_sum_cols) == 0) {
  stop("No pine stem columns found for recomputing 2025 pine values.")
}

# Winter browsing = any combo containing fts OR fb OR fs
winter_tokens <- c("fts", "fb", "fs")
winter_regex  <- paste0("(^pine_|,)(?:", paste(winter_tokens, collapse = "|"), ")(,|$)")
winter_cols   <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]

if (length(winter_cols) == 0) {
  stop("No winter browsing pine columns found for recomputing 2025 pine values.")
}

# Recompute directly from source columns
abin_all <- abin_all %>%
  mutate(across(all_of(unique(c(pine_sum_cols, winter_cols))), to_num)) %>%
  mutate(
    pine_stems_2025calc = rowSums_keepNA_df(select(., all_of(pine_sum_cols))),
    pine_winter_damage_stems_2025calc = rowSums_keepNA_df(select(., all_of(winter_cols))),
    proportion_pine_damage_winter_2025calc = if_else(
      is.na(pine_stems_2025calc) | pine_stems_2025calc == 0,
      NA_real_,
      pine_winter_damage_stems_2025calc / pine_stems_2025calc
    )
  )

# Final year-aware columns
abin_all <- abin_all %>%
  mutate(
    pine_stems = if_else(
      .data[[year_col]] == 2025,
      pine_stems_2025calc,
      if (!is.null(hist_pine_stems_col)) to_num(.data[[hist_pine_stems_col]]) else pine_stems_2025calc
    ),
    pine_winter_damage_stems = if_else(
      .data[[year_col]] == 2025,
      pine_winter_damage_stems_2025calc,
      if (!is.null(hist_pine_winter_n_col)) to_num(.data[[hist_pine_winter_n_col]]) else pine_winter_damage_stems_2025calc
    ),
    proportion_pine_damage_winter = if_else(
      .data[[year_col]] == 2025,
      proportion_pine_damage_winter_2025calc,
      if (!is.null(hist_pine_winter_p_col)) to_num(.data[[hist_pine_winter_p_col]]) else proportion_pine_damage_winter_2025calc
    )
  )
#=============================================================================
# 11B) STANDARDISE PINE WINTER PROPORTION TO 0-1 SCALE
#=============================================================================

abin_all <- abin_all %>%
  mutate(
    proportion_pine_damage_winter = if_else(
      !is.na(proportion_pine_damage_winter) & proportion_pine_damage_winter > 1,
      proportion_pine_damage_winter / 100,
      proportion_pine_damage_winter
    ),

    proportion_pine_damage_summer = if_else(
      !is.na(proportion_pine_damage_summer) & proportion_pine_damage_summer > 1,
      proportion_pine_damage_summer / 100,
      proportion_pine_damage_summer
    ),
    proportion_pine_damage_yearly = if_else(
      !is.na(proportion_pine_damage_yearly) & proportion_pine_damage_yearly > 1,
      proportion_pine_damage_yearly / 100,
      proportion_pine_damage_yearly
    )
  )
summary(abin_all$proportion_pine_damage_winter)

summary(abin_all$proportion_pine_damage_winter[abin_all[[year_col]] == 2025])
summary(abin_all$proportion_pine_damage_winter[abin_all[[year_col]] < 2025])
#=============================================================================
# 12) FINAL NAME CLEAN
#=============================================================================

names(abin_all) <- clean_names_basic(names(abin_all))
names(abin_all) <- tolower(names(abin_all))

#=============================================================================
# 13) MERGE DUPLICATE COLUMN NAMES CREATED BY NAME CLEANING
#=============================================================================

abin_all <- coalesce_all_duplicate_names(abin_all)

dup_after <- unique(names(abin_all)[duplicated(names(abin_all))])
if (length(dup_after) == 0) {
  message("All duplicate column names successfully merged.")
} else {
  message("These duplicate names still remain:")
  print(dup_after)
}

#=============================================================================
# 14) STANDARDISE FINAL CORE COLUMN NAMES
#=============================================================================

# Pine
abin_all <- rename_if_present(abin_all, "pine_stem_calc", "pine_stems_calc")
abin_all <- rename_if_present(abin_all, "pine_summer_damage_prop", "proportion_pine_damage_summer")
abin_all <- rename_if_present(abin_all, "pine_rebrowsing_prop", "pine_rebrowsing_prop")

# Spruce
abin_all <- rename_if_present(abin_all, "spruce_winter_damage_prop", "spruce_winter_damage_prop")
abin_all <- rename_if_present(abin_all, "spruce_summer_damage_prop", "spruce_summer_damage_prop")
abin_all <- rename_if_present(abin_all, "spruce_rebrowsing_prop", "spruce_rebrowsing_prop")

# Contorta
abin_all <- rename_if_present(abin_all, "contorta_winter_damage_prop", "contorta_winter_damage_prop")
abin_all <- rename_if_present(abin_all, "contorta_summer_damage_prop", "contorta_summer_damage_prop")
abin_all <- rename_if_present(abin_all, "contorta_rebrowsing_prop", "contorta_rebrowsing_prop")

#=============================================================================
# 15) OPTIONAL CLEANUP OF TEMPORARY COLUMNS
#=============================================================================

drop_temp <- intersect(
  c(
    "pine_stems_calc",
    "pine_winter_browsed_stems_calc",
    "winter_browsing_calc",
    "half_height_raw"
  ),
  names(abin_all)
)

if (length(drop_temp) > 0) {
  abin_all <- abin_all %>% select(-all_of(drop_temp))
}

#=============================================================================
# 16) OPTIONAL CHECKS
#=============================================================================

if (all(c("stand", "year") %in% names(abin_all))) {
  df_ma20 <- abin_all %>% filter(stand == "MA20")
  print(nrow(df_ma20))
  print(unique(df_ma20$year))
}

#=============================================================================
# 17) CREATE COMPACT VERSION
#=============================================================================

# Final save after harmonisation
write_csv(abin_all, "abin2008_2025_full.csv")
write_xlsx(abin_all, "abin2008_2025_full.xlsx")

# Compact version
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
#=============================================================================
# 18) EXPORT
#=============================================================================

write_csv(abin_all, "abin2008_2025_full.csv")
write_xlsx(abin_all, "abin2008_2025_full.xlsx")

write_csv(abin_compact, "abin_compact.csv")
write_xlsx(abin_compact, "abin_compact.xlsx")

cat("\nDone.\n")
cat("Saved:\n")
cat(" - abin2008_2025_full.csv\n")
cat(" - abin2008_2025_full.xlsx\n")
cat(" - abin_compact.csv\n")
cat(" - abin_compact.xlsx\n")

summary(abin_all$pine_stems)
summary(abin_all$pine_winter_damage_stems)
summary(abin_all$proportion_pine_damage_winter)

sum(!is.na(abin_all$pine_stems[abin_all[[year_col]] == 2025]))
sum(!is.na(abin_all$pine_winter_damage_stems[abin_all[[year_col]] == 2025]))
sum(!is.na(abin_all$proportion_pine_damage_winter[abin_all[[year_col]] == 2025]))

