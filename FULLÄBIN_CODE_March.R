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
library(lubridate)
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
# Robust date parser for mixed ÄBIN formats
# Handles Excel dates, normal dates, and 2025 strings like "4/5/2025 2:25:00 PM"
parse_abin_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  
  # Excel-style numeric dates
  date_excel <- suppressWarnings(
    ifelse(
      !is.na(x_num) & x_num > 30000,
      as.character(as.Date(x_num, origin = "1899-12-30")),
      NA_character_
    )
  )
  
  # Text/date-time formats
  date_text <- suppressWarnings(
    as.character(as.Date(lubridate::parse_date_time(
      x_chr,
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "dmy HMS", "dmy HM", "dmy",
        "mdy HMS", "mdy HM", "mdy",
        "Ymd HMS", "Ymd HM", "Ymd",
        "dmY HMS", "dmY HM", "dmY",
        "mdY HMS", "mdY HM", "mdY",
        "mdy IMS p", "mdy IM p",
        "dmy IMS p", "dmy IM p"
      ),
      tz = "UTC"
    )))
  )
  
  out <- dplyr::coalesce(
    as.Date(date_excel),
    as.Date(date_text)
  )
  
  # Remove nonsense early dates
  out[lubridate::year(out) < 2000] <- NA
  
  out
}

# Safely coalesce across possible column names
coalesce_existing <- function(df, candidates) {
  nms_low <- tolower(names(df))
  idx <- match(tolower(candidates), nms_low)
  idx <- idx[!is.na(idx)]
  
  if (length(idx) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  Reduce(dplyr::coalesce, lapply(df[idx], to_num))
}

# Find the first column name that exists in the dataframe
first_existing <- function(candidates, df) {
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
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
    Date = parse_abin_date(Date)
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

##=============================================================================
# 7) STANDARDISE HEIGHT VARIABLES TO METERS
#=============================================================================

height_cols <- c("half_height", "downy_height", "silver_height")

abin_all <- abin_all %>%
  mutate(
    across(any_of(height_cols), to_num),
    across(
      any_of(height_cols),
      ~ case_when(
        is.na(.) ~ NA_real_,
        . > 8 ~ . / 100,   # cm → m
        TRUE ~ .
      )
    )
  )

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
# 9) PRESERVE RAW PELLET COUNTS EARLY
# Handles both older *_pellets and 2025 *_piles before final name cleaning
#=============================================================================

abin_all <- abin_all %>%
  mutate(
    moose_pellets_original = coalesce_existing(
      .,
      c("moose_pellets", "Moose_pellets", "moose_piles", "Moose_piles")
    ),
    
    red_deer_pellets_original = coalesce_existing(
      .,
      c("red_deer_pellets", "Red_deer_pellets", "red_deer_piles", "Red_deer_piles")
    ),
    
    small_deer_pellets_original = coalesce_existing(
      .,
      c("small_deer_pellets", "Small_deer_pellets", "small_deer_piles", "Small_deer_piles")
    ),
    
    reindeer_pellets_original = coalesce_existing(
      .,
      c("reindeer_pellets", "Reindeer_pellets", "reindeer_piles", "Reindeer_piles", "reindeer", "Reindeer")
    )
  )

defec_rate <- list(
  moose = 16.5,
  red_deer = 19,
  small_deer = 22
)

plot_area_m2 <- 38.48451
K_plot <- 1000000 / plot_area_m2

leaf_fall_doy <- function(lat) {
  lat_ref <- c(56, 58, 60, 62, 64, 66)
  doy_ref <- c(
    as.numeric(format(as.Date("2024-10-25"), "%j")),
    as.numeric(format(as.Date("2024-10-20"), "%j")),
    as.numeric(format(as.Date("2024-10-15"), "%j")),
    as.numeric(format(as.Date("2024-10-10"), "%j")),
    as.numeric(format(as.Date("2024-10-05"), "%j")),
    as.numeric(format(as.Date("2024-10-01"), "%j"))
  )
  
  approx(lat_ref, doy_ref, xout = lat, rule = 2)$y
}
#=============================================================================
# 10) COMPUTE PINE / SPRUCE / CONTORTA STEMS AND BROWSING PROPORTIONS
#=============================================================================

abin_all <- add_damage_props(
  abin_all,
  prefix = "pine",
  codes = c("fts", "o", "od", "fb", "fs", "ps", "ss"),
  ub_col = "pine_unbrowsed",
  winter_tokens = c("fts", "fb", "fs"),
  summer_tokens = c("ps"),
  rebrows_tokens = c("fts", "fb", "fs", "ps")
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
# Rebuild winter, summer, and yearly pine variables directly from pine combo cols
# 2025 and other detailed years use recomputed values
# older years fall back to historical columns
#=============================================================================

# Which year column exists?
year_col <- first_existing(c("year", "Year"), abin_all)
if (is.na(year_col)) stop("No year column found in abin_all")

abin_all[[year_col]] <- to_num(abin_all[[year_col]])

# Historical columns if present (accept either old or already-cleaned names)
hist_pine_stems_col <- first_existing(c("Pine_stems", "pine_stems"), abin_all)
hist_pine_winter_n_col <- first_existing(c("Pine_Winter_Damage_Stems", "pine_winter_damage_stems"), abin_all)
hist_pine_winter_p_col <- first_existing(c("Proportion_Pine_Damage_Winter", "proportion_pine_damage_winter"), abin_all)

hist_pine_summer_n_col <- first_existing(c("Pine_Summer_Damage_Stems", "pine_summer_damage_stems"), abin_all)
hist_pine_summer_p_col <- first_existing(c("Proportion_Pine_Damage_Summer", "proportion_pine_damage_summer"), abin_all)

hist_pine_yearly_n_col <- first_existing(c("Pine_Yearly_Damage_Stems", "pine_yearly_damage_stems"), abin_all)
hist_pine_yearly_p_col <- first_existing(c("Proportion_Pine_Damage_Yearly", "proportion_pine_damage_yearly"), abin_all)

for (nm in c(
  hist_pine_stems_col,
  hist_pine_winter_n_col, hist_pine_winter_p_col,
  hist_pine_summer_n_col, hist_pine_summer_p_col,
  hist_pine_yearly_n_col, hist_pine_yearly_p_col
)) {
  if (!is.na(nm) && nm %in% names(abin_all)) {
    abin_all[[nm]] <- to_num(abin_all[[nm]])
  }
}

# Identify pine combo columns
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
  stop("No pine combo columns found for recomputing pine values.")
}

# Total pine stems = all pine combo columns + pine_unbrowsed
pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(abin_all))
if (length(pine_sum_cols) == 0) {
  stop("No pine stem columns found for recomputing pine values.")
}

# Winter = any combo containing fts OR fb OR fs
winter_tokens <- c("fts", "fb", "fs")
winter_regex <- paste0("(^pine_|,)(?:", paste(winter_tokens, collapse = "|"), ")(,|$)")
winter_cols <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]

# Summer = any combo containing ps
summer_tokens <- c("ps")
summer_regex <- paste0("(^pine_|,)(?:", paste(summer_tokens, collapse = "|"), ")(,|$)")
summer_cols <- pine_combo_cols[str_detect(pine_combo_cols, summer_regex)]

# Yearly fresh = any combo containing fts OR fb OR fs OR ps
yearly_tokens <- c("fts", "fb", "fs", "ps")
yearly_regex <- paste0("(^pine_|,)(?:", paste(yearly_tokens, collapse = "|"), ")(,|$)")
yearly_cols <- pine_combo_cols[str_detect(pine_combo_cols, yearly_regex)]

if (length(winter_cols) == 0) {
  stop("No winter browsing pine columns found.")
}
if (length(summer_cols) == 0) {
  warning("No summer browsing pine columns found.")
}
if (length(yearly_cols) == 0) {
  stop("No yearly fresh-damage pine columns found.")
}

cat("\nWinter cols used:\n")
print(winter_cols)

cat("\nSummer cols used:\n")
print(summer_cols)

cat("\nYearly cols used:\n")
print(yearly_cols)

# Recompute directly from source columns
abin_all <- abin_all %>%
  mutate(across(all_of(unique(c(pine_sum_cols, winter_cols, summer_cols, yearly_cols))), to_num)) %>%
  mutate(
    pine_stems_calc = rowSums_keepNA_df(select(., all_of(pine_sum_cols))),
    pine_winter_damage_stems_calc = rowSums_keepNA_df(select(., all_of(winter_cols))),
    pine_summer_damage_stems_calc = if (length(summer_cols) > 0) {
      rowSums_keepNA_df(select(., all_of(summer_cols)))
    } else {
      NA_real_
    },
    pine_yearly_damage_stems_calc = pmax(
      rowSums_keepNA_df(select(., all_of(yearly_cols))),
      pine_winter_damage_stems_calc,
      na.rm = TRUE
    ),
    
    proportion_pine_damage_winter_calc = if_else(
      is.na(pine_stems_calc) | pine_stems_calc == 0,
      NA_real_,
      pine_winter_damage_stems_calc / pine_stems_calc
    ),
    proportion_pine_damage_summer_calc = if_else(
      is.na(pine_stems_calc) | pine_stems_calc == 0,
      NA_real_,
      pine_summer_damage_stems_calc / pine_stems_calc
    ),
    proportion_pine_damage_yearly_calc = if_else(
      is.na(pine_stems_calc) | pine_stems_calc == 0,
      NA_real_,
      pine_yearly_damage_stems_calc / pine_stems_calc
    )
  )

# Safe historical fallback vectors
hist_pine_stems_vec <- if (!is.na(hist_pine_stems_col) && hist_pine_stems_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_stems_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

hist_pine_winter_n_vec <- if (!is.na(hist_pine_winter_n_col) && hist_pine_winter_n_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_winter_n_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

hist_pine_winter_p_vec <- if (!is.na(hist_pine_winter_p_col) && hist_pine_winter_p_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_winter_p_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

hist_pine_summer_n_vec <- if (!is.na(hist_pine_summer_n_col) && hist_pine_summer_n_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_summer_n_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

hist_pine_summer_p_vec <- if (!is.na(hist_pine_summer_p_col) && hist_pine_summer_p_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_summer_p_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

hist_pine_yearly_n_vec <- if (!is.na(hist_pine_yearly_n_col) && hist_pine_yearly_n_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_yearly_n_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

hist_pine_yearly_p_vec <- if (!is.na(hist_pine_yearly_p_col) && hist_pine_yearly_p_col %in% names(abin_all)) {
  to_num(abin_all[[hist_pine_yearly_p_col]])
} else {
  rep(NA_real_, nrow(abin_all))
}

# Use calculated values when detailed pine coding exists;
# otherwise fall back to historical columns
abin_all <- abin_all %>%
  mutate(
    has_detailed_pine = rowSums(!is.na(select(., any_of(pine_combo_cols)))) > 0
  ) %>%
  mutate(
    pine_stems = case_when(
      has_detailed_pine ~ pine_stems_calc,
      !is.na(hist_pine_stems_vec) ~ hist_pine_stems_vec,
      TRUE ~ pine_stems_calc
    ),
    
    pine_winter_damage_stems = case_when(
      has_detailed_pine ~ pine_winter_damage_stems_calc,
      !is.na(hist_pine_winter_n_vec) ~ hist_pine_winter_n_vec,
      TRUE ~ pine_winter_damage_stems_calc
    ),
    
    pine_summer_damage_stems = case_when(
      has_detailed_pine ~ pine_summer_damage_stems_calc,
      !is.na(hist_pine_summer_n_vec) ~ hist_pine_summer_n_vec,
      TRUE ~ NA_real_
    ),
    
    pine_yearly_damage_stems = case_when(
      has_detailed_pine ~ pmax(pine_yearly_damage_stems_calc, pine_winter_damage_stems, na.rm = TRUE),
      !is.na(hist_pine_yearly_n_vec) ~ pmax(hist_pine_yearly_n_vec, pine_winter_damage_stems, na.rm = TRUE),
      !is.na(pine_winter_damage_stems) ~ pine_winter_damage_stems,
      TRUE ~ NA_real_
    ),
    
    proportion_pine_damage_winter = case_when(
      has_detailed_pine ~ proportion_pine_damage_winter_calc,
      !is.na(hist_pine_winter_p_vec) ~ hist_pine_winter_p_vec,
      TRUE ~ proportion_pine_damage_winter_calc
    ),
    
    proportion_pine_damage_summer = case_when(
      has_detailed_pine ~ proportion_pine_damage_summer_calc,
      !is.na(hist_pine_summer_p_vec) ~ hist_pine_summer_p_vec,
      TRUE ~ NA_real_
    ),
    
    proportion_pine_damage_yearly = case_when(
      !is.na(pine_stems) & pine_stems > 0 & !is.na(pine_yearly_damage_stems) ~
        pine_yearly_damage_stems / pine_stems,
      !is.na(hist_pine_yearly_p_vec) ~ hist_pine_yearly_p_vec,
      TRUE ~ NA_real_
    )
  )
#=============================================================================
# 11B) STANDARDISE FINAL PINE PROPORTIONS TO 0-1 SCALE
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
  ) %>%
  mutate(
    proportion_pine_damage_winter = if_else(
      proportion_pine_damage_winter > 1, NA_real_, proportion_pine_damage_winter
    ),
    proportion_pine_damage_summer = if_else(
      proportion_pine_damage_summer > 1, NA_real_, proportion_pine_damage_summer
    ),
    proportion_pine_damage_yearly = if_else(
      proportion_pine_damage_yearly > 1, NA_real_, proportion_pine_damage_yearly
    )
  )

summary(abin_all$proportion_pine_damage_winter)
summary(abin_all$proportion_pine_damage_summer)
summary(abin_all$proportion_pine_damage_yearly)

summary(abin_all$proportion_pine_damage_winter[abin_all[[year_col]] == 2025])
summary(abin_all$proportion_pine_damage_winter[abin_all[[year_col]] < 2025])
#=============================================================================
# 12) FINAL NAME CLEAN
#=============================================================================

names(abin_all) <- clean_names_basic(names(abin_all))
names(abin_all) <- tolower(names(abin_all))


#=============================================================================
# 12B) FINAL PELLET REPAIR AFTER NAME CLEANING
#=============================================================================

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
#=============================================================================
# 13B) FINAL PELLET STANDARDISATION AFTER NAME CLEANING + DUPLICATE MERGE
#=============================================================================

area_lat <- tibble::tribble(
  ~area,                 ~lat_mean,            ~lon_mean,
  "Fredrika",            64.2056,              18.7331,
  "Furudal",             61.4302,              15.1002,
  "Lofsdalen",           62.08458141582506,    NA_real_,
  "Ljusdal",             61.9583,              15.3818,
  "Lycksele",            64.6845,              18.1589,
  "Nordmaling",          63.6590,              19.6754,
  "Råneå",               66.2720,              21.3994,
  "Sorsele",             65.6100,              17.5919,
  "Växjö",               57.1893,              14.6869,
  "Åtvidaberg",          58.3171,              15.7491,
  "OsterMalma",          58.7000,              17.0912,
  "Barksätter",          58.97757317897449,    NA_real_,
  "Malå",                65.20166947906844,    NA_real_
)

pellet_years <- c(2022, 2023, 2024, 2025)

abin_all <- abin_all %>%
  mutate(
    area = as.character(area),
    year = to_num(year),
    date = parse_abin_date(date),
    
    area = recode(
      area,
      "om" = "OsterMalma",
      "at" = "Åtvidaberg",
      "vx" = "Växjö",
      "bk" = "Barksätter",
      "fk" = "Fredrika",
      "fu" = "Furudal",
      "lf" = "Lofsdalen",
      "lj" = "Ljusdal",
      "ly" = "Lycksele",
      "ma" = "Malå",
      "no" = "Nordmaling",
      "ra" = "Råneå",
      "so" = "Sorsele",
      "Öster Malma" = "OsterMalma",
      "ÖsterMalma" = "OsterMalma",
      "Oster Malma" = "OsterMalma",
      "Raaneaa" = "Råneå",
      "Raanea" = "Råneå",
      "Ranea" = "Råneå",
      "Lyksele" = "Lycksele",
      "Växjo" = "Växjö",
      "Fågelåsen" = "Lofsdalen",
      "Furudal_Kontroll" = "Furudal",
      "Sorsele_Kontroll" = "Sorsele",
      "Råneå_Kontroll" = "Råneå",
      .default = area
    )
  ) %>%
  select(-any_of(c("lat_mean", "lon_mean"))) %>%
  left_join(area_lat, by = "area") %>%
  mutate(
    #--------------------------------------------------
    # 1) Coalesce raw pellet columns from all possible names
    #--------------------------------------------------
    moose_pellets_original = coalesce_existing(
      .,
      c("moose_pellets_original", "moose_pellets", "moose_piles")
    ),
    
    red_deer_pellets_original = coalesce_existing(
      .,
      c("red_deer_pellets_original", "red_deer_pellets", "red_deer_piles")
    ),
    
    small_deer_pellets_original = coalesce_existing(
      .,
      c("small_deer_pellets_original", "small_deer_pellets", "small_deer_piles")
    ),
    
    reindeer_pellets_original = coalesce_existing(
      .,
      c("reindeer_pellets_original", "reindeer_pellets", "reindeer_piles", "reindeer")
    ),
    
    #--------------------------------------------------
    # 2) Treat NA as zero ONLY in years where pellet data was collected
    #    Keep other years as NA because pellets were not part of the data
    #--------------------------------------------------
    moose_pellets_original = if_else(
      year %in% pellet_years,
      if_else(is.na(moose_pellets_original), 0, moose_pellets_original),
      NA_real_
    ),
    
    red_deer_pellets_original = if_else(
      year %in% pellet_years,
      if_else(is.na(red_deer_pellets_original), 0, red_deer_pellets_original),
      NA_real_
    ),
    
    small_deer_pellets_original = if_else(
      year %in% pellet_years,
      if_else(is.na(small_deer_pellets_original), 0, small_deer_pellets_original),
      NA_real_
    ),
    
    reindeer_pellets_original = if_else(
      year %in% pellet_years,
      if_else(is.na(reindeer_pellets_original), 0, reindeer_pellets_original),
      NA_real_
    ),
    
    #--------------------------------------------------
    # 3) Time-window correction
    #--------------------------------------------------
    days_since_jan = as.numeric(date - as.Date(paste0(year, "-01-01"))) + 1,
    
    start_doy = leaf_fall_doy(lat_mean),
    start_date = as.Date(paste0(year - 1L, "-01-01")) + round(start_doy) - 1L,
    
    t_days = as.numeric(date - start_date),
    t_days = if_else(!is.na(t_days) & t_days <= 0, NA_real_, t_days),
    
    #--------------------------------------------------
    # 4) Corrected pellet densities
    #--------------------------------------------------
    moose_pellets = if_else(
      !is.na(moose_pellets_original) & !is.na(t_days),
      (moose_pellets_original * K_plot) / (defec_rate$moose * t_days),
      NA_real_
    ),
    
    red_deer_pellets = if_else(
      !is.na(red_deer_pellets_original) & !is.na(t_days),
      (red_deer_pellets_original * K_plot) / (defec_rate$red_deer * t_days),
      NA_real_
    ),
    
    small_deer_pellets = if_else(
      !is.na(small_deer_pellets_original) & !is.na(t_days),
      (small_deer_pellets_original * K_plot) / (defec_rate$small_deer * t_days),
      NA_real_
    ),
    
    reindeer_pellets = reindeer_pellets_original,
    
    #--------------------------------------------------
    # 5) Presence/absence variables for modelling
    #--------------------------------------------------
    moose_present = if_else(
      !is.na(moose_pellets_original) & moose_pellets_original > 0,
      1, 0,
      missing = NA_real_
    ),
    
    red_deer_present = if_else(
      !is.na(red_deer_pellets_original) & red_deer_pellets_original > 0,
      1, 0,
      missing = NA_real_
    ),
    
    small_deer_present = if_else(
      !is.na(small_deer_pellets_original) & small_deer_pellets_original > 0,
      1, 0,
      missing = NA_real_
    ),
    
    reindeer_present = if_else(
      !is.na(reindeer_pellets_original) & reindeer_pellets_original > 0,
      1, 0,
      missing = NA_real_
    ),
    
    #--------------------------------------------------
    # 6) Log and combined pellet variables
    #--------------------------------------------------
    moose_pellets_only = moose_pellets,
    moose_pellets_log = log1p(moose_pellets),
    
    deer_pellets = rowSums_keepNA_df(
      tibble::tibble(
        red_deer_pellets = red_deer_pellets,
        small_deer_pellets = small_deer_pellets,
        reindeer_pellets = reindeer_pellets
      )
    ),
    
    deer_present = if_else(
      !is.na(deer_pellets) & deer_pellets > 0,
      1, 0,
      missing = NA_real_
    ),
    
    deer_pellets_log = log1p(deer_pellets),
    
    total_cervid_pellets = rowSums_keepNA_df(
      tibble::tibble(
        moose_pellets = moose_pellets,
        red_deer_pellets = red_deer_pellets,
        small_deer_pellets = small_deer_pellets,
        reindeer_pellets = reindeer_pellets
      )
    ),
    
    cervid_present = if_else(
      !is.na(total_cervid_pellets) & total_cervid_pellets > 0,
      1, 0,
      missing = NA_real_
    ),
    
    total_cervid_pellets_log = log1p(total_cervid_pellets)
  )

cat("\n--- FINAL PELLET QC AFTER FINAL STANDARDISATION ---\n")

print(
  abin_all %>%
    group_by(year) %>%
    summarise(
      n = n(),
      raw_moose_sum = sum(moose_pellets_original, na.rm = TRUE),
      corrected_moose_sum = sum(moose_pellets, na.rm = TRUE),
      raw_red_deer_sum = sum(red_deer_pellets_original, na.rm = TRUE),
      corrected_red_deer_sum = sum(red_deer_pellets, na.rm = TRUE),
      raw_small_deer_sum = sum(small_deer_pellets_original, na.rm = TRUE),
      corrected_small_deer_sum = sum(small_deer_pellets, na.rm = TRUE),
      reindeer_sum = sum(reindeer_pellets, na.rm = TRUE),
      total_sum = sum(total_cervid_pellets, na.rm = TRUE),
      n_moose_zero = sum(moose_pellets_original == 0, na.rm = TRUE),
      n_moose_positive = sum(moose_pellets_original > 0, na.rm = TRUE),
      n_moose_na = sum(is.na(moose_pellets_original)),
      n_with_date = sum(!is.na(date)),
      n_with_days_since_jan = sum(!is.na(days_since_jan)),
      n_with_t_days = sum(!is.na(t_days)),
      min_date = if_else(all(is.na(date)), as.Date(NA), min(date, na.rm = TRUE)),
      max_date = if_else(all(is.na(date)), as.Date(NA), max(date, na.rm = TRUE)),
      min_t_days = if_else(all(is.na(t_days)), NA_real_, min(t_days, na.rm = TRUE)),
      max_t_days = if_else(all(is.na(t_days)), NA_real_, max(t_days, na.rm = TRUE)),
      .groups = "drop"
    ),
  width = Inf
)
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
# 14B) FINAL POST-MERGE PINE HARMONISATION
#=============================================================================

abin_all <- abin_all %>%
  mutate(
    pine_stems = to_num(pine_stems),
    pine_winter_damage_stems = to_num(pine_winter_damage_stems),
    pine_summer_damage_stems = to_num(pine_summer_damage_stems),
    pine_yearly_damage_stems = to_num(pine_yearly_damage_stems)
  ) %>%
  mutate(
    # enforce logical consistency:
    # yearly must be at least winter
    pine_yearly_damage_stems = case_when(
      !is.na(pine_yearly_damage_stems) & !is.na(pine_winter_damage_stems) ~
        pmax(pine_yearly_damage_stems, pine_winter_damage_stems),
      is.na(pine_yearly_damage_stems) & !is.na(pine_winter_damage_stems) ~
        pine_winter_damage_stems,
      TRUE ~ pine_yearly_damage_stems
    )
  ) %>%
  mutate(
    proportion_pine_damage_winter = if_else(
      !is.na(pine_stems) & pine_stems > 0 & !is.na(pine_winter_damage_stems),
      pine_winter_damage_stems / pine_stems,
      NA_real_
    ),
    proportion_pine_damage_summer = if_else(
      !is.na(pine_stems) & pine_stems > 0 & !is.na(pine_summer_damage_stems),
      pine_summer_damage_stems / pine_stems,
      NA_real_
    ),
    proportion_pine_damage_yearly = if_else(
      !is.na(pine_stems) & pine_stems > 0 & !is.na(pine_yearly_damage_stems),
      pine_yearly_damage_stems / pine_stems,
      NA_real_
    )
  ) %>%
  mutate(
    proportion_pine_damage_winter = if_else(
      !is.na(proportion_pine_damage_winter) & proportion_pine_damage_winter > 1,
      NA_real_,
      proportion_pine_damage_winter
    ),
    proportion_pine_damage_summer = if_else(
      !is.na(proportion_pine_damage_summer) & proportion_pine_damage_summer > 1,
      NA_real_,
      proportion_pine_damage_summer
    ),
    proportion_pine_damage_yearly = if_else(
      !is.na(proportion_pine_damage_yearly) & proportion_pine_damage_yearly > 1,
      NA_real_,
      proportion_pine_damage_yearly
    )
  )
#=============================================================================
# 15) OPTIONAL CLEANUP OF TEMPORARY COLUMNS
#=============================================================================

drop_temp <- intersect(
  c(
    "pine_stems_calc",
    "pine_winter_damage_stems_calc",
    "pine_summer_damage_stems_calc",
    "pine_yearly_damage_stems_calc",
    "proportion_pine_damage_winter_calc",
    "proportion_pine_damage_summer_calc",
    "proportion_pine_damage_yearly_calc",
    "has_detailed_pine",
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
    "pine_yearly_damage_stems",
    "proportion_pine_damage_yearly",
    
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
    "wild_boar",
    
    "moose_pellets_only",
    "moose_pellets_log",
    "deer_pellets",
    "deer_pellets_log",
    "total_cervid_pellets",
    "total_cervid_pellets_log"
  ),
  names(abin_all)
)

abin_compact <- abin_all %>%
  select(all_of(compact_keep))

write_csv(
  abin_compact,
  "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/abin_compact.csv"
)

write_xlsx(
  abin_compact,
  "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/abin_compact.xlsx"
)

#=============================================================================
# 18) EXPORT
#=============================================================================

write_csv(abin_all, "abin2008_2025_full.csv")
write_xlsx(abin_all, "abin2008_2025_full.xlsx")

write_csv(
  abin_compact,
  "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/abin_compact.csv"
)

write_xlsx(
  abin_compact,
  "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/abin_compact.xlsx"
)

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

#=============================================================================
# 19) FINAL PINE DATA QUALITY CHECKS
#=============================================================================

cat("\n--- FINAL PINE QC ---\n")

print(summary(abin_all$pine_stems))
print(summary(abin_all$pine_winter_damage_stems))
print(summary(abin_all$pine_summer_damage_stems))
print(summary(abin_all$pine_yearly_damage_stems))

print(summary(abin_all$proportion_pine_damage_winter))
print(summary(abin_all$proportion_pine_damage_summer))
print(summary(abin_all$proportion_pine_damage_yearly))

cat("\nCounts of impossible values:\n")
cat("winter > stems: ",
    sum(abin_all$pine_winter_damage_stems > abin_all$pine_stems, na.rm = TRUE), "\n")
cat("summer > stems: ",
    sum(abin_all$pine_summer_damage_stems > abin_all$pine_stems, na.rm = TRUE), "\n")
cat("yearly > stems: ",
    sum(abin_all$pine_yearly_damage_stems > abin_all$pine_stems, na.rm = TRUE), "\n")

cat("yearly < winter: ",
    sum(abin_all$pine_yearly_damage_stems < abin_all$pine_winter_damage_stems, na.rm = TRUE), "\n")



