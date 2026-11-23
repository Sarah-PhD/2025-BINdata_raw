###############################################################################
# ÄBIN 2008–2026: ANNOTATED import and harmonisation pipeline
# Historical Excel + Survey123 raw CSV (2025 and 2026)
# Preserves original observations and damage-category counts; NO derived browsing metrics.
# IMPORTANT: check CONFIGURATION and review QA output before analysis.
# READ FIRST: this is the same processing logic as the previous import-only
# script, with expanded explanations; it has not been tested on your CSVs.
# Output granularity: ONE ROW PER PLOT, not one row per tree.
# Original CSV files are copied separately; plot-level aggregation cannot
# preserve every individual-tree relationship in a single row.
###############################################################################

library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)

# STEP 1 — FILE LOCATIONS AND INPUT FILENAMES
# PURPOSE: Tell R where the historical Excel workbook and each year's
# Survey123 export folder are located. Nothing is imported or changed here.
# ACTION REQUIRED: Replace survey_folder_2026 with the actual folder path.
# The expected filenames are constructed in survey_files(year); if Survey123
# used different names, change those strings rather than renaming source files.
# output_folder defaults to R's current working directory (getwd()).
# The existence checks stop immediately if a folder or workbook is missing.
# 1. CONFIGURATION -----------------------------------------------------------
historical_workbook <- "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/ABIN_each separate years files in one file.xlsx"
survey_folder_2025 <- "//storage-ume.slu.se/home$/shge0002/My Documents/ÄBIN Data/Raw ÄBIN data 2025/ÄBIN2025rawCVS"
survey_folder_2026 <-  "//storage-ume.slu.se/home$/shge0002/My Documents/ÄBIN Data/raw ÄBIN data 2026"
output_folder <- getwd()

# Change file names here if the 2026 export uses different names.
# These are the corresponding 2025 file names from the March script.
# The 2026 exports still use the original 2025 Survey123 filenames.
# Named entries are required by import_survey_year() (e.g. files[["stands"]]).
survey_files <- function(year) {
  if (!year %in% c(2025, 2026)) stop("Only 2025 and 2026 are supported")
  c(
    stands   = "ÄBIN Survey 2025_0.csv",
    plots    = "ABIN_2025_wp_repeat_1.csv",
    pine     = "ABIN_2025_Pines_2.csv",
    spruce   = "ABIN_2025_Spruce_3.csv",
    contorta = "ABIN_2025_Contorta_4.csv"
  )
}

if (!file.exists(historical_workbook)) stop("Historical workbook not found: ", historical_workbook)
if (!dir.exists(survey_folder_2025)) stop("2025 folder not found: ", survey_folder_2025)
if (!dir.exists(survey_folder_2026)) stop("2026 folder not found: ", survey_folder_2026)
if (!dir.exists(output_folder)) stop("Output folder not found: ", output_folder)

# STEP 2 — REUSABLE SMALL FUNCTIONS
# These functions are defined now but run later when called.
# to_num: convert numeric-looking text, including decimal commas, to numbers.
# rowSums_keepNA: sum measurements while keeping NA if ALL inputs are missing.
# severity_to_num: map Survey123 severity bands to approximate midpoints;
#   these are descriptive summaries, not new browsing proportions.
# rename_if_exists: rename a column only if the source exists and target does not.
# require_columns: stop if a required Survey123 field is missing.
# read_survey_csv: import one CSV without modifying its original file.
# Historical cleaning functions below standardise names for old year groups.
# NOTE: standardisation may change column names, but source files remain intact.
# 2. HELPERS -----------------------------------------------------------------
to_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x)))
}

rowSums_keepNA <- function(x) {
  if (ncol(x) == 0L) return(rep(NA_real_, nrow(x)))
  x <- as.data.frame(lapply(x, to_num))
  ans <- rowSums(x, na.rm = TRUE)
  ans[rowSums(!is.na(x)) == 0] <- NA_real_
  ans
}

severity_to_num <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[–—-]", "_", x)
  x <- gsub("\\s+", "", x)
  dplyr::recode(x, "≤10" = 5, "11_25" = 18, "26_50" = 38,
                "51_75" = 63, "76_100" = 88, .default = NA_real_)
}

rename_if_exists <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) names(df)[names(df) == old] <- new
  df
}

require_columns <- function(df, cols, source) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop(source, " is missing columns: ", paste(missing, collapse = ", "))
}

read_survey_csv <- function(folder, file) {
  path <- file.path(folder, file)
  if (!file.exists(path)) stop("Survey123 CSV not found: ", path,
                               "\nCheck the filename in survey_files().")
  readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
}

# Original historical cleaning functions from the 2008–2025 script:
clean_colnames_basic <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("\\.+", "_") %>%
    str_replace_all("/", "_") %>%
    str_replace_all("__+", "_") %>%
    str_replace_all("^_|_$", "")
}

rename_if_present <- function(df, old, new) {
  if (old %in% names(df)) {
    names(df)[names(df) == old] <- new
  }
  df
}

rename_many_if_present <- function(df, map) {
  for (old in names(map)) {
    df <- rename_if_present(df, old, map[[old]])
  }
  df
}

standardise_basic_names <- function(df) {
  
  names(df) <- clean_colnames_basic(names(df))
  
  basic_map <- c(
    "Area" = "area",
    "Område" = "area",
    "area" = "area",
    
    "Year" = "year",
    "year" = "year",
    
    "Surveyer" = "surveyor",
    "Inventerare" = "surveyor",
    "inventerare" = "surveyor",
    "surveyor" = "surveyor",
    
    "Date" = "date",
    "Datum" = "date",
    "date" = "date",
    
    "Stand" = "stand",
    "Stand_number" = "stand",
    "Bestånd" = "stand",
    "stand" = "stand",
    
    "Point" = "plot",
    "Yta" = "plot",
    "#" = "plot",
    "plot" = "plot",
    
    "PCT" = "pct",
    "Röjt?" = "pct",
    "Röjt" = "pct",
    "pct" = "pct",
    
    "Height" = "half_height",
    "Höjd_m" = "half_height",
    "half_height" = "half_height",
    "Medelhöjd" = "average_height_m",
    
    "N" = "north",
    "Nord" = "north",
    "north" = "north",
    
    "E" = "east",
    "Ost" = "east",
    "east" = "east",
    
    "Nearest_Trakt" = "nearest_tract",
    "nearest_tract" = "nearest_tract",
    
    "Productivity" = "productivity",
    "productivity" = "productivity",
    
    "Kommentar" = "comments",
    "kommentar" = "comments",
    "Comments" = "comments",
    "comments" = "comments"
  )
  
  rename_many_if_present(df, basic_map)
}

#=============================================================================
# STEP 2B — HISTORICAL COLUMN NAME DICTIONARIES
# The older surveys used different headings for comparable measurements.
# Each clean_YEAR() function maps names from that period into a shared scheme,
# e.g. downy birch counts -> downy_total and plot identifiers -> plot.
# The functions do NOT calculate winter, summer, yearly or rebrowsing values.
# They should not be assumed to make historically different measurements
# biologically equivalent: review the original field definitions when needed.
# 2) YEAR-GROUP CLEANING FUNCTIONS
#=============================================================================

clean_2008_2011 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "Pine_undamaged" = "pine_ub",
    "Pine_damaged" = "pine_damaged",
    "Pine_Top_shoot" = "pine_fts",
    "Pine_Top_shoot_Old" = "pine_fts,od",
    "Pine_Top_shoot_Presumer" = "pine_fts,ps",
    "Pine_Stem_Break" = "pine_fs",
    "Pine_Bark_damage" = "pine_fb",
    "Pine_Presummer" = "pine_ps",
    "Pine_Presummer_Old" = "pine_ps,od",
    "Pine_Old" = "pine_od",
    "Other" = "pine_o",
    "PD_H" = "pine_stems_<_hh",
    "PU_H" = "pine_ub_<_hh",
    "Over_HH" = "pine_stems_>_hh",
    "Under_HH" = "pine_stems_<_hh",
    
    "Spruce_undamaged" = "spruce_ub",
    "Spruce_damage" = "spruce_damaged",
    "Spruce_Top_shoot" = "spruce_fts",
    "Spruce_Top_shoot_Old" = "spruce_fts,od",
    "Spruce_Bark_damage" = "spruce_fb",
    "Spruce_Stem_Break" = "spruce_fs",
    "Spruce_Old" = "spruce_od",
    "Spruce_Other" = "spruce_o",
    "Spruce_Over_HH" = "spruce_>_hh",
    
    "DownyBirch_Height" = "downy_height",
    "DownyBirch_Total" = "downy_total",
    "SilverBirch_Height" = "silver_height",
    "SilverBirch_Total" = "silver_total",
    
    "Rowan_Height" = "rowan_height",
    "Rowan_Total" = "rowan_total",
    "Aspen_Height" = "aspen_height",
    "Aspen_Total" = "aspen_total",
    "Salix_Height" = "salix_height",
    "Salix_Total" = "salix_total",
    "Oak_Height" = "oak_height",
    "Oak_Total" = "oak_total",
    
    "Moose_Pellets" = "moose_pellets_raw",
    "RedDeer_Pellts" = "red_deer_pellets_raw",
    "OtherDeer_Pellets" = "small_deer_pellets_raw",
    "WildBore_Pellets" = "wild_boar"
  )
  
  rename_many_if_present(df, map)
}

clean_2012_2019 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "Pine_undamaged" = "pine_ub",
    "Pine_damaged" = "pine_damaged",
    "Pine_Top" = "pine_fts",
    "Pine_Stem" = "pine_fs",
    "Pine_Bark" = "pine_fb",
    
    "Downy_undamaged" = "downy_ub",
    "Downy_damaged" = "downy_damaged",
    "Downy_Top" = "downy_fts",
    "Downy_Stem" = "downy_fs",
    "Downy_Bark" = "downy_fb",
    
    "Silver_undamaged" = "silver_ub",
    "Silver_damaged" = "silver_damaged",
    "Silver_Top" = "silver_fts",
    "Silver_Stem" = "silver_fs",
    "Silber_Bark" = "silver_fb",
    
    "Spruce_undamaged" = "spruce_ub",
    "Spruce_damaged" = "spruce_damaged",
    "Spruce_Top" = "spruce_fts",
    "Spruce_Stem" = "spruce_fs",
    "Spruce_Bark" = "spruce_fb",
    
    "Rowan_Height" = "rowan_height",
    "Rowan_damaged" = "rowan_damaged",
    "Aspen_Height" = "aspen_height",
    "Aspen_damaged" = "aspen_damaged",
    "Salix_Height" = "salix_height",
    "Salix_damaged" = "salix_damaged",
    "Oak_Height" = "oak_height",
    "Oak_damaged" = "oak_damaged"
  )
  
  rename_many_if_present(df, map)
}

clean_2021 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "T_oskad" = "pine_ub",
    "T_topp_färsk" = "pine_fts",
    "T_topp_fjol" = "pine_fts,ps",
    "T_topp_gam" = "pine_od",
    "Tallar___17" = "pine_stems",
    "Tallar...17" = "pine_stems",
    "Tallar_17" = "pine_stems",
    "T_brott_färsk" = "pine_fs",
    "T_brott_fjol" = "pine_fs,ps",
    "T_brott_gam" = "pine_fs,od",
    "T_bark_färsk" = "pine_fb",
    "T_bark_fjol" = "pine_fb,ps",
    "T_bark_gam" = "pine_fb,od",
    "Färsk_tallskada_antal" = "pine_fresh_damage_stems",
    "Färsk_tallskada_%" = "proportion_pine_damage_fresh",
    "Årsskada_tall" = "pine_yearly_damage_stems",
    
    "GB_oskad" = "downy_ub",
    "GB_topp_färsk" = "downy_fts",
    "GB_topp_gam" = "downy_od",
    "Glasbjörk___30" = "downy_total",
    "Glasbjörk...30" = "downy_total",
    "Glasbjörk_30" = "downy_total",
    "GB_brott_färsk" = "downy_fs",
    "GB_brott_gam" = "downy_fs,od",
    "GB_bark_färsk" = "downy_fb",
    "GB_bark_gam" = "downy_fb,od",
    
    "VB_oskad" = "silver_ub",
    "VB_topp_färsk" = "silver_fts",
    "VB_topp_fjol" = "silver_fts,ps",
    "VB_topp_gam" = "silver_od",
    "Vårtbjörk___39" = "silver_total",
    "Vårtbjörk...39" = "silver_total",
    "Vårtbjörk_39" = "silver_total",
    "VB_brott_färsk" = "silver_fs",
    "VB_brott_fjol" = "silver_fs,ps",
    "VB_brott_gam" = "silver_fs,od",
    "VB_bark_färsk" = "silver_fb",
    "VB_bark_gam" = "silver_fb,od",
    
    "G_oskad" = "spruce_ub",
    "G_topp_färsk" = "spruce_fts",
    "G_topp_fjol" = "spruce_fts,ps",
    "G_topp_gam" = "spruce_od",
    "Gran___49" = "spruce_stems",
    "Gran...49" = "spruce_stems",
    "Gran_49" = "spruce_stems",
    "G_brott_färsk" = "spruce_fs",
    "G_brott_gam" = "spruce_fs,od",
    "G_bark_färsk" = "spruce_fb",
    "G_bark_gam" = "spruce_fb,od",
    
    "R_höjd_m" = "rowan_height",
    "R_betad" = "rowan_damaged",
    "Rönn_bet" = "rowan_damaged",
    "Rönn_obet" = "rowan_undamaged",
    
    "A_höjd_m" = "aspen_height",
    "A_betad" = "aspen_damaged",
    "Asp_bet" = "aspen_damaged",
    "asp_obet" = "aspen_undamaged",
    
    "S_höjd_m" = "salix_height",
    "S_betad" = "salix_damaged",
    "sälg_bet" = "salix_damaged",
    "sälg_obet" = "salix_undamaged",
    
    "E_höjd_m" = "oak_height",
    "E_betad" = "oak_damaged",
    "ek_bet" = "oak_damaged",
    "ek_obet" = "oak_undamaged",
    
    "T_låg_bet" = "pine_browsed_<_hh",
    "T_låg_obet" = "pine_ub_<_hh",
    
    "Antal_barrstammar" = "conifer_stems",
    "Antal_skadade_barrstammar" = "conifer_damaged_stems",
    "Andel_skadade_barrstammar" = "proportion_conifer_damaged",
    
    "Totalt" = "total_stems_density_or_summary",
    "Andel_tall" = "proportion_pine",
    "Andel_gran" = "proportion_spruce",
    "Andel_björk" = "proportion_birch",
    "Talltäthet" = "pine_density",
    "Grantäthet" = "spruce_density",
    "Björktäthet" = "birch_density",
    "Stamtäthet" = "stem_density",
    "Tall_+_Björk" = "pine_plus_birch"
  )
  
  df <- rename_many_if_present(df, map)
  names(df) <- make.unique(names(df), sep = "...dup")
  
  df <- df %>%
    select(
      -any_of(c(
        "Tallar...77", "Tallar_77",
        "Gran...78", "Gran_78",
        "Glasbjörk...79", "Glasbjörk_79",
        "Vårtbjörk...80", "Vårtbjörk_80",
        "76"
      ))
    )
  
  df
}

clean_2022 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "#" = "plot",
    "nearest_tract" = "nearest_tract",
    "half_height" = "half_height",
    "PCT" = "pct",
    
    "yearly_damage" = "pine_yearly_damage_stems",
    "proportion_of_yearly_damage" = "proportion_pine_damage_yearly",
    
    ">50%" = "pine_>_50",
    "<50%" = "pine_<_50",
    ">50%_gran" = "spruce_>_50",
    
    "O" = "pine_ub",
    "T" = "pine_fts",
    "T+G" = "pine_fts,od",
    "S" = "pine_fs",
    "B" = "pine_fb",
    "G" = "pine_od",
    "A" = "pine_o",
    
    "Glasbjörk_(antal)" = "downy_total",
    "Vårtbjörk_(antal)" = "silver_total",
    "Rönn_(höjd)" = "rowan_height",
    "Rönn_(antal)" = "rowan_total",
    "Asp_(höjd)" = "aspen_height",
    "Asp_(antal)" = "aspen_total",
    "Salix_(höjd)" = "salix_height",
    "Salix_(antal)" = "salix_total",
    "Ek_(höjd)" = "oak_height",
    "Ek_(antal)" = "oak_total",
    
    "Ä" = "moose_pellets_raw",
    "K" = "red_deer_pellets_raw",
    "övrig_hjort" = "small_deer_pellets_raw",
    "V" = "wild_boar"
  )
  
  rename_many_if_present(df, map)
}

clean_2023_2025 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  spruce_map <- c(
    "o,spruce_fts" = "spruce_fts,o",
    "o,spruce_od"  = "spruce_od,o",
    "o,spruce_ub"  = "spruce_ub,o",
    "od,spruce_fs" = "spruce_fs,od",
    "od,spruce_o"  = "spruce_o,od"
  )
  
  rename_many_if_present(df, spruce_map)
}


# STEP 3 — READ AND STANDARDISE HISTORICAL DATA
# Import the five historical Excel sheets, then apply the appropriate
# period-specific renaming function to each sheet.
# The final historical sheet is labelled 2023–2025, but its 2025 rows are
# deliberately EXCLUDED because 2025 will instead come from Survey123.
# This prevents including 2025 twice. Rows with unrecognisable years cause
# a stop rather than being silently discarded.
# h08/h12/h21/h22/h23 are intermediate tables, not output files.
# 3. IMPORT HISTORICAL WORKBOOK (2008–2024 ONLY) -------------------------------
# The 2023–2025 sheet contains 2025: exclude those rows to prevent duplication.
required_sheets <- c("2008 to 2011", "2012 to 2019", "2021", "2022", "2023 to 2025")
if (!all(required_sheets %in% excel_sheets(historical_workbook))) {
  stop("Historical workbook is missing sheets: ",
       paste(setdiff(required_sheets, excel_sheets(historical_workbook)), collapse = ", "))
}

h08 <- read_excel(historical_workbook, sheet = "2008 to 2011")
h12 <- read_excel(historical_workbook, sheet = "2012 to 2019")
h21 <- read_excel(historical_workbook, sheet = "2021")
h22 <- read_excel(historical_workbook, sheet = "2022")
h23 <- read_excel(historical_workbook, sheet = "2023 to 2025")

h08 <- clean_2008_2011(h08)
h12 <- clean_2012_2019(h12)
h21 <- clean_2021(h21)
h22 <- clean_2022(h22)
h23 <- clean_2023_2025(h23)
h21$year <- 2021
h22$year <- 2022

# Do not silently lose historical rows without a valid year.
if (!"year" %in% names(h23)) stop("2023–2025 sheet has no year column after cleaning")
h23_year <- to_num(h23$year)
if (any(is.na(h23_year))) stop("Some 2023–2025 rows have missing/non-numeric year; inspect before continuing")
h23 <- h23[h23_year %in% c(2023, 2024), , drop = FALSE]

# STEP 4 — CONVERT EACH SURVEY123 YEAR INTO A PLOT-LEVEL TABLE
# Survey123 exports are relational: one stand can contain multiple plots,
# and each plot can have multiple pine/spruce/contorta damage records.
# The stand CSV supplies stand attributes; the plot CSV supplies plot data.
# The species CSVs contain repeat records linked by ParentGlobalID.
# summarise_damage() groups those repeat records by plot and damage label.
# IMPORTANT: the resulting damage-category counts count RECORDS/EVENTS;
# they are not necessarily counts of DISTINCT browsed trees.
# We keep original CSV exports separately so this aggregation can be audited.
# No winter/yearly/summer/rebrowsing proportions are calculated in this step.
# 4. SURVEY123 IMPORT: ONE FUNCTION FOR BOTH YEARS ----------------------------
# The March 2026 script read stands, plots, pine, spruce and contorta separately.
# Here the exact same logic is run for both years, with year-specific outputs.

# FUNCTION DETAIL: species repeat records -> plot-level summaries.
# Validate the parent plot ID, count each recorded damage label, and create
# one column per label (wide format). Also retain event counts and optional
# severity summaries. Missing damage labels are not counted as zero damage.
# The count() step counts Survey123 rows, NOT independently verified stems.
summarise_damage <- function(df, year, species, plot_ids) {
  require_columns(df, c("ParentGlobalID", "Select damage type"), species)
  df <- rename_if_exists(df, "ParentGlobalID", "plot_id")
  df <- rename_if_exists(df, "Select damage type", "damage")
  if (any(is.na(df$plot_id))) stop(species, " has rows with missing plot_id")
  unknown <- setdiff(unique(as.character(df$plot_id)), as.character(plot_ids))
  if (length(unknown)) warning(year, " ", species, ": ", length(unknown),
                               " plot IDs not found in plots CSV")
  
  # Event counts by damage category. Damage labels remain available for audit.
  # Prefix species names to avoid collisions between pine, spruce and contorta.
  counts <- df %>%
    filter(!is.na(damage), trimws(as.character(damage)) != "") %>%
    count(plot_id, damage, name = "damage_events") %>%
    mutate(damage = paste0(species, "_", damage)) %>%
    pivot_wider(names_from = damage, values_from = damage_events,
                values_fill = list(damage_events = NA_integer_))
  
  # Preserve Survey123 severity as a descriptive summary, not a browsing proportion.
  sev_name <- "how much relative damage"
  if (species %in% c("pine", "contorta") && sev_name %in% names(df)) {
    df$severity_num <- severity_to_num(df[[sev_name]])
  } else df$severity_num <- NA_real_
  
  n_events <- function(x) sum(!is.na(x))
  result <- df %>% group_by(plot_id) %>% summarise(
    events = n(),
    severity_sum = if (all(is.na(severity_num))) NA_real_ else sum(severity_num, na.rm = TRUE),
    severity_mean = if (all(is.na(severity_num))) NA_real_ else mean(severity_num, na.rm = TRUE),
    severe_n = if (all(is.na(severity_num))) NA_real_ else sum(severity_num >= 38, na.rm = TRUE),
    .groups = "drop"
  )
  names(result)[-1] <- paste0(species, "_", names(result)[-1], "_", year)
  
  if (species %in% c("pine", "contorta")) {
    stem_name <- if (species == "pine") "Pine Stems < HH" else "Contorta Stems < HH"
    browse_name <- "Browsed < HH"
    # The 2026 pine repeat export omits both of these columns. Add missing
    # measurements as NA (unknown), never 0 (observed absence). This happens
    # only in the in-memory working copy; the source CSV is unchanged.
    if (!stem_name %in% names(df)) {
      message(year, " ", species, ": missing column '", stem_name,
              "'; retaining its values as NA")
      df[[stem_name]] <- NA_real_
    }
    if (!browse_name %in% names(df)) {
      message(year, " ", species, ": missing column '", browse_name,
              "'; retaining its values as NA")
      df[[browse_name]] <- NA_real_
    }
    # Restore NA if all values were missing for a plot.
    extras <- df %>% group_by(plot_id) %>% summarise(
      stems_lthh = if (all(is.na(to_num(.data[[stem_name]])))) NA_real_ else sum(to_num(.data[[stem_name]]), na.rm = TRUE),
      browsed_lthh = if (all(is.na(to_num(.data[[browse_name]])))) NA_real_ else sum(to_num(.data[[browse_name]]), na.rm = TRUE),
      .groups = "drop"
    )
    names(extras)[-1] <- paste0(species, "_", names(extras)[-1], "_", year)
    result <- left_join(result, extras, by = "plot_id")
  }
  list(counts = counts, summary = result)
}

# FUNCTION DETAIL: run the same five-file import for either 2025 or 2026.
# Check stand/plot GlobalIDs are unique and plot-to-stand links are valid.
# Aggregate each species repeat table before joining, preventing a
# many-to-many join from multiplying the number of sampling plots.
# The final row count must equal the number of rows in the plots CSV.
import_survey_year <- function(year, folder) {
  files <- survey_files(year)
  stands <- read_survey_csv(folder, files[["stands"]])
  plots <- read_survey_csv(folder, files[["plots"]])
  pine <- read_survey_csv(folder, files[["pine"]])
  spruce <- read_survey_csv(folder, files[["spruce"]])
  contorta <- read_survey_csv(folder, files[["contorta"]])
  
  require_columns(stands, "GlobalID", "stands")
  require_columns(plots, c("GlobalID", "ParentGlobalID"), "plots")
  stands <- rename_if_exists(stands, "GlobalID", "stand_id")
  plots <- rename_if_exists(plots, "GlobalID", "plot_id")
  plots <- rename_if_exists(plots, "ParentGlobalID", "stand_id")
  if (anyDuplicated(stands$stand_id)) stop(year, " duplicate stand GlobalIDs")
  if (anyDuplicated(plots$plot_id)) stop(year, " duplicate plot GlobalIDs")
  if (any(is.na(plots$stand_id))) stop(year, " plots missing stand_id")
  if (any(!plots$stand_id %in% stands$stand_id)) stop(year, " plots refer to missing stands")
  
  pine_d <- summarise_damage(pine, year, "pine", plots$plot_id)
  spruce_d <- summarise_damage(spruce, year, "spruce", plots$plot_id)
  contorta_d <- summarise_damage(contorta, year, "contorta", plots$plot_id)
  
  result <- plots %>%
    left_join(pine_d$counts, by = "plot_id") %>%
    left_join(pine_d$summary, by = "plot_id") %>%
    left_join(spruce_d$counts, by = "plot_id") %>%
    left_join(spruce_d$summary, by = "plot_id") %>%
    left_join(contorta_d$counts, by = "plot_id") %>%
    left_join(contorta_d$summary, by = "plot_id") %>%
    left_join(stands, by = "stand_id")
  if (nrow(result) != nrow(plots)) stop(year, " join changed plot row count")
  result$year <- year
  cat(year, ":", nrow(stands), "stands,", nrow(plots), "plots,",
      nrow(result), "joined rows\n")
  result
}

# STEP 4B — ARCHIVE THE EXACT INPUT EXPORTS
# Copy the five Survey123 CSV files from each year into a dated subfolder.
# This creates a reproducible snapshot and leaves the original exports alone.
# Existing archived files are not overwritten; if the source exports change,
# compare or deliberately replace the archive before assuming it is current.
# Archive source CSVs without modifying the original files.
archive_dir <- file.path(output_folder, "abin_2008_2026_survey123_original_exports")
dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
for (yr in c(2025, 2026)) {
  folder <- if (yr == 2025) survey_folder_2025 else survey_folder_2026
  target <- file.path(archive_dir, as.character(yr))
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  source_files <- file.path(folder, survey_files(yr))
  if (!all(file.exists(source_files))) stop("Missing source files: ", paste(source_files[!file.exists(source_files)], collapse=", "))
  if (!all(file.copy(source_files, target, overwrite=FALSE) | file.exists(file.path(target, basename(source_files))))) stop("Could not archive Survey123 source files")
}

survey_2025_raw <- import_survey_year(2025, survey_folder_2025)
survey_2026_raw <- import_survey_year(2026, survey_folder_2026)

# STEP 5 — MATCH 2025 AND 2026 SURVEY123 NAMES TO HISTORICAL NAMES
# The Survey123 plot fields have long question labels such as
# 'Downy Birch Stems' and 'Pre-commercially thinned?'.
# survey_map translates these into the same concise names used in the
# historical dataset, such as downy_total and pct.
# Both birch species remain SEPARATE. Pine, spruce, contorta, other
# broadleaves, pellet data, and other unmatched columns are retained.
# Area abbreviations are expanded to readable names.
# Required-field checks deliberately stop if an expected question changed.
# This helps catch changed Survey123 forms rather than losing a column.
# 5. HARMONISE SURVEY123 PLOT COLUMNS -----------------------------------------
# Same names in 2025/2026; explicit mapping based on the March 2026 script.
survey_map <- c(
  "Area" = "area", "Stand" = "stand", "Surveyor" = "surveyor",
  "Sampling plot 1-10" = "plot", "Pre-commercially thinned?" = "pct",
  "Half height (m)" = "half_height", "x.x" = "north", "y.x" = "east",
  "Closest tract" = "nearest_tract", "Site productivity" = "productivity",
  "Downy Birch Highest Height (m)" = "downy_height",
  "Downy Birch Stems" = "downy_total",
  "Downy birch damage (whole plot)" = "downy_damage",
  "Silver Birch Highest Height (m)" = "silver_height",
  "Silver Birch Stems" = "silver_total",
  "Silver Birch damage (whole plot)" = "silver_damage",
  "Rowan Highest Height (m)" = "rowan_height", "Rowan Stems" = "rowan_total",
  "Rowan damage (whole plot)" = "rowan_damage",
  "Oak Highest Height (m)" = "oak_height", "Oak Stems" = "oak_total",
  "Oak Damage (whole plot)" = "oak_damage",
  "Salix Highest Height (m)" = "salix_height", "Salix Stems" = "salix_total",
  "Salix Damage (whole plot)" = "salix_damage",
  "Aspen Highest Height (m)" = "aspen_height", "Aspen Stems" = "aspen_total",
  "Aspen Damage (whole plot)" = "aspen_damage"
)

# FUNCTION DETAIL: rename plot-level Survey123 fields and recode areas.
# A missing required field stops execution; inspect source CSV headers
# rather than silently skipping it. Unmapped columns are kept as-is.
harmonise_survey <- function(df) {
  require_columns(df, names(survey_map), "Survey123 merged data")
  for (old in names(survey_map)) df <- rename_if_exists(df, old, survey_map[[old]])
  df$area <- dplyr::recode(as.character(df$area),
                           "om" = "OsterMalma", "at" = "Åtvidaberg", "vx" = "Växjö",
                           "bk" = "Barksätter", "fk" = "Fredrika", "fu" = "Furudal",
                           "lf" = "Lofsdalen", "lj" = "Ljusdal", "ly" = "Lycksele",
                           "ma" = "Malå", "no" = "Nordmaling", "ra" = "Råneå", "so" = "Sorsele",
                           .default = as.character(df$area))
  df <- clean_2023_2025(df)
  df
}
survey_2025 <- harmonise_survey(survey_2025_raw)
survey_2026 <- harmonise_survey(survey_2026_raw)

# STEP 6 — STACK ALL PLOT TABLES INTO ONE MASTER TABLE
# 'bind_rows' adds rows from each period/year beneath one another.
# It matches columns by NAME, not their position. If a column exists in
# one year but not another, the missing year's entries become NA.
# All fields are temporarily converted to character to avoid type errors
# between Excel and CSV imports. Selected measurements are restored later.
# make.unique() protects against duplicate names, but the next loop merges
# duplicate-name columns ONLY if their non-missing values do not disagree.
# Conflicting duplicate columns are retained and flagged for manual review.
# No observations are intentionally removed during binding.
# 6. BIND HISTORICAL AND SURVEY123 --------------------------------------------
# Character-safe bind follows the first supplied script; numeric columns restored below.
# FUNCTION DETAIL: prepare differing year-group tables for bind_rows().
# The conversion to character is temporary for comparable measurements;
# categorical fields and unrecognised numeric fields may remain text.
make_bind_safe <- function(df) {
  df <- as.data.frame(df, check.names = FALSE)
  names(df) <- make.unique(names(df), sep = "...dup")
  df[] <- lapply(df, as.character)
  df
}

parts <- list(h08, h12, h21, h22, h23, survey_2025, survey_2026)
parts <- lapply(parts, make_bind_safe)
abin_combined <- dplyr::bind_rows(parts)

# Collapse ...dup columns only if their non-missing values do not conflict.
dups <- grep("\\.\\.\\.dup[0-9]+$", names(abin_combined), value = TRUE)
for (dup in dups) {
  base <- sub("\\.\\.\\.dup[0-9]+$", "", dup)
  if (base %in% names(abin_combined)) {
    a <- abin_combined[[base]]; b <- abin_combined[[dup]]
    conflict <- !is.na(a) & !is.na(b) & a != b
    if (any(conflict)) warning("Conflicting duplicate columns: ", base, " / ", dup,
                               "; preserving duplicate for review")
    else {
      abin_combined[[base]] <- coalesce(a, b)
      abin_combined[[dup]] <- NULL
    }
  }
}

# STEP 7 — QUALITY CONTROL AND SAVE (BEFORE BROWSING CALCULATIONS)
# Restore numeric types only where every nonblank entry can be parsed.
# Leave categories such as PCT as text so labels are not converted to NA.
# Build year/area/stand/plot identifiers and warn about missing or repeated
# combinations. A duplicate-key report is written when duplicates occur.
# A row-count check confirms that the combination retained every imported
# plot-level row. It does NOT prove that all column mappings are correct.
# Finally, export a CSV and an Excel copy of the harmonised master.
# No new browsing proportions are computed here; those belong in a
# separate, explicitly validated analysis script.
# 7. SAVE RAW-HARMONISED PLOT MASTER -----------------------------------------
# The master is one row per plot. No winter/summer/yearly/rebrowsing metrics
# are calculated here. Survey123 damage category counts are *event counts*,
# not necessarily distinct stems. The source CSVs are archived separately.

# Preserve raw names and labels in separate source CSVs. Original values in
# harmonised columns are not overwritten by derived damage metrics.

# Numeric conversion is restricted to known measurement columns; leave
# categorical fields (especially pct and damage labels) unchanged.
measurement_names <- grep(
  "^(year|plot|age|half_height|average_height_m|north|east|productivity|downy_total|silver_total|downy_height|silver_height|rowan_total|aspen_total|salix_total|oak_total)$|(_pellets|_stems|_height|_total|_events_[0-9]{4}$)",
  names(abin_combined), value=TRUE
)
for (nm in measurement_names) {
  x <- abin_combined[[nm]]
  vals <- to_num(x)
  nonblank <- !is.na(x) & trimws(as.character(x)) != ""
  if (!any(nonblank & is.na(vals))) abin_combined[[nm]] <- vals
}

# Add unique plot key while keeping original stand and plot identifiers.
# Survey123 uses a GlobalID; historical rows use year/area/stand/plot.
if (!all(c("year", "area", "stand", "plot") %in% names(abin_combined))) {
  stop("Missing one of year, area, stand, plot in combined dataset")
}
key <- paste(abin_combined$year, abin_combined$area,
             abin_combined$stand, abin_combined$plot, sep="|")
missing_key <- is.na(abin_combined$year) | is.na(abin_combined$area) |
  is.na(abin_combined$stand) | is.na(abin_combined$plot)
if (any(missing_key)) warning(sum(missing_key), " rows have incomplete plot keys; inspect them")
if (anyDuplicated(key[!missing_key])) {
  warning("Duplicate year/area/stand/plot combinations found. Inspect before analysis; no rows removed.")
  write_csv(abin_combined[!missing_key & (duplicated(key) | duplicated(key, fromLast=TRUE)), ,drop=FALSE],
            file.path(output_folder, "abin_2008_2026_duplicate_plot_keys_review.csv"))
}

# Verify Survey123 imports remain one row per plot after harmonisation.
stopifnot(nrow(survey_2025_raw) == nrow(survey_2025),
          nrow(survey_2026_raw) == nrow(survey_2026))
expected <- sum(vapply(parts, nrow, integer(1)))
stopifnot(nrow(abin_combined) == expected)

abin_2008_2026_combined_raw <- abin_combined
cat("\nRows by year:\n")
print(table(abin_2008_2026_combined_raw$year, useNA="ifany"))
cat("\nTotal rows and columns:\n")
print(dim(abin_2008_2026_combined_raw))

write_csv(abin_2008_2026_combined_raw,
          file.path(output_folder, "abin_2008_2026_combined_raw.csv"))
write_xlsx(abin_2008_2026_combined_raw,
           file.path(output_folder, "abin_2008_2026_combined_raw.xlsx"))

message("Saved plot-level master, original Survey123 exports, and any duplicate-key review file.")
message("Browsing calculations intentionally deferred to a separate script.")

#==============================================================================
# RECONSTRUCT MISSING 2021 STAND IDENTIFIERS
#==============================================================================
#
# PURPOSE:
# Identify stands in the 2021 dataset where the original stand number
# is missing, using the combination of area, north and east coordinates.
#
# Every coordinate group has 10 sampling plots, consistent with the
# survey design.
#
# The original stand column remains unchanged.
#==============================================================================

library(dplyr)

h21 <- h21 %>%
  mutate(
    stand_id_reconstructed = case_when(
      !is.na(stand) ~ paste0("2021_", area, "_", stand),
      is.na(stand) & !is.na(north) & !is.na(east) ~
        paste0("2021_", area, "_coord_", north, "_", east),
      TRUE ~ NA_character_
    ),
    stand_id_source = case_when(
      !is.na(stand) ~ "original",
      is.na(stand) & !is.na(north) & !is.na(east) ~ "coordinates",
      TRUE ~ "unresolved"
    )
  )

# Check results
h21 %>%
  count(stand_id_source)

# Check how many distinct stand identifiers were created
h21 %>%
  filter(stand_id_source == "coordinates") %>%
  summarise(
    reconstructed_stands = n_distinct(stand_id_reconstructed),
    plots = n()
  )

# Show the 10 plots without stand identifiers or complete coordinates

h21 %>%
  filter(stand_id_source == "unresolved") %>%
  select(
    area,
    stand,
    plot,
    year,
    north,
    east,
    pine_ub,
    pine_stems,
    stand_id_reconstructed
  ) %>%
  print(n = Inf)



#==============================================================================
# STEP 7 — DATA VALIDATION AND QUALITY CONTROL
#==============================================================================
#
# DESCRIPTION:
#
# This section evaluates the quality and consistency of the combined ÄBIN
# dataset (2008–2026) before it is used for statistical analysis.
#
# Because the dataset combines historical Excel records with more recent
# Survey123 exports, differences in column names, data formats, recording
# methods and missing information may exist between survey years.
#
# The validation process investigates four potential issues:
#
# 1. DUPLICATE PLOT IDENTIFIERS
#    Identifies observations sharing the same year, area, stand and plot
#    combination. These may represent duplicate records or inconsistencies
#    in plot identification.
#
# 2. CONFLICTING COLUMNS
#    Investigates columns that appear to represent the same measurement
#    but contain different values. Particular attention is given to pine
#    damage variables and other tree species measurements.
#
# 3. NUMERIC CONSISTENCY
#    Identifies measurement columns containing non-numeric values that
#    may prevent subsequent calculations or statistical analyses.
#
# 4. MISSING AND NEGATIVE VALUES
#    Summarises missing measurements across survey years and identifies
#    negative values that may be inappropriate for stem counts, browsing
#    damage and pellet counts.
#
# OUTPUT:
#
# Diagnostic summaries are printed in the R console, and detailed review
# tables are saved in the folder:
#
#   abin_2008_2026_validation
#
# IMPORTANT:
#
# This section is diagnostic only. It does not remove observations,
# replace missing values, resolve conflicting measurements or alter
# the original combined dataset.
#
# Identified problems must be assessed before any corrections are made.
# Browsing damage calculations will be performed separately, after
# the relevant data quality checks have been completed.
#
#==============================================================================

#==============================================================================
# STEP 7 — VALIDATE THE COMBINED 2008–2026 DATASET
#==============================================================================
#
# PURPOSE:
# Investigate potential problems before calculating browsing damage.
#
# CHECKS:
#   7A. Duplicate plot identifiers
#   7B. Conflicting columns
#   7C. Numeric measurement consistency
#   7D. Missing values and negative measurements
#
# IMPORTANT:
# This section is diagnostic only.
# It does not modify abin_2008_2026_combined_raw.
#
# Review files are saved in a separate validation folder.
#==============================================================================

library(dplyr)
library(readr)
library(tidyr)

df_check <- abin_2008_2026_combined_raw

validation_folder <- file.path(
  output_folder,
  "abin_2008_2026_validation"
)

dir.create(
  validation_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


#==============================================================================
# STEP 7A — INVESTIGATE DUPLICATE PLOT IDENTIFIERS
#==============================================================================
#
# A plot is currently identified using:
#   year + area + stand + plot
#
# We exclude incomplete identifiers from the duplicate test.
# Missing stand identifiers from 2021 are investigated separately.
#
# We do NOT remove any duplicate observations.
#==============================================================================

cat("\n========== STEP 7A: DUPLICATE PLOT IDENTIFIERS ==========\n")

df_keys <- df_check %>%
  mutate(
    across(
      all_of(c("year", "area", "stand", "plot")),
      ~ na_if(trimws(as.character(.x)), "")
    ),
    complete_plot_key =
      !is.na(year) &
      !is.na(area) &
      !is.na(stand) &
      !is.na(plot)
  )

duplicate_groups <- df_keys %>%
  filter(complete_plot_key) %>%
  count(
    year, area, stand, plot,
    name = "n_records"
  ) %>%
  filter(n_records > 1) %>%
  arrange(year, area, stand, plot)

cat("\nNumber of duplicated plot-key groups:\n")
print(nrow(duplicate_groups))

cat("\nDuplicate groups by year:\n")

duplicate_groups %>%
  group_by(year) %>%
  summarise(
    duplicate_groups = n(),
    records_in_groups = sum(n_records),
    .groups = "drop"
  ) %>%
  print(n = Inf)

cat("\nFirst 30 duplicate groups:\n")

tibble::as_tibble(duplicate_groups) %>%
  head(30) %>%
  print(n = 30)

# Save the duplicated keys.
write_csv(
  duplicate_groups,
  file.path(validation_folder, "duplicate_plot_groups.csv")
)

# Save ALL records belonging to duplicated keys,
# retaining every original measurement column.
duplicate_records <- df_keys %>%
  semi_join(
    duplicate_groups,
    by = c("year", "area", "stand", "plot")
  )

write_csv(
  duplicate_records,
  file.path(validation_folder, "duplicate_plot_full_records.csv")
)

# Check whether repeated keys correspond to different Survey123 GlobalIDs.
if ("plot_id" %in% names(duplicate_records)) {
  
  cat("\nDuplicate keys and Survey123 GlobalIDs:\n")
  
  duplicate_records %>%
    group_by(year, area, stand, plot) %>%
    summarise(
      records = n(),
      distinct_global_ids = n_distinct(
        plot_id,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    count(year, distinct_global_ids) %>%
    print(n = Inf)
}


#==============================================================================
# STEP 7B — INVESTIGATE CONFLICTING COLUMN PAIRS
#==============================================================================
#
# Six column pairs were flagged during the original import.
#
# For example:
#   pine_od
#   pine_od...dup1
#
# We need to know:
#   - Which years contain conflicting values?
#   - How many rows are affected?
#   - Are the values actually different?
#
# This section preserves both columns.
#==============================================================================

cat("\n========== STEP 7B: CONFLICTING COLUMNS ==========\n")

conflict_bases <- c(
  "rowan_damaged",
  "aspen_damaged",
  "salix_damaged",
  "oak_damaged",
  "pine_od",
  "pine_o"
)

conflict_summary <- list()
conflict_examples <- list()

for (base in conflict_bases) {
  
  duplicate_name <- paste0(base, "...dup1")
  
  if (!all(c(base, duplicate_name) %in% names(df_check))) {
    cat("\nColumn pair not found:", base, "\n")
    next
  }
  
  a <- trimws(as.character(df_check[[base]]))
  b <- trimws(as.character(df_check[[duplicate_name]]))
  
  a[a == ""] <- NA_character_
  b[b == ""] <- NA_character_
  
  both_present <- !is.na(a) & !is.na(b)
  different <- both_present & a != b
  
  summary_one <- tibble(
    variable = base,
    total_rows = nrow(df_check),
    both_present = sum(both_present),
    conflicting_rows = sum(different),
    only_first_present = sum(!is.na(a) & is.na(b)),
    only_second_present = sum(is.na(a) & !is.na(b))
  )
  
  conflict_summary[[base]] <- summary_one
  
  if (any(different)) {
    
    examples <- df_check[different, , drop = FALSE] %>%
      select(
        any_of(c(
          "year", "area", "stand", "plot",
          "plot_id", base, duplicate_name
        ))
      )
    
    examples$variable <- base
    
    conflict_examples[[base]] <- examples
    
    cat("\nConflicts for:", base, "\n")
    
    examples %>%
      count(year, name = "conflicting_rows") %>%
      as.data.frame() %>%
      print()
  }
}

conflict_summary_df <- bind_rows(conflict_summary)

cat("\nOverall conflicting-column summary:\n")
print(as.data.frame(conflict_summary_df))

write_csv(
  conflict_summary_df,
  file.path(validation_folder, "conflicting_columns_summary.csv")
)

if (length(conflict_examples) > 0) {
  
  for (base in names(conflict_examples)) {
    
    write_csv(
      conflict_examples[[base]],
      file.path(
        validation_folder,
        paste0("conflicts_", base, ".csv")
      )
    )
  }
}


#==============================================================================
# STEP 7C — CHECK NUMERIC MEASUREMENTS
#==============================================================================
#
# PURPOSE:
# Find columns that appear to contain numeric measurements
# but also contain non-numeric text.
#
# This is particularly important for:
#   - Pine stems and damage categories
#   - Birch stem counts
#   - Spruce measurements
#   - Pellet counts
#
# Non-numeric values are reported, not automatically converted.
#==============================================================================

cat("\n========== STEP 7C: NUMERIC CONSISTENCY ==========\n")

measurement_pattern <- paste(
  "pine|spruce|contorta|downy|silver|",
  "rowan|aspen|salix|oak|",
  "pellet|stem|height|density",
  sep = ""
)

measurement_columns <- grep(
  measurement_pattern,
  names(df_check),
  value = TRUE,
  ignore.case = TRUE
)

numeric_issues <- list()

for (nm in measurement_columns) {
  
  x <- trimws(as.character(df_check[[nm]]))
  
  nonblank <- !is.na(x) & x != ""
  
  converted <- to_num(x)
  
  bad <- nonblank & is.na(converted)
  
  if (any(bad)) {
    
    numeric_issues[[nm]] <- tibble(
      column = nm,
      nonnumeric_values = sum(bad),
      example_values = paste(
        head(unique(x[bad]), 5),
        collapse = " | "
      )
    )
  }
}

numeric_issues_df <- bind_rows(numeric_issues)

cat("\nColumns containing non-numeric values:\n")

if (nrow(numeric_issues_df) > 0) {
  print(numeric_issues_df, n = Inf)
} else {
  cat("No non-numeric values detected in selected columns.\n")
}

write_csv(
  numeric_issues_df,
  file.path(validation_folder, "numeric_consistency_review.csv")
)

# NOTE:
# Some columns may legitimately contain descriptive categories,
# such as damage severity labels.
# A non-numeric result is not automatically an error.


#==============================================================================
# STEP 7D — CHECK MISSING AND NEGATIVE MEASUREMENTS
#==============================================================================
#
# PURPOSE:
# Summarise missing values by year and identify negative numbers.
#
# Negative values are usually unexpected for stem counts,
# damage counts and pellet counts.
#
# Some other measurements may legitimately be negative,
# so all findings require interpretation.
#==============================================================================

cat("\n========== STEP 7D: MISSING AND NEGATIVE VALUES ==========\n")

missing_summary <- df_check %>%
  group_by(year) %>%
  summarise(
    rows = n(),
    across(
      all_of(measurement_columns),
      ~ sum(is.na(.x) | trimws(as.character(.x)) == ""),
      .names = "missing_{.col}"
    ),
    .groups = "drop"
  )

write_csv(
  missing_summary,
  file.path(validation_folder, "missing_measurements_by_year.csv")
)

negative_issues <- list()

for (nm in measurement_columns) {
  
  # Only check values that can be converted to numbers.
  values <- to_num(df_check[[nm]])
  
  negative <- !is.na(values) & values < 0
  
  if (any(negative)) {
    
    negative_issues[[nm]] <- tibble(
      column = nm,
      negative_values = sum(negative),
      minimum_value = min(values, na.rm = TRUE)
    )
  }
}

negative_issues_df <- bind_rows(negative_issues)

cat("\nColumns containing negative numeric values:\n")

if (nrow(negative_issues_df) > 0) {
  print(negative_issues_df, n = Inf)
} else {
  cat("No negative numeric values detected.\n")
}

write_csv(
  negative_issues_df,
  file.path(validation_folder, "negative_values_review.csv")
)


#==============================================================================
# STEP 7E — FINAL VALIDATION SUMMARY
#==============================================================================

cat("\n========== VALIDATION SUMMARY ==========\n")

cat("Total rows:", nrow(df_check), "\n")
cat("Total columns:", ncol(df_check), "\n")

cat("Duplicate plot-key groups:",
    nrow(duplicate_groups), "\n")

cat("Conflicting column pairs:",
    if (nrow(conflict_summary_df) > 0)
      sum(conflict_summary_df$conflicting_rows > 0)
    else 0,
    "\n")

cat("Columns with non-numeric values:",
    nrow(numeric_issues_df), "\n")

cat("Columns with negative values:",
    nrow(negative_issues_df), "\n")

cat("\nValidation files saved to:\n")
cat(validation_folder, "\n")

cat("\nOriginal combined dataset remains unchanged.\n")

#==============================================================================
# STEP 7F — IDENTIFY NEGATIVE MEASUREMENTS
#==============================================================================
#
# DESCRIPTION:
# Locate the original records containing negative measurements.
# Preserve all values until their meaning has been verified.
#==============================================================================

negative_columns <- c(
  "downy_height",
  "salix_total",
  "contorta_stems_>_hh",
  "Pine_Stems_<_HH"
)

for (nm in negative_columns) {
  
  values <- to_num(df_check[[nm]])
  
  problem_rows <- which(!is.na(values) & values < 0)
  
  if (length(problem_rows) > 0) {
    
    cat("\nNegative values in:", nm, "\n")
    
    result <- df_check[problem_rows, , drop = FALSE] %>%
      select(
        any_of(c(
          "year", "area", "stand", "plot",
          "plot_id", nm
        ))
      )
    
    print(as.data.frame(result))
  }
}

