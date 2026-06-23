###############################################################################
# ÄBIN 2008–2025
# IMPORT ALL YEAR GROUPS FROM MASTER EXCEL FILE
#
# Workbook structure:
#   - 2008 to 2011
#   - 2012 to 2019
#   - 2021
#   - 2022
#   - 2023 to 2025
#
# Goal:
#   1. Import all sheets
#   2. Standardise column names
#   3. Harmonise variables across years
#   4. Preserve all original information
#   5. Combine into one master dataset
#   6. Apply a consistent column order
#
# IMPORTANT:
#   - Original data are never modified
#   - Missing variables are filled with NA
#   - Columns unique to a year are retained
#   - No rows are removed
###############################################################################

#==============================================================================
# LOAD PACKAGES
#==============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(writexl)
library(tidyr)

#==============================================================================
# FILE LOCATION
#==============================================================================

abin_file <-
  "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/ABIN_each separate years files in one file.xlsx"

#==============================================================================
# CHECK SHEETS
#==============================================================================

excel_sheets(abin_file)

#==============================================================================
# IMPORT SHEETS
#==============================================================================

abin_2008_2011 <- read_excel(
  abin_file,
  sheet = "2008 to 2011"
)

abin_2012_2019 <- read_excel(
  abin_file,
  sheet = "2012 to 2019"
)

abin_2021 <- read_excel(
  abin_file,
  sheet = "2021"
)

abin_2022 <- read_excel(
  abin_file,
  sheet = "2022"
)

abin_2023_2025 <- read_excel(
  abin_file,
  sheet = "2023 to 2025"
)

#==============================================================================
# QUICK CHECKS
#==============================================================================

cat("\nRows imported:\n")

cat("2008–2011 :", nrow(abin_2008_2011), "\n")
cat("2012–2019 :", nrow(abin_2012_2019), "\n")
cat("2021      :", nrow(abin_2021), "\n")
cat("2022      :", nrow(abin_2022), "\n")
cat("2023–2025 :", nrow(abin_2023_2025), "\n")

cat("\nColumns imported:\n")

cat("2008–2011 :", ncol(abin_2008_2011), "\n")
cat("2012–2019 :", ncol(abin_2012_2019), "\n")
cat("2021      :", ncol(abin_2021), "\n")
cat("2022      :", ncol(abin_2022), "\n")
cat("2023–2025 :", ncol(abin_2023_2025), "\n")

#==============================================================================
# OPTIONAL: SAVE ORIGINAL COPIES
#
# These objects are never touched again.
# Useful if you later need to compare against the source workbook.
#==============================================================================

abin_2008_2011_raw <- abin_2008_2011
abin_2012_2019_raw <- abin_2012_2019
abin_2021_raw      <- abin_2021
abin_2022_raw      <- abin_2022
abin_2023_2025_raw <- abin_2023_2025

###############################################################################
# START CLEANING FUNCTIONS BELOW THIS LINE
###############################################################################




library(dplyr)
library(stringr)
library(readr)
library(writexl)

#=============================================================================
# 1) HELPER FUNCTIONS
#=============================================================================

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
  # map must be c("old_name" = "new_name")
  for (old in names(map)) {
    df <- rename_if_present(df, old, map[[old]])
  }
  df
}

standardise_basic_names <- function(df) {
  
  names(df) <- clean_colnames_basic(names(df))
  
  basic_map <- c(
    # Core site variables
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
    "Medelhöjd" = "half_height",
    "half_height" = "half_height",
    
    "N" = "north",
    "Nord" = "north",
    "north" = "north",
    
    "E" = "east",
    "Ost" = "east",
    "east" = "east",
    
    "Nearest_Trakt" = "nearest_tract",
    "nearest_tract" = "nearest_tract",
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
# 2) YEAR-GROUP CLEANING FUNCTIONS
#=============================================================================

clean_2008_2011 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "Pine_undamaged" = "pine_unbrowsed",
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
    "PU_H" = "pine_unbrowsed_<_hh",
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
  
  df <- rename_many_if_present(df, map)
  df
}

clean_2012_2019 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "Pine_undamaged" = "pine_unbrowsed",
    "Pine_damaged" = "pine_damaged",
    "Pine_Top" = "pine_fts",
    "Pine_Stem" = "pine_fs",
    "Pine_Bark" = "pine_fb",
    
    "Downy_undamaged" = "downy_undamaged",
    "Downy_damaged" = "downy_damage",
    "Downy_Top" = "downy_winter_top_shoot",
    "Downy_Stem" = "downy_winter_stem",
    "Downy_Bark" = "downy_winter_bark",
    
    "Silver_undamaged" = "silver_undamaged",
    "Silver_damaged" = "silver_damage",
    "Silver_Top" = "silver_winter_top_shoot",
    "Silver_Stem" = "silver_winter_stem",
    "Silber_Bark" = "silver_winter_bark",
    
    "Spruce_undamaged" = "spruce_ub",
    "Spruce_damaged" = "spruce_damaged",
    "Spruce_Top" = "spruce_fts",
    "Spruce_Stem" = "spruce_fs",
    "Spruce_Bark" = "spruce_fb",
    
    "Rowan_Height" = "rowan_height",
    "Rowan_damaged" = "rowan_damage",
    "Aspen_Height" = "aspen_height",
    "Aspen_damaged" = "aspen_damage",
    "Salix_Height" = "salix_height",
    "Salix_damaged" = "salix_damage",
    "Oak_Height" = "oak_height",
    "Oak_damaged" = "oak_damage"
  )
  
  df <- rename_many_if_present(df, map)
  df
}
#=============================================================================
# UPDATED 2021 CLEANING FUNCTION
#=============================================================================

clean_2021 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "T_oskad" = "pine_unbrowsed",
    "T_topp_färsk" = "pine_fts",
    "T_topp_fjol" = "pine_fts,od",
    "T_topp_gam" = "pine_od",
    "Tallar___17" = "pine_stems",
    "Tallar...17" = "pine_stems",
    "T_brott_färsk" = "pine_fs",
    "T_brott_fjol" = "pine_fs,od",
    "T_brott_gam" = "pine_fs,od",
    "T_bark_färsk" = "pine_fb",
    "T_bark_fjol" = "pine_fb,od",
    "T_bark_gam" = "pine_fb,od",
    "Färsk_tallskada_antal" = "pine_winter_damage_stems",
    "Färsk_tallskada_%" = "proportion_pine_damage_winter",
    "Årsskada_tall" = "pine_yearly_damage_stems",
    
    "GB_oskad" = "downy_undamaged",
    "GB_topp_färsk" = "downy_winter_top_shoot",
    "GB_topp_gam" = "downy_wts_+_old",
    "Glasbjörk___30" = "downy_total",
    "Glasbjörk...30" = "downy_total",
    "GB_brott_färsk" = "downy_winter_stem",
    "GB_brott_gam" = "downy_ws_+_old",
    "GB_bark_färsk" = "downy_winter_bark",
    "GB_bark_gam" = "downy_wb_+_old",
    
    "VB_oskad" = "silver_undamaged",
    "VB_topp_färsk" = "silver_winter_top_shoot",
    "VB_topp_fjol" = "silver_wts_+_old",
    "VB_topp_gam" = "silver_old",
    "Vårtbjörk___39" = "silver_total",
    "Vårtbjörk...39" = "silver_total",
    "VB_brott_färsk" = "silver_winter_stem",
    "VB_brott_fjol" = "silver_ws_+_old",
    "VB_brott_gam" = "silver_ws_+_old",
    "VB_bark_färsk" = "silver_winter_bark",
    "VB_bark_gam" = "silver_wb_+_old",
    
    "G_oskad" = "spruce_ub",
    "G_topp_färsk" = "spruce_fts",
    "G_topp_fjol" = "spruce_fts,od",
    "G_topp_gam" = "spruce_od",
    "Gran___49" = "spruce_stems",
    "Gran...49" = "spruce_stems",
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
    "T_låg_obet" = "pine_unbrowsed_<_hh",
    
    "Antal_barrstammar" = "conifer_stems",
    "Antal_skadade_barrstammar" = "conifer_damaged_stems",
    "Andel_skadade_barrstammar" = "proportion_conifer_damaged",
    
    "Tallar___77" = "pine_stems_density_or_summary",
    "Tallar...77" = "pine_stems_density_or_summary",
    "Gran___78" = "spruce_stems_density_or_summary",
    "Gran...78" = "spruce_stems_density_or_summary",
    "Glasbjörk___79" = "downy_stems_density_or_summary",
    "Glasbjörk...79" = "downy_stems_density_or_summary",
    "Vårtbjörk___80" = "silver_stems_density_or_summary",
    "Vårtbjörk...80" = "silver_stems_density_or_summary",
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
  df
}

#=============================================================================
# UPDATED 2022 CLEANING FUNCTION
#=============================================================================

clean_2022 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  map <- c(
    "#" = "plot",
    "nearest_tract" = "nearest_tract",
    "nearest_tract" = "nearest_tract",
    "half_height" = "half_height",
    "PCT" = "pct",
    
    "yearly_damage" = "pine_yearly_damage_stems",
    "proportion_of_yearly_damage" = "proportion_pine_damage_yearly",
    
    ">50%" = "pine_>_50",
    "<50%" = "pine_<_50",
    ">50%_gran" = "spruce_>_50",
    
    "Glasbjörk_antal" = "downy_total",
    "Vårtbjörk_antal" = "silver_total",
    "Rönn_höjd" = "rowan_height",
    "Rönn_antal" = "rowan_total",
    "Asp_höjd" = "aspen_height",
    "Asp_antal" = "aspen_total",
    "Salix_höjd" = "salix_height",
    "Salix_antal" = "salix_total",
    "Ek_höjd" = "oak_height",
    "Ek_antal" = "oak_total",
    
    "Ä" = "moose_pellets_raw",
    "K" = "red_deer_pellets_raw",
    "övrig_hjort" = "small_deer_pellets_raw",
    "V" = "wild_boar"
  )
  
  df <- rename_many_if_present(df, map)
  df
}


clean_2023_2025 <- function(df) {
  
  df <- standardise_basic_names(df)
  
  # Fix the special spruce names you asked about
  spruce_map <- c(
    "o,spruce_fts" = "spruce_fts,o",
    "o,spruce_od"  = "spruce_od,o",
    "o,spruce_ub"  = "spruce_ub,o",
    "od,spruce_fs" = "spruce_fs,od",
    "od,spruce_o"  = "spruce_o,od"
  )
  
  df <- rename_many_if_present(df, spruce_map)
  df
}

#=============================================================================
# 3) CLEAN EACH DATAFRAME
#=============================================================================

abin_2008_2011_clean <- clean_2008_2011(abin_2008_2011)
abin_2012_2019_clean <- clean_2012_2019(abin_2012_2019)
abin_2021_clean      <- clean_2021(abin_2021)
abin_2022_clean      <- clean_2022(abin_2022)
abin_2023_2025_clean <- clean_2023_2025(abin_2023_2025)

#=============================================================================
# 4) CHECK THAT NO COLUMNS WERE LOST DURING RENAMING
#=============================================================================

check_lost_columns <- function(original, cleaned, label) {
  cat("\n---", label, "---\n")
  cat("Original columns:", ncol(original), "\n")
  cat("Cleaned columns:", ncol(cleaned), "\n")
  
  if (ncol(original) == ncol(cleaned)) {
    cat("No columns lost.\n")
  } else {
    cat("Column number changed. Check carefully.\n")
  }
}

check_lost_columns(abin_2008_2011, abin_2008_2011_clean, "2008–2011")
check_lost_columns(abin_2012_2019, abin_2012_2019_clean, "2012–2019")
check_lost_columns(abin_2021, abin_2021_clean, "2021")
check_lost_columns(abin_2022, abin_2022_clean, "2022")
check_lost_columns(abin_2023_2025, abin_2023_2025_clean, "2023–2025")

#=============================================================================
# ADD YEAR WHERE SHEETS DO NOT HAVE A YEAR COLUMN
#=============================================================================

abin_2008_2011_clean$year <- ifelse(
  is.na(abin_2008_2011_clean$year),
  NA,
  abin_2008_2011_clean$year
)

abin_2021_clean$year <- 2021
abin_2022_clean$year <- 2022

#=============================================================================
# MAKE ALL CLEANED DATAFRAMES SAFE TO BIND
# Handles duplicate names first, then converts all columns to character
#=============================================================================

make_bind_safe <- function(df) {
  df <- as.data.frame(df, check.names = FALSE)
  
  names(df) <- make.unique(names(df), sep = "...dup")
  
  df[] <- lapply(df, as.character)
  
  df
}

abin_2008_2011_clean <- make_bind_safe(abin_2008_2011_clean)
abin_2012_2019_clean <- make_bind_safe(abin_2012_2019_clean)
abin_2021_clean      <- make_bind_safe(abin_2021_clean)
abin_2022_clean      <- make_bind_safe(abin_2022_clean)
abin_2023_2025_clean <- make_bind_safe(abin_2023_2025_clean)

#=============================================================================
# 5) COMBINE ALL YEARS
#=============================================================================

abin_combined <- bind_rows(
  abin_2008_2011_clean,
  abin_2012_2019_clean,
  abin_2021_clean,
  abin_2022_clean,
  abin_2023_2025_clean
)


#=============================================================================
# CONVERT IMPORTANT NUMERIC COLUMNS BACK TO NUMERIC
#=============================================================================

to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

numeric_cols <- names(abin_combined)[
  grepl(
    "year|age|height|north|east|stems|unbrowsed|damaged|damage|pellets|density|proportion|pct|plot|productivity|wild_boar|reindeer",
    names(abin_combined),
    ignore.case = TRUE
  )
]

abin_combined <- abin_combined %>%
  mutate(across(any_of(numeric_cols), to_num))

#=============================================================================
# MERGE DUPLICATE COLUMNS AFTER BINDING
#=============================================================================

coalesce_duplicate_columns <- function(df) {
  
  dup_names <- unique(names(df)[duplicated(names(df))])
  
  if (length(dup_names) == 0) {
    message("No duplicate column names found.")
    return(df)
  }
  
  message("Merging duplicate columns:")
  print(dup_names)
  
  for (nm in dup_names) {
    idx <- which(names(df) == nm)
    
    merged <- dplyr::coalesce(!!!df[idx])
    
    df <- df[, -idx, drop = FALSE]
    df[[nm]] <- merged
  }
  
  df
}

abin_combined <- coalesce_duplicate_columns(abin_combined)

#=============================================================================
# 6) FINAL COLUMN ORDER
#=============================================================================

core_cols <- c(
  "area", "year", "surveyor", "date", "stand", "plot",
  "age", "pct", "half_height", "average_height_m",
  "north", "east", "nearest_tract", "productivity"
)

pine_core <- c(
  "pine_stems",
  "pine_unbrowsed",
  "pine_winter_damage_stems",
  "proportion_pine_damage_winter",
  "pine_summer_damage_stems",
  "proportion_pine_damage_summer",
  "pine_yearly_damage_stems",
  "proportion_pine_damage_yearly",
  "pine_rebrowsed_stems",
  "pine_rebrowsing_prop"
)

pine_combo <- names(abin_combined)[
  grepl("^pine_", names(abin_combined)) &
    !names(abin_combined) %in% pine_core
]

spruce_cols <- names(abin_combined)[grepl("^spruce_", names(abin_combined))]
contorta_cols <- names(abin_combined)[grepl("^contorta_", names(abin_combined))]
larch_cols <- names(abin_combined)[grepl("^larch_", names(abin_combined))]

birch_cols <- names(abin_combined)[
  grepl("^downy_|^silver_", names(abin_combined))
]

broadleaf_cols <- names(abin_combined)[
  grepl("^rowan_|^aspen_|^salix_|^oak_", names(abin_combined))
]

pellet_cols <- names(abin_combined)[
  grepl("pellets|moose_density|reindeer|wild_boar", names(abin_combined))
]

end_cols <- c("comments")

ordered_cols <- c(
  core_cols,
  pine_core,
  sort(pine_combo),
  sort(spruce_cols),
  sort(contorta_cols),
  sort(larch_cols),
  sort(birch_cols),
  sort(broadleaf_cols),
  sort(pellet_cols),
  end_cols
)

ordered_cols <- intersect(ordered_cols, names(abin_combined))
remaining_cols <- setdiff(names(abin_combined), ordered_cols)

abin_combined <- abin_combined %>%
  select(all_of(ordered_cols), all_of(remaining_cols))

#=============================================================================
# 7) FINAL CHECKS
#=============================================================================

cat("\nRows by year:\n")
print(table(abin_combined$year, useNA = "ifany"))

cat("\nOld bad spruce names still present:\n")
print(intersect(
  names(abin_combined),
  c("o,spruce_fts", "o,spruce_od", "o,spruce_ub", "od,spruce_fs", "od,spruce_o")
))

cat("\nCorrect spruce names present:\n")
print(intersect(
  names(abin_combined),
  c("spruce_fts,o", "spruce_od,o", "spruce_ub,o", "spruce_fs,od", "spruce_o,od")
))

cat("\nFirst 30 columns:\n")
print(names(abin_combined)[1:30])

#=============================================================================
# 8) SAVE
#=============================================================================

write_csv(abin_combined, "abin_2008_2025_combined.csv")
write_xlsx(abin_combined, "abin_2008_2025_combined.xlsx")


# Check rows by year
table(abin_combined$year, useNA = "ifany")

# Check final dimensions
dim(abin_combined)

# Check duplicate columns
names(abin_combined)[duplicated(names(abin_combined))]

# Check old bad spruce names
intersect(
  names(abin_combined),
  c("o,spruce_fts", "o,spruce_od", "o,spruce_ub", "od,spruce_fs", "od,spruce_o")
)

# Check corrected spruce names
intersect(
  names(abin_combined),
  c("spruce_fts,o", "spruce_od,o", "spruce_ub,o", "spruce_fs,od", "spruce_o,od")
)

# View first columns
names(abin_combined)[1:40]

