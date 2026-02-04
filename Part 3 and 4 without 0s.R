###############################################################################
# Title: ÄBIN 2008–2025 pipeline (fixing 0s reappearing in damage columns)
# Author: Sarah Gore
# Date: 2026-02-03
#
# What this script does (key improvements):
# 1) Collapses duplicate damage-combination columns (e.g. "fb,o" + "o,fb")
# 2) Ensures that if ALL source columns are NA in a row, the collapsed value stays NA
#    (prevents rowSums(..., na.rm=TRUE) from creating 0s)
# 3) (Optional but recommended) keeps damage wide columns as NA not 0 when created from 2025 tree sheets
###############################################################################

# -------------------------
# Libraries
# -------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(writexl)
library(lubridate)

# ============================================================================
# PART A — 2008–2024: streamline column names (your code unchanged)
# ============================================================================

library(readxl)
X2008_2024_ÄBIN1024 <- read_excel("//storage-ume.slu.se/home$/shge0002/Desktop/ABIN files/2008-2024 ÄBIN1024.xlsx", 
                                  sheet = "Raw")


rename_damage_name <- function(nm) {
  if (grepl("Undamaged", nm, ignore.case = TRUE)) return("ub")
  nm_clean <- sub("^Pine\\s+", "", nm)
  parts <- strsplit(nm_clean, "\\s*\\+\\s*")[[1]]
  parts <- trimws(parts)
  
  codes <- vapply(parts, function(p) {
    if (p %in% c("Bark")) return("fb")
    if (p %in% c("Stem")) return("fs")
    if (p %in% c("Winter Top Shoot","WTS")) return("fts")
    if (p %in% c("Other")) return("o")
    if (p %in% c("Old")) return("od")
    if (p %in% c("Summer Top Shoot","STS")) return("ps")
    if (p %in% c("SS")) return("ss")
    if (p %in% c("Undamaged")) return("ub")
    p
  }, FUN.VALUE = character(1))
  
  codes <- sort(unique(codes))
  paste(codes, collapse = ",")
}

cn <- names(X2008_2024_ÄBIN1024)

cols_damage <- 31:58
cn[cols_damage] <- vapply(cn[cols_damage], rename_damage_name, FUN.VALUE = character(1))
cn[13] <- "ub"
names(X2008_2024_ÄBIN1024) <- cn

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
      if (p_low %in% c("old damage only", "old other damage")) return("od")
      if (p_low %in% c("old damage", "old")) return("od")
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

cn <- names(X2008_2024_ÄBIN1024)
idx <- grep("^(Spruce|Contorta)\\b", cn)
cn[idx] <- vapply(cn[idx], rename_conifer_damage, FUN.VALUE = character(1))
names(X2008_2024_ÄBIN1024) <- cn

write_xlsx(X2008_2024_ÄBIN1024, "X2008_2024_ÄBIN1024.xlsx")

# ============================================================================
# PART B — 2025: load + align + collapse duplicates WITHOUT making 0s
# ============================================================================

ÄBIN2025 <- read_excel("ÄBIN2025.xlsx")
X2008_2024_ÄBIN1024 <- read_excel("X2008_2024_ÄBIN1024.xlsx")

X2008_2024_ÄBIN1024 <- X2008_2024_ÄBIN1024 %>%
  rename(`Stand` = `Stand_number`) %>%
  rename(`Surveyor` = `Surveyer`)

# --- Area recode (your code) ---
ÄBIN2025col <- ÄBIN2025 %>%
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
  )

ÄBIN2025col <- ÄBIN2025col %>%
  rename(
    `Plot`= `Sampling plot 1-10`,
    `PCT`  = `Pre-commercially thinned?`,
    `Half Height` =`Half height (m)`,
    `North` = `x.x`,
    `East` = `y.x`,
    `Nearest Tract` = `Closest tract`,
    `Productivity` = `Site productivity`,
    `Downy Height` = `Downy Birch Highest Height (m)`,
    `Downy Total` = `Downy Birch Stems`,
    `Downy Damage` = `Downy birch damage (whole plot)`,
    `Silver Height` = `Silver Birch Highest Height (m)`,
    `Silver Total` = `Silver Birch Stems`,
    `Silver Damage` = `Silver Birch damage (whole plot)`,
    `Rowan Height` = `Rowan Highest Height (m)`,
    `Rowan Total` = `Rowan Stems`,
    `Rowan Damage` = `Rowan damage (whole plot)`,
    `Oak Height` = `Oak Highest Height (m)`,
    `Oak Total` = `Oak Stems`,
    `Oak Damage` = `Oak Damage (whole plot)`,
    `Salix Height` = `Salix Highest Height (m)`,
    `Salix Total` = `Salix Stems`,
    `Salix Damage` = `Salix Damage (whole plot)`,
    `Aspen Height` = `Aspen Highest Height (m)`,
    `Aspen Total` = `Aspen Stems`,
    `Aspen Damage` = `Aspen Damage (whole plot)`
  )

# --- Delete columns (your code, kept) ---
ÄBIN2025col <- ÄBIN2025col %>%
  select(
    -ObjectID.x, -ObjectID.y, -plot_id,
    -CreationDate.x, -CreationDate.y,
    -Creator.x, -Creator.y,
    -EditDate.x, -EditDate.y,
    -Editor.x, -Editor.y,
    -stand_id,
    -`Tallest tree 1 (m)`,
    -`Tallest tree 2 (m)`,
    -`spruce_o,ub`,
    -`spruce_ub,o`
  )

# ============================================================================
# NEW IMPROVEMENT: collapse duplicate damage columns but keep NA (not 0)
# ============================================================================

# Helper: safe row sum that returns NA if all values are NA in that row
rowSums_keepNA <- function(m) {
  m_num <- as.data.frame(lapply(m, function(x) as.numeric(as.character(x))))
  rs <- rowSums(m_num, na.rm = TRUE)
  all_na <- rowSums(!is.na(m_num)) == 0
  rs[all_na] <- NA_real_
  rs
}

df <- ÄBIN2025col

# Your original column span:
code_cols <- 35:min(197, ncol(df))

orig_names <- names(df)[code_cols]

# Normalise names by sorting tokens split by comma
norm_names <- vapply(
  strsplit(orig_names, ","),
  function(parts) {
    parts <- trimws(parts)
    parts <- sort(parts)
    paste(parts, collapse = ",")
  },
  FUN.VALUE = character(1)
)

groups <- split(code_cols, norm_names)

# Combine each group using rowSums_keepNA (THIS PREVENTS 0s)
combined_mat <- sapply(groups, function(idx) {
  m <- df[, idx, drop = FALSE]
  rowSums_keepNA(m)
})

combined_df <- as.data.frame(combined_mat)
colnames(combined_df) <- names(groups)

ÄBIN2025_combined <- cbind(
  df[, -code_cols, drop = FALSE],
  combined_df
)

# ============================================================================
# Spruce duplicates: do the same "keep NA if all NA" logic
# (Your previous special-case created 0s when both cols were NA)
# ============================================================================

if (all(c("spruce_o,od", "spruce_od,o") %in% names(ÄBIN2025_combined))) {
  m <- ÄBIN2025_combined[, c("spruce_o,od", "spruce_od,o"), drop = FALSE]
  ÄBIN2025_combined$`spruce_o,od` <- rowSums_keepNA(m)
  # Optionally drop the redundant column:
  # ÄBIN2025_combined <- ÄBIN2025_combined %>% select(-`spruce_od,o`)
}

# ============================================================================
# Contorta duplicates: re-collapse contorta_ columns safely
# ============================================================================

df2 <- ÄBIN2025_combined
cont_cols <- grep("^contorta_", names(df2), value = TRUE)

if (length(cont_cols) > 0) {
  canon_names <- vapply(cont_cols, function(nm) {
    codes <- sub("^contorta_", "", nm)
    parts <- sort(trimws(strsplit(codes, ",")[[1]]))
    paste0("contorta_", paste(parts, collapse = ","))
  }, FUN.VALUE = character(1))
  
  cont_groups <- split(cont_cols, canon_names)
  
  cont_combined <- as.data.frame(lapply(cont_groups, function(cols) {
    m <- df2[, cols, drop = FALSE]
    rowSums_keepNA(m)
  }))
  names(cont_combined) <- names(cont_groups)
  
  df2 <- cbind(df2[, setdiff(names(df2), cont_cols), drop = FALSE], cont_combined)
  ÄBIN2025_combined <- df2
}

# ============================================================================
# Add year + fix Date type + bind rows
# ============================================================================

ÄBIN2025_combined <- ÄBIN2025_combined %>%
  mutate(Year = 2025) %>%
  select(Year, everything())

X2008_2024_ÄBIN1024 <- X2008_2024_ÄBIN1024 %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))

ÄBIN2025_combined <- ÄBIN2025_combined %>%
  mutate(Date = as.Date(Date))

X2008_2025_ÄBIN1024 <- bind_rows(
  X2008_2024_ÄBIN1024,
  ÄBIN2025_combined
)

# Logical -> numeric (your step)
ÄBIN2008_2025 <- X2008_2025_ÄBIN1024
ÄBIN2008_2025[] <- lapply(ÄBIN2008_2025, function(x) {
  if (is.logical(x)) as.numeric(x) else x
})

write_xlsx(ÄBIN2008_2025, "ÄBIN2008_2025.xlsx")

# Check MA20 (your check)
df_MA20 <- ÄBIN2008_2025 %>%
  filter(Stand == "MA20")
nrow(df_MA20)
unique(df_MA20$Year)
View(df_MA20)
