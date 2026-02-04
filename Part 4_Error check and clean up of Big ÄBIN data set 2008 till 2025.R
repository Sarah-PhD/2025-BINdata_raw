###############################################################################
# Date: 2026-01-23
# Author: Sarah GOre
# Title: Error check and general df clean up. of df ÄBIN2008_2025
###############################################################################

##### IMPORT data from directory ####

ÄBIN2008_2025 <- read_excel("ÄBIN2008_2025.xlsx")

library(dplyr)
library(stringr)
library(readxl)

# ---- Load ----
ÄBIN2008_2025 <- read_excel("ÄBIN2008_2025.xlsx")

# ============================================================
# 1) Half Height: standardise to cm
# ============================================================

ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(
    Half_Height_raw = suppressWarnings(as.numeric(`Half Height`)),
    Half_Height_cm = case_when(
      is.na(Half_Height_raw) ~ NA_real_,
      Half_Height_raw < 7   ~ Half_Height_raw * 100,  # metres -> cm
      TRUE                   ~ Half_Height_raw         # already cm
    )
  )

# ============================================================
# 2) Prefix pine_ to damage-combo columns
#    + special handling of ub -> pine_unbrowsed
# ============================================================

damage_codes <- c("fts","o","od","fb","fs","ps","ss","ub")

damage_combo_regex <- paste0(
  "^(",
  paste(damage_codes, collapse = "|"),
  ")(,(",
  paste(damage_codes, collapse = "|"),
  "))*$"
)

cols_to_prefix <- names(ÄBIN2008_2025) %>%
  keep(~ str_detect(.x, damage_combo_regex)) %>%
  discard(~ str_detect(.x, "^[A-Za-z]+_")) %>%  # exclude pine_, spruce_, contorta_, etc.
  discard(~ .x == "NA")                         # safety

# ---- First: rename ub specifically ----
if ("ub" %in% cols_to_prefix) {
  ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
    rename(pine_unbrowsed = ub)
  
  cols_to_prefix <- setdiff(cols_to_prefix, "ub")
}

# ---- Then: prefix remaining damage-combo columns ----
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  rename_with(~ paste0("pine_", .x), all_of(cols_to_prefix))

# ============================================================
# 3) Optional: force pine_ damage columns to numeric
# ============================================================

ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(across(starts_with("pine_"), ~ suppressWarnings(as.numeric(.x))))

# ============================================================
# 4) Verification
# ============================================================

cat("Renamed damage columns:\n")
print(c("ub -> pine_unbrowsed", paste0(cols_to_prefix, " -> pine_", cols_to_prefix)))

summary(ÄBIN2008_2025$Half_Height_cm)

names(ÄBIN2008_2025)[str_detect(names(ÄBIN2008_2025), "^pine_")]


# ================================================================
# 5) Ordering
# =================================================================


library(dplyr)
library(stringr)

df <- ÄBIN2008_2025

# ----------------------------
# Helpers
# ----------------------------
safe_intersect <- function(x, nm) intersect(x, nm)

# Identify pine damage-combo cols (the ones with commas / codes) AFTER renaming
pine_combo_regex <- "^pine_(fts|o|od|fb|fs|ps|ss)(,(fts|o|od|fb|fs|ps|ss))*$"
spruce_combo_regex <- "^spruce_(fts|o|od|fb|fs|ps|ss)(,(fts|o|od|fb|fs|ps|ss))*$"
contorta_combo_regex <- "^contorta_(fts|o|od|fb|fs|ps|ss)(,(fts|o|od|fb|fs|ps|ss))*$"

nm <- names(df)

# ----------------------------
# 1) Define block orders
# ----------------------------

# --- Core ID / metadata block (edit if you want different “front matter”) ---
id_block <- c(
  "Area", "Year", "Surveyor", "Date", "Stand", "Plot",
  "Nearest Tract", "Productivity", "North", "East", "PCT",
  "Half Height", "Half_Height_cm", "pine_unbrowsed"
)

# --- Pine block: logical ordering ---
pine_priority <- c(
  # key status / totals / derived
  "pine_unbrowsed",
  "Pine stems", "Pine Winter Damage Stems", "Proportion Pine Damage Winter",
  "Pine Summer Damage Stems", "Proportion Pine Damage Summer",
  "Pine Yearly Damage Stems", "Proportion Pine Damage Yearly",
  "Pine damages no WTS", "Pine damages with SS",
  "Fresh_damage", "Rebrows",
  # your derived/summary columns you already have
  "pine_damage_events", "pine_severity_sum", "pine_severe_n",
  "pine_severity_mean", "pine_n_trees",
  "pine_stems_ltHH", "pine_browsed_ltHH"
)

pine_combo_cols <- nm[str_detect(nm, pine_combo_regex)]
# pine_combo_cols <- nm[str_detect(nm, pine_combo_regex)]  # (you already had this)

pine_combo_cols <- pine_combo_cols %>%
  tibble::tibble(col = .) %>%
  mutate(
    # number of codes = number of commas + 1
    n_codes = str_count(col, ",") + 1
  ) %>%
  arrange(n_codes, col) %>%   # single (1) first, then 2, 3, ...; alphabetical within each
  pull(col)

pine_other_prefix <- nm[str_detect(nm, "^pine_") & !str_detect(nm, pine_combo_regex) & nm != "pine_unbrowsed"]

# Put combo cols after pine_priority; other pine_ cols after that
pine_block <- unique(c(
  safe_intersect(id_block, nm)[length(safe_intersect(id_block, nm))], # ensures pine_unbrowsed included if in id_block
  safe_intersect(pine_priority, nm),
  pine_combo_cols,
  pine_other_prefix
))

# --- Spruce block: keep existing spruce_* order but nudge key derived totals to front if present ---
spruce_priority <- c(
  "spruce_ub", "spruce_Damaged",
  "spruce_Winter Damage", "Proportion Spruce Winter Damage",
  "spruce_> HH"
)

spruce_combo_cols <- nm[str_detect(nm, spruce_combo_regex)]
spruce_other_prefix <- nm[str_detect(nm, "^spruce_") & !str_detect(nm, spruce_combo_regex)]

spruce_block <- unique(c(
  safe_intersect(spruce_priority, nm),
  spruce_combo_cols,
  spruce_other_prefix
))

# --- Contorta block: same idea ---
contorta_priority <- c(
  "Contorta",
  "contorta_Contorta",
  "contorta_Stems > HH", "contorta_Stems < HH", "contorta_Browsed < HH",
  "total_contorta",
  "contorta_damage_events", "contorta_severity_sum", "contorta_severe_n",
  "contorta_severity_mean", "contorta_n_trees",
  "contorta_stems_ltHH", "contorta_browsed_ltHH"
)

contorta_combo_cols <- nm[str_detect(nm, contorta_combo_regex)]
contorta_other_prefix <- nm[str_detect(nm, "^contorta_") & !str_detect(nm, contorta_combo_regex)]

contorta_block <- unique(c(
  safe_intersect(contorta_priority, nm),
  contorta_combo_cols,
  contorta_other_prefix
))

# --- Broadleaves block (Downy/Silver + other species) ---
broadleaf_prefixes <- c(
  "Downy ", "Silver ", "Rowan ", "Aspen ", "Salix ", "Oak ", "Larch "
)

broadleaf_cols <- nm[
  str_detect(nm, paste0("^(", paste0(str_replace_all(broadleaf_prefixes, " ", "\\\\s"), collapse="|"), ")"))
]

# --- Wildlife / pellets block ---
wildlife_cols <- safe_intersect(c(
  "Moose Pellets", "Moose Density", "Moose Piles",
  "Red Deer Pellets", "Red Deer Piles",
  "Small Deer Pellets", "Small Deer Piles",
  "Reindeer", "Reindeer Piles",
  "Wild Boar"
), nm)

# --- Misc tail block (everything else not yet placed) ---
already <- unique(c(
  safe_intersect(id_block, nm),
  pine_block,
  spruce_block,
  contorta_block,
  broadleaf_cols,
  wildlife_cols
))

tail_block <- setdiff(nm, already)

# ----------------------------
# 2) Build final order
#    Requirement: all pine_ right after pine_unbrowsed (col 13 in your current layout)
# ----------------------------

# Keep everything up to pine_unbrowsed (whatever its current index is)
idx_pu <- which(nm == "pine_unbrowsed")
if (length(idx_pu) == 0) stop("pine_unbrowsed column not found. (Did you rename ub -> pine_unbrowsed?)")

front_block <- nm[1:idx_pu]

# Then the pine block EXCLUDING pine_unbrowsed (since it’s already in front_block)
pine_after <- setdiff(unique(c(
  safe_intersect(pine_priority, nm),
  pine_combo_cols,
  pine_other_prefix
)), "pine_unbrowsed")

# Then spruce, contorta, broadleaves, wildlife, tail
final_order <- unique(c(
  front_block,
  pine_after,
  setdiff(spruce_block, front_block),
  setdiff(contorta_block, front_block),
  setdiff(broadleaf_cols, front_block),
  setdiff(wildlife_cols, front_block),
  setdiff(tail_block, front_block)
))

# Apply
ÄBIN2008_2025 <- df %>% select(all_of(final_order))

# ----------------------------
# 3) Quick checks
# ----------------------------
cat("pine_unbrowsed position:", which(names(ÄBIN2008_2025) == "pine_unbrowsed"), "\n")
cat("First pine_ columns after pine_unbrowsed:\n")
pu_pos <- which(names(ÄBIN2008_2025) == "pine_unbrowsed")
print(names(ÄBIN2008_2025)[(pu_pos+1):(pu_pos+20)])



# Check MA20 (your check)
df_MA20 <- ÄBIN2008_2025 %>%
  filter(Stand == "MA20")
nrow(df_MA20)
unique(df_MA20$Year)
View(df_MA20)
