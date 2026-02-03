#Packages
library(readxl)
library(read)
library(dplyr)
library(tidyr)
library(writexl)
library(tidyverse)
library(readr)
library(writexl)

# Base folder (network path)
base_path <- "//storage-ume.slu.se/home$/shge0002/My Documents/ÄBIN Data/Raw ÄBIN data 2025/ÄBIN2025rawCVS"

# -------------------------------
# First sheet - Stand information
# -------------------------------

ALL_areas_2025 <- read_csv(file.path(base_path, "ÄBIN Survey 2025_0.csv"))

stands <- ALL_areas_2025 %>%
  rename(stand_id = GlobalID)


# --------------------------------
# Second sheet - plot level info
# --------------------------------

ALL_areas_2025 <- read_csv(file.path(base_path, "ABIN_2025_wp_repeat_1.csv"))

plots <- ALL_areas_2025 %>%
  rename(
    plot_id  = GlobalID,
    stand_id = ParentGlobalID
  )


# ---------------------------------------------
# Third sheet - Pine damages (one row per tree)
# ---------------------------------------------

ALL_areas_2025 <- read_csv(file.path(base_path, "ABIN_2025_Pines_2.csv"))

pine <- ALL_areas_2025 %>%
  rename(
    plot_id   = ParentGlobalID,
    damage    = `Select damage type`,
    severity  = `how much relative damage`
  )

# Now i will format the pine sheet so it can be merged with the plot and stand sheet

pine_damage_events <- pine %>%
  group_by(plot_id, damage) %>%
  summarise(
    damage_events = n(),
    .groups = "drop"
  )

pine_damage_wide <- pine_damage_events %>%
  tidyr::pivot_wider(
    names_from = damage,
    values_from = damage_events,
    values_fill = 0
  )

# The severity is in a text state atm, so the severity runs from ≤10, 11_25, 26_50, 51_75, 76_100
pine <- pine %>%
  mutate(
    severity_num = recode(
      severity,
      "≤10" = 5,
      "11_25" = 18,
      "26_50" = 38,
      "51_75" = 63,
      "76_100" = 88,
      .default = NA_real_
    )
  )

pine_severity_sum <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_damage_events = n(),
    pine_severity_sum  = sum(severity_num, na.rm = TRUE),
    pine_severe_n      = sum(severity_num >= 2, na.rm = TRUE),
    .groups = "drop"
  )

pine_stems_sum <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_stems_ltHH   = sum(`Pine Stems < HH`, na.rm = TRUE),
    pine_browsed_ltHH = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

# Decided I actually want severity as a mean value per plot. Not just a sum
pine_severity_mean <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_severity_mean = mean(severity_num, na.rm = TRUE),
    pine_n_trees = n(),
    .groups = "drop"
  )

plots_full <- plots %>%
  left_join(pine_damage_wide, by = "plot_id") %>%
  left_join(pine_severity_sum, by = "plot_id") %>%
  left_join(pine_stems_sum, by = "plot_id")  %>%
  left_join(pine_severity_mean, by = "plot_id")


# ------------------------------------------------
# Fourth sheet - Spruce damage per tree
# ------------------------------------------------

ALL_areas_2025 <- read_csv(file.path(base_path, "ABIN_2025_Spruce_3.csv"))

spruce <- ALL_areas_2025 %>%
  rename(
    plot_id   = ParentGlobalID,
    damage    = `Select damage type`
  )

spruce_damage_events <- spruce %>%
  group_by(plot_id, damage) %>%
  summarise(
    damage_events = n(),
    .groups = "drop"
  )

spruce_damage_wide <- spruce_damage_events %>%
  tidyr::pivot_wider(
    names_from = damage,
    values_from = damage_events,
    values_fill = 0
  )

spruce_damage_wide <- spruce_damage_wide %>%
  rename_with(.fn = ~ paste0("spruce_", .), .cols = -plot_id)

spruce_damage_wide <- spruce_damage_wide %>%
  rowwise() %>%
  mutate(total_spruce = sum(c_across(-plot_id), na.rm = TRUE)) %>%
  ungroup()

plots_ALL_pine_spruce <- plots_full %>%
  left_join(spruce_damage_wide, by = "plot_id")


# ------------------------------------------------
# Fifth sheet - Contorta
# ------------------------------------------------

ALL_areas_2025 <- read_csv(file.path(base_path, "ABIN_2025_Contorta_4.csv"))

contorta <- ALL_areas_2025 %>%
  rename(
    plot_id   = ParentGlobalID,
    damage    = `Select damage type`,
    severity  = `how much relative damage`
  )

# Now i will format the contorta sheet so it can be merged with the plot and stand sheet

contorta_damage_events <- contorta %>%
  group_by(plot_id, damage) %>%
  summarise(
    damage_events = n(),
    .groups = "drop"
  )

contorta_damage_wide <- contorta_damage_events %>%
  tidyr::pivot_wider(
    names_from = damage,
    values_from = damage_events,
    values_fill = 0
  )

contorta_damage_wide <- contorta_damage_wide %>%
  rename_with(.fn = ~ paste0("contorta_", .), .cols = -plot_id)

contorta_damage_wide <- contorta_damage_wide %>%
  rowwise() %>%
  mutate(total_contorta = sum(c_across(-plot_id), na.rm = TRUE)) %>%
  ungroup()

contorta <- contorta %>%
  mutate(
    severity_num = recode(
      severity,
      "≤10" = 5,
      "11_25" = 18,
      "26_50" = 38,
      "51_75" = 63,
      "76_100" = 88,
      .default = NA_real_
    )
  )

contorta_severity_sum <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_damage_events = n(),
    contorta_severity_sum  = sum(severity_num, na.rm = TRUE),
    contorta_severe_n      = sum(severity_num >= 2, na.rm = TRUE),
    .groups = "drop"
  )

contorta_stems_sum <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_stems_ltHH   = sum(`Contorta Stems < HH`, na.rm = TRUE),
    contorta_browsed_ltHH = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

# Decided I actually want severity as a mean value per plot. Not just a sum
contorta_severity_mean <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_severity_mean = mean(severity_num, na.rm = TRUE),
    contorta_n_trees = n(),
    .groups = "drop"
  )

plots_ALL_pine_spruce_contorta <- plots_ALL_pine_spruce %>%
  left_join(contorta_damage_wide, by = "plot_id") %>%
  left_join(contorta_severity_sum, by = "plot_id") %>%
  left_join(contorta_stems_sum, by = "plot_id")  %>%
  left_join(contorta_severity_mean, by = "plot_id")


# ------------------------------------------------
# Merge the stands, plots, pine, spruce, contorta
# ------------------------------------------------

ÄBIN2025 <- plots_ALL_pine_spruce_contorta %>%
  left_join(stands, by = "stand_id")


# ------------------------------------------------
# Write output
# ------------------------------------------------

write_xlsx(
  ÄBIN2025,
  "ÄBIN2025.xlsx"
)
