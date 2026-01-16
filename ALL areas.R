### I just wanted to see if i download all of the areas at once in survey 123
# and use the same code as "merge sheets from survey" script for the whole data set
# the this would be the fastest way

#2025 ÄBIN raw data, downloaded from the survey 123 website on the 15-01-2026
# I downloaded the data per area for clarity 
# I will start with Växjö
# Author: Sarah Louise Gore
# Date: 2025-01-16

#Packages
library(readxl)
library(dplyr)
library(tidyr)

####VÄXJÖ#####

#Import an rename each sheet 
### First sheet - Stand information ###


växjö_2025_raw_data <- read_excel("~/ÄBIN Data/Raw ÄBIN data 2025/växjö 2025 raw data.xlsx", 
                                  sheet = "ÄBIN Survey 2025")

stands <- växjö_2025_raw_data %>%
  rename(stand_id = GlobalID)



### Second sheet - plot level information ###

växjö_2025_raw_data <- read_excel("~/ÄBIN Data/Raw ÄBIN data 2025/växjö 2025 raw data.xlsx", 
                                  sheet = "ABIN_2025_wp_repeat")

plots <- växjö_2025_raw_data %>%
  rename(
    plot_id  = GlobalID,
    stand_id = ParentGlobalID
  )

### Third sheet - Pine tree damages, each row is one tree ###
### I have renamed some rows so that they can be more easily processed later

växjö_2025_raw_data <- read_excel("~/ÄBIN Data/Raw ÄBIN data 2025/växjö 2025 raw data.xlsx", 
                                  sheet = "ABIN_2025_Pines")

pine <- växjö_2025_raw_data %>%
  rename(
    plot_id   = ParentGlobalID,
    damage    = `Select damage type`,
    severity  = `how much relative damage`
  )


#### Now i will format the pine sheet so it can be merged with the plot and stand sheet ###

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


#The servity is in a text state atm, so the severity runs from ≤10, 11_25, 26_50
# 51_75, 76_100

pine <- pine %>%
  mutate(
    severity_num = recode(
      severity,
      "≤10" = 5,
      "11_25" = 18,
      "26_50" = 38,
      "51_75" = 63,
      "76_100" = 88
      
    )
  )

pine_severity_sum <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_damage_events = n(),
    pine_severity_sum  = sum(severity_num, na.rm = TRUE),
    pine_severe_n      = sum(severity_num >= 2),
    .groups = "drop"
  )

pine_stems_sum <- pine %>%
  group_by(plot_id) %>%
  summarise(
    pine_stems_ltHH   = sum(`Pine Stems < HH`, na.rm = TRUE),
    pine_browsed_ltHH = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

###DEcided I actually want severity as a mean value per plot. Not just a sum
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



#### Fourth sheet - Spruce damage per tree, each row is one tree ####

växjö_2025_raw_data <- read_excel("~/ÄBIN Data/Raw ÄBIN data 2025/växjö 2025 raw data.xlsx", 
                                  sheet = "ABIN_2025_Spruce")

spruce <- växjö_2025_raw_data %>%
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
  rowwise() %>%  # rowwise allows summing across columns
  mutate(total_spruce = sum(c_across(-plot_id), na.rm = TRUE)) %>%
  ungroup()


plots_full_pine_spruce <- plots_full %>%
  left_join(spruce_damage_wide, by = "plot_id")

#### Fith sheet - contorta - same as above ####

växjö_2025_raw_data <- read_excel("~/ÄBIN Data/Raw ÄBIN data 2025/växjö 2025 raw data.xlsx", 
                                  sheet = "ABIN_2025_Contorta")

contorta <- växjö_2025_raw_data %>%
  rename(
    plot_id   = ParentGlobalID,
    damage    = `Select damage type`,
    severity  = `how much relative damage`
  )


#### Now i will format the pine sheet so it can be merged with the plot and stand sheet ###

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


#The servity is in a text state atm, so the severity runs from ≤10, 11_25, 26_50
# 51_75, 76_100

contorta <- contorta %>%
  mutate(
    severity_num = recode(
      severity,
      "≤10" = 5,
      "11_25" = 18,
      "26_50" = 38,
      "51_75" = 63,
      "76_100" = 88
      
    )
  )

contorta_severity_sum <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_damage_events = n(),
    contorta_severity_sum  = sum(severity_num, na.rm = TRUE),
    contorta_severe_n      = sum(severity_num >= 2),
    .groups = "drop"
  )

contorta_stems_sum <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_stems_ltHH   = sum(`contorta < HH`, na.rm = TRUE),
    contorta_browsed_ltHH = sum(`Browsed < HH`, na.rm = TRUE),
    .groups = "drop"
  )

###DEcided I actually want severity as a mean value per plot. Not just a sum
contorta_severity_mean <- contorta %>%
  group_by(plot_id) %>%
  summarise(
    contorta_severity_mean = mean(severity_num, na.rm = TRUE),
    contorta_n_trees = n(),
    .groups = "drop"
  )

plots_full <- plots %>%
  left_join(contorta_damage_wide, by = "plot_id") %>%
  left_join(contorta_severity_sum, by = "plot_id") %>%
  left_join(contorta_stems_sum, by = "plot_id")  %>%
  left_join(contorta_severity_mean, by = "plot_id")

plots_full_pine_spruce_contorta <- plots_full_pine_spruce %>%
  left_join(spruce_damage_wide, by = "plot_id")

#### Merge the stands, plots, pine and spruce dfs ###


# Merge stand info into plots
Växjö plots_full_pine_spruce_contorta <- plots_full_pine_spruce %>%
  left_join(stands, by = "stand_id")


